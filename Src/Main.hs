{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription hiding (options)
import Distribution.Package
import Distribution.Compiler
import Distribution.License
import Distribution.System
import Distribution.Version
import Distribution.Text
import Text.PrettyPrint

import Control.Applicative
import Control.Exception (bracket)
import qualified Control.Exception as E
import Control.Monad

import System.Environment
import qualified Codec.Archive.Tar as Tar

import System.IO.Error
import System.Process
import System.Exit
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Tuple (swap)

import Options
import Graph

import qualified Text.PrettyPrint.ANSI.Leijen as PP

platformPackages = map PackageName $
    ["array"
    ,"base", "bytestring"
    ,"containers", "cgi"
    ,"deepseq","directory"
    ,"extensible-exceptions"
    ,"fgl","filepath"
    ,"GLUT"
    ,"haskell-src","haskell2010","haskell98","hpc","html","HUnit"
    ,"mtl"
    ,"network"
    ,"old-locale","old-time","OpenGL"
    ,"parallel","parsec","pretty","primitive","process"
    ,"QuickCheck"
    ,"random","regex-base","regex-compat","regex-posix"
    ,"split","stm","syb"
    ,"template-haskell","text","time","transformers"
    ,"unix"
    ,"vector"
    ,"xhtml"
    ,"zlib"
    ] ++ -- not stricly platform package, but act as such
    ["GHC"
    ,"Cabal"
    ,"ghc-prim"
    ]

-- FIXME use cabal's Version type.
type Ver = String
newtype AvailablePackages = AvailablePackages (M.Map PackageName [(String,L.ByteString)])

-- | return cabal 00-index.tar filepath
readCabalConfig = do
    cfgEnv <- lookup "CABAL_CONFIG" <$> getEnvironment
    cabalAppDir <- getAppUserDataDirectory "cabal"
    let cabalAppConfig = cabalAppDir </> "config"
    let configFile = fromMaybe cabalAppConfig cfgEnv

    cfg <- parseConfig . preParse <$> readFile configFile
    case (lookup "remote-repo" cfg, lookup "remote-repo-cache" cfg) of
        (Just rrepo, Just rcache) ->
            let (name,_) = fromMaybe (error "cannot parse remote-repo") $ parseOneLine rrepo in
            return (rcache </> name </> "00-index.tar")
        _ -> error ("cannot find 'remote-repo' and 'remote-repo-cache' in config file " ++ configFile)
  where
    parseConfig = catMaybes . map parseOneLine
    preParse    = filter (not . isPrefixOf " ")  -- filter all spaces leading line
                . filter (not . isPrefixOf "--") -- filter comments out
                . filter (not . null)            -- filter null line out
                . lines
    parseOneLine line = let (a,b) = break (== ':') line
                         in if null b
                                then Nothing
                                else Just (a, dropWhile (== ' ') $ drop 1 b)

unPackageName :: PackageName -> String
unPackageName (PackageName n) = n

finPkgDesc :: GenericPackageDescription -> Either [Dependency] (PackageDescription, FlagAssignment)
finPkgDesc = finalizePackageDescription [] (const True) buildPlatform (CompilerId buildCompilerFlavor (Version []{-[7, 6, 2]-} [])) []

showVerconstr c = render $ Distribution.Text.disp c

packageDescOfBS bs =
    case parsePackageDescription $ UTF8.toString bs of
         ParseFailed _ -> Nothing
         ParseOk _ a   -> Just a

getAllPackageName :: AvailablePackages -> [PackageName]
getAllPackageName (AvailablePackages apkgs) = M.keys apkgs

getPackageVersions :: AvailablePackages -> PackageName -> Maybe [Ver]
getPackageVersions (AvailablePackages apkgs) pn =
    sortVers . map fst <$> M.lookup pn apkgs

-- | sort versions, lowest first
sortVers :: [Ver] -> [Ver]
sortVers = sortBy compareVer
  where compareVer v1 v2 =
            case (break (== '.') v1, break (== '.') v2) of
                (("",_),("",_))   -> EQ
                (("",_),_)        -> LT
                (_,("",_))        -> GT
                ((i1,r1),(i2,r2)) | i1 == i2  -> compareVer (dropDot r1) (dropDot r2)
                                  | otherwise -> compare (read i1 :: Int) (read i2)
        dropDot s | s == []       = s
                  | head s == '.' = drop 1 s
                  | otherwise     = s

getPackageDescription :: AvailablePackages -> PackageName -> Maybe Ver -> Maybe GenericPackageDescription
getPackageDescription (AvailablePackages apkgs) pn mver =
    M.lookup pn apkgs >>= resolveVer mver >>= packageDescOfBS
  where resolveVer Nothing pdescs  = lookup (last $ sortVers $ map fst pdescs) pdescs
        resolveVer (Just v) pdescs = lookup v pdescs

foldallLatest :: Monad m => AvailablePackages -> a -> (a -> PackageName -> PackageDescription -> m a) -> m a
foldallLatest apkgs acc f = foldM process acc (getAllPackageName apkgs)
        where process a pn = case finPkgDesc <$> getPackageDescription apkgs pn Nothing of
                                Just (Right (pd, _)) -> f a pn pd
                                _                    -> return a

loadAvailablePackages :: IO AvailablePackages
loadAvailablePackages = do
    tarFile <- readCabalConfig

    foldl' mkMap (AvailablePackages M.empty) . listTar . Tar.read <$> L.readFile tarFile 

    where listTar :: Show e => Tar.Entries e -> [([FilePath],L.ByteString)]
          listTar (Tar.Next ent nents) =
                case Tar.entryContent ent of
                    Tar.NormalFile bs _ -> (splitPath $ Tar.entryPath ent, bs) : listTar nents
                    _                   -> listTar nents
          listTar Tar.Done             = []
          listTar (Tar.Fail err)       = error ("failed: " ++ show err)

          mkMap :: AvailablePackages -> ([FilePath], L.ByteString) -> AvailablePackages
          mkMap (AvailablePackages acc) ([(dropTrailingPathSeparator -> packagename),packageVer,_],entBS)
                | packagename == "." = AvailablePackages acc
                | otherwise          = AvailablePackages $ tweak (PackageName packagename)
                                                                 (dropTrailingPathSeparator packageVer)
                                                                 entBS acc
                          where tweak !pname !pver !cfile !m = M.alter alterF pname m
                                  where alterF Nothing  = Just [(pver,cfile)]
                                        alterF (Just z) = Just ((pver,cfile) : z)
          mkMap nacc _ = nacc

-----------------------------------------------------------------------

generateDotM boxToColor f = do
    (indexTable, depsTable) <- withGraph f
    putStrLn "digraph projects {"
    forM_ (M.toList indexTable) $ \((PackageName pn), i) -> do
        let extra = case boxToColor (PackageName pn) of
                        Nothing -> ""
                        Just c  -> ", style=filled, fillcolor=" ++ c
        putStrLn (show i ++ " [label=\"" ++ pn ++ "\"" ++ extra ++ "];")
    forM_ (M.toList depsTable) $ \(src,dsts) ->
        mapM_ (\dst -> putStrLn (show src ++ " -> " ++ show dst ++ ";")) dsts
    putStrLn "}"

unindexify :: (M.Map a GraphIndex, M.Map GraphIndex [GraphIndex]) -> [(a, [a])]
unindexify (aTable, edgeTable) = map (resolveKeyValues resolveIndex) $ M.toList edgeTable
  where resolveKeyValues r (k,l) = (r k, map r l)
        resolveIndex i = maybe (error ("internal error: unknown index: " ++ show i)) id $ M.lookup i idxTable
        idxTable = M.fromList $ map swap $ M.toList aTable

-----------------------------------------------------------------------
run apkgs hidePlatform hiddenPackages specifiedPackages = generateDotM colorize $ mapM_ (graphLoop getDeps) specifiedPackages
  where colorize pn
            | pn `elem` specifiedPackages = Just "red"
            | pn `elem` platformPackages  = Just "green"
            | otherwise                   = Nothing

        getDeps :: PackageName -> IO [PackageName]
        getDeps pn = do
            let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
            case desc of
                Just (Right (d,_)) ->
                    return
                        $ (if hidePlatform then filter (not . flip elem platformPackages) else id)
                        $ filter (not . flip elem hiddenPackages)
                        $ map (\(Dependency n _) -> n) $ buildDepends d
                _ -> do
                    --liftIO $ putStrLn ("warning cannot handle: " ++ show pn ++ " : " ++ show desc)
                    return []

-----------------------------------------------------------------------
runCmd (CmdGraph (map PackageName -> hidden) hidePlatform (map PackageName -> pkgs)) = do
    availablePackages <- loadAvailablePackages
    run availablePackages hidePlatform hidden pkgs

-----------------------------------------------------------------------
runCmd (CmdBumpable pkgs) = do
    apkgs <- loadAvailablePackages
    let getPkg n = (fmap fst . finPkgDesc) <$> getPackageDescription apkgs (PackageName n) Nothing
        bumpables = filter (not . null . snd) $ map checkBump pkgs
        checkBump pname = case getPkg pname of
            Nothing -> error ("no such package : " ++ show pname)
            Just resp -> (,) pname $ do
                Dependency (PackageName depname) verrange <- case resp of
                    Left d -> d
                    Right x -> buildDepends x
                v <- case getPkg depname of
                         Just (Right a) -> return (pkgVersion (package a))
                         _ -> mzero -- might be an error ?
                guard (not (withinRange v verrange))
                return $ PP.string depname PP.<+> PP.string "->" PP.<+> PP.cat (intersperse PP.dot (map PP.int (versionBranch v)))
    forM_ bumpables $ \(pname, desc) -> PP.putDoc (PP.string pname PP.<$> PP.indent 4 (PP.vcat desc) PP.<> PP.line)
-----------------------------------------------------------------------
runCmd (CmdDiff (PackageName -> pname) v1 v2) = runDiff
  where runDiff = do
            availablePackages <- loadAvailablePackages
            let mvers = getPackageVersions availablePackages pname
            case mvers of
                Nothing -> error ("no such package : " ++ show pname)
                Just vers -> do
                    when (not $ elem v1 vers) $ error ("package doesn't have version " ++ show v1)
                    when (not $ elem v2 vers) $ error ("package doesn't have version " ++ show v2)

                    cd <- getCurrentDirectory
                    bracket createTempDir (changeAndRemoveDirectory cd) $ \dir -> do
                        putStrLn (cd ++ " " ++ dir)
                        setCurrentDirectory dir
                        cabalUnpack pname v1
                        cabalUnpack pname v2
                        diff pname
        createTempDir = do
            tmp <- getTemporaryDirectory
            loopCreateDir (tmp ++ "/cabal-db-diff-") (0 :: Int)
          where loopCreateDir prefix i = do
                    let dir = prefix ++ show i
                    r <- E.try (createDirectory dir)
                    case r of
                        Left e | isAlreadyExistsError e -> loopCreateDir prefix (i+1)
                               | otherwise              -> E.throwIO e
                        Right () -> return dir
        changeAndRemoveDirectory cd dir =
            setCurrentDirectory cd >> removeDirectoryRecursive dir
        cabalUnpack (PackageName pn) v = do
            ec <- rawSystem "cabal" ["unpack", pn ++ "-" ++ v]
            case ec of
                ExitSuccess   -> return ()
                ExitFailure i -> error ("cabal unpack failed with error code: " ++ show i)
        diff (PackageName pn) = do
            let dir1 = pn ++ "-" ++ v1
            let dir2 = pn ++ "-" ++ v2
            _ <- rawSystem "diff" ["-Naur", dir1, dir2]
            return ()

-----------------------------------------------------------------------
runCmd (CmdRevdeps (map PackageName -> args))
    | null args = exitSuccess
    | otherwise = do
        availablePackages <- loadAvailablePackages
        founds <- foldallLatest availablePackages [] $ \a pkgname pkgDesc -> do
            let found = any (\(Dependency n _) -> n `elem` args) (buildDepends pkgDesc)
            if found
                then return ((pkgname, pkgDesc):a)
                else return a
        forM_ founds $ \(pkgname,pdesc) -> do
            let deps = filter (\(Dependency n _) -> n `elem` args) $ buildDepends pdesc
            putStrLn (unPackageName pkgname ++ ": " ++ intercalate ", " (map showDep deps))
        where showDep (Dependency p v) = unPackageName p ++ " (" ++ showVerconstr v ++ ")"

-----------------------------------------------------------------------
runCmd (CmdInfo (map PackageName -> args)) = do
    availablePackages <- loadAvailablePackages
    forM_ args $ \arg -> do
        let vers = maybe (error ("no package " ++ show arg)) id $ getPackageVersions availablePackages arg
        let pdesc = finPkgDesc <$> getPackageDescription availablePackages arg Nothing
        case pdesc of
            Just (Right (d,_)) -> do
                putStrLn (show arg)
                putStrLn ("  synopsis: " ++ synopsis d)
                putStrLn ("  versions: " ++ intercalate ", " vers)
                putStrLn ("  dependencies for " ++ (last vers) ++ ":")
                mapM_ (\(Dependency p v) -> putStrLn ("    " ++ unPackageName p ++ " (" ++ showVerconstr v ++ ")")) (buildDepends d)
            _                  -> error "cannot resolve package"

-----------------------------------------------------------------------
runCmd (CmdLicense printTree printSummary (map PackageName -> args)) = do
    availablePackages <- loadAvailablePackages
    t <- M.fromList . unindexify <$> withGraph (mapM_ (graphLoop (getDeps availablePackages)) args)
    foundLicenses <- foldM (loop availablePackages t 0) M.empty args

    when ((not printTree && not printSummary) || printSummary) $ do
        putStrLn "== license summary =="
        forM_ (map nameAndLength $ group $ sortBy licenseCmp $ map snd $ M.toList foundLicenses) $ \(licenseName, licenseNumb) -> do
            let (lstr, ppComb) = ppLicense licenseName
            PP.putDoc (ppComb (PP.text lstr) PP.<> PP.colon PP.<+> PP.text (show licenseNumb) PP.<> PP.line)

  where getDeps apkgs pn = do
            let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
            case desc of
                Just (Right (d,_)) -> do
                    let deps = map (\(Dependency n _) -> n) $ buildDepends d
                    return deps
                _ ->
                    return []
        loop apkgs tbl indentSpaces founds pn@(PackageName name)
            | M.member pn founds = return founds
            | otherwise = do
                let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
                case desc of
                    Just (Right (d,_)) -> do
                        let found = license d
                        when printTree $ do
                            let (lstr, ppComb) = ppLicense found
                            PP.putDoc $ PP.text (replicate indentSpaces ' ') PP.<> PP.text name PP.<> PP.colon PP.<+> ppComb (PP.text lstr) PP.<> PP.line
                        case M.lookup pn tbl of
                            Just l  -> foldM (loop apkgs tbl (indentSpaces + 2)) (M.insert pn found founds) l
                            Nothing -> error "internal error"
                    _ -> return founds
        licenseCmp l1 l2
            | l1 == l2    = EQ
            | otherwise   = compare (show l1) (show l2)

        nameAndLength []      = error "empty group"
        nameAndLength l@(x:_) = (x, length l)

        ppLicense (GPL (Just (Version [v] [])))    = ("GPLv" ++ show v, PP.yellow)
        ppLicense (GPL Nothing)                    = ("GPL", PP.yellow)
#if MIN_VERSION_Cabal(1,18,0)
        ppLicense (AGPL (Just (Version [v] [])))   = ("AGPLv" ++ show v, PP.yellow)
#endif
        ppLicense (LGPL (Just (Version [v] [])))   = ("LGPLv" ++ show v, PP.yellow)
        ppLicense (Apache (Just (Version [v] []))) = ("Apache" ++ show v, PP.green)
        ppLicense (UnknownLicense s)               = (s, PP.red)
        ppLicense BSD3                             = ("BSD3", PP.green)
        ppLicense BSD4                             = ("BSD4", PP.green)
        ppLicense MIT                              = ("MIT", PP.green)
        ppLicense l                                = (show l, PP.magenta)

-----------------------------------------------------------------------
runCmd (CmdSearch term vals) = do
    availablePackages <- loadAvailablePackages
    founds <- foldallLatest availablePackages [] $ \a pkgname pkgDesc -> do
        let found = any (\arg -> contains arg (accessor pkgDesc)) vals
        if found
             then return ((pkgname, pkgDesc):a)
             else return a
    mapM_ (putStrLn . unPackageName . fst) founds
  where contains searching searched = maybe False (const True) $ find (isPrefixOf searching) $ tails searched
        accessor = toAccessor term
        toAccessor SearchMaintainer = maintainer
        toAccessor SearchAuthor     = author

-----------------------------------------------------------------------
main = getOptions >>= runCmd
