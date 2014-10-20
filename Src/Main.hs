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
import Data.String

import Options
import Graph
import Env
import Ver
import Printing

import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- FIXME use cabal's Version type.
newtype AvailablePackages = AvailablePackages (M.Map PackageName [(Ver,L.ByteString)])

data Policy =
      Policy_Any            -- ^ no bounds
    | Policy_LowerBoundOnly -- ^ only a lower specified
    | Policy_Bounded        -- ^ when both bounds specified but not following the pvp as the version is not correct
    | Policy_StrictPVP      -- ^ follow the PVP, lower bound, and higher bounds not greater to one that exist.
    | Policy_Other          -- ^ unspecified
    | Policy_Many [Policy]  -- ^ multiple policy in the same file. something probably went wrong in cabal-db :)
    deriving (Show,Eq,Ord)

showPolicy :: Policy -> PP.Doc -- String
showPolicy Policy_Any            = PP.green $ PP.text "any"
showPolicy Policy_LowerBoundOnly = PP.yellow $ PP.text "lower-bound"
showPolicy Policy_StrictPVP      = PP.red $ PP.text "strict-pvp"
showPolicy Policy_Bounded        = PP.blue $ PP.text "bounded"
showPolicy Policy_Other          = PP.blue $ PP.text "other"
showPolicy (Policy_Many  _)      = PP.blue $ PP.text "many"

getPolicy :: AvailablePackages -> VersionRange -> Policy
getPolicy (AvailablePackages _apkgs) vr
    | isAnyVersion vr = Policy_Any
    | otherwise       =
        let vi = asVersionIntervals vr
         in if and $ map isBounded vi
                then Policy_StrictPVP
                else Policy_LowerBoundOnly
  where
        isBounded (_, NoUpperBound)   = False
        isBounded (_, UpperBound _ _) = True

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

dependencyName :: Dependency -> PackageName
dependencyName (Dependency n _) = n

dependencyConstraints :: Dependency -> VersionRange
dependencyConstraints (Dependency _ v) = v

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
    sort . map fst <$> M.lookup pn apkgs

getPackageDependencies :: AvailablePackages -> PackageName -> IO [Dependency]
getPackageDependencies apkgs pn = do
    let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
    case desc of
        Just (Right (d,_)) -> return $ buildDepends d
        _                  -> return []

getPackageDependencyNames apkgs pn =
    map dependencyName <$> getPackageDependencies apkgs pn

-- | sort versions, lowest first
sortVers :: [Ver] -> [Ver]
sortVers = sort

getPackageDescription :: AvailablePackages -> PackageName -> Maybe Ver -> Maybe GenericPackageDescription
getPackageDescription (AvailablePackages apkgs) pn mver =
    M.lookup pn apkgs >>= resolveVer mver >>= packageDescOfBS
  where resolveVer Nothing pdescs  = lookup (last $ sortVers $ map fst pdescs) pdescs
        resolveVer (Just v) pdescs = lookup v pdescs

getPackageLatestMajorVersion :: AvailablePackages -> PackageName -> Maybe (Int,Int)
getPackageLatestMajorVersion apkgs pn = versionMajor =<< last <$> getPackageVersions apkgs pn

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
                                                                 (fromString $ dropTrailingPathSeparator packageVer)
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
            | isPlatformPackage pn        = Just "green"
            | otherwise                   = Nothing

        getDeps :: PackageName -> IO [PackageName]
        getDeps pn = do
            let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
            case desc of
                Just (Right (d,_)) ->
                    return
                        $ (if hidePlatform then filter (not . isPlatformPackage) else id)
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
runCmd (CmdDiff (PackageName -> pname) (fromString -> v1) (fromString -> v2)) = runDiff
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
        cabalUnpack :: PackageName -> Ver -> IO ()
        cabalUnpack (PackageName pn) v = do
            ec <- rawSystem "cabal" ["unpack", pn ++ "-" ++ show v]
            case ec of
                ExitSuccess   -> return ()
                ExitFailure i -> error ("cabal unpack failed with error code: " ++ show i)
        diff (PackageName pn) = do
            let dir1 = pn ++ "-" ++ show v1
            let dir2 = pn ++ "-" ++ show v2
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
                putStrLn ("  versions: " ++ intercalate ", " (map show vers))
                putStrLn ("  dependencies for " ++ show (last vers) ++ ":")
                mapM_ (\(Dependency p v) -> putStrLn ("    " ++ unPackageName p ++ " (" ++ showVerconstr v ++ ")")) (buildDepends d)
            _                  -> error "cannot resolve package"

-----------------------------------------------------------------------
runCmd (CmdLicense printTree printSummary (map PackageName -> args)) = do
    availablePackages <- loadAvailablePackages
    t <- M.fromList . unindexify <$> withGraph (mapM_ (graphLoop (getPackageDependencyNames availablePackages)) args)
    foundLicenses <- foldM (loop availablePackages t 0) M.empty args

    when ((not printTree && not printSummary) || printSummary) $ do
        putStrLn "== license summary =="
        forM_ (map nameAndLength $ group $ sortBy licenseCmp $ map snd $ M.toList foundLicenses) $ \(licenseName, licenseNumb) -> do
            let (lstr, ppComb) = ppLicense licenseName
            ppLine 0 (ppComb lstr PP.<> PP.colon PP.<+> PP.text (show licenseNumb))
  where
        loop apkgs tbl indentSpaces founds pn@(PackageName name)
            | M.member pn founds = return founds
            | otherwise = do
                let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
                case desc of
                    Just (Right (d,_)) -> do
                        let found = license d
                        when printTree $ do
                            let (lstr, ppComb) = ppLicense found
                            ppLine 0 (PP.text (replicate indentSpaces ' ') PP.<> PP.text name PP.<> PP.colon PP.<+> ppComb lstr)
                        case M.lookup pn tbl of
                            Just l  -> foldM (loop apkgs tbl (indentSpaces + 2)) (M.insert pn found founds) l
                            Nothing -> error "internal error"
                    _ -> return founds
        licenseCmp l1 l2
            | l1 == l2    = EQ
            | otherwise   = compare (show l1) (show l2)

        nameAndLength []      = error "empty group"
        nameAndLength l@(x:_) = (x, length l)

        ppLicense (GPL (Just (Version [v] [])))    = ("GPLv" ++ show v, col Yellow)
        ppLicense (GPL Nothing)                    = ("GPL", col Yellow)
#if MIN_VERSION_Cabal(1,18,0)
        ppLicense (AGPL (Just (Version [v] [])))   = ("AGPLv" ++ show v, col Yellow)
#endif
        ppLicense (LGPL (Just (Version [v] [])))   = ("LGPLv" ++ show v, col Yellow)
#if MIN_VERSION_Cabal(1,16,0)
        ppLicense (Apache (Just (Version [v] []))) = ("Apache" ++ show v, col Green)
#endif
        ppLicense (UnknownLicense s)               = (s, col Red)
        ppLicense BSD3                             = ("BSD3", col Green)
        ppLicense BSD4                             = ("BSD4", col Green)
        ppLicense MIT                              = ("MIT", col Green)
        ppLicense l                                = (show l, col Magenta)

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

runCmd (CmdCheckRevdepsPolicy (map PackageName -> pkgs)) = do
    availablePackages <- loadAvailablePackages

    ret <- foldallLatest availablePackages M.empty $ \accOuter pkgname pkgDesc -> do
        let deps = buildDepends pkgDesc
        foldM (accOnDep availablePackages pkgname) accOuter deps
    forM_ (M.toList ret) $ \(p, z) -> do
        let sums = map (\l -> (head l, length l)) $ group $ sort $ map snd $ M.toList z
        putStrLn ("== " ++ show p)
        forM_ (M.toList z) $ \(revDep, policy) -> do
            ppLine 2 $ PP.hcat [PP.string (unPackageName revDep), PP.colon, showPolicy policy]
        forM_ sums $ \(policy, n) ->
            ppLine 0 $ PP.hcat [PP.string (show n), PP.string " packages have a constraint set to ", showPolicy policy ]
    return ()
  where accOnDep allPackages pkg a dep =
            case find (== dependencyName dep) pkgs of
                Nothing              -> return a
                Just packageMatching ->
                    let vr = dependencyConstraints dep
                     in return $ updatePolTree a packageMatching pkg (getPolicy allPackages vr)

        updatePolTree :: M.Map PackageName (M.Map PackageName Policy)
                      -> PackageName
                      -> PackageName
                      -> Policy
                      -> M.Map PackageName (M.Map PackageName Policy)
        updatePolTree a withPkg pkg pol = M.alter updateRoot withPkg a
          where updateRoot :: Maybe (M.Map PackageName Policy) -> Maybe (M.Map PackageName Policy)
                updateRoot Nothing  = Just (M.alter x pkg M.empty)
                updateRoot (Just l) = Just (M.alter x pkg l)
                x :: Maybe Policy -> Maybe Policy
                x Nothing          = Just pol
                x (Just actualPol) =
                    case actualPol of
                        Policy_Many ps | pol `elem` ps -> Just actualPol
                                       | otherwise     -> Just $ Policy_Many (pol:ps)

                        _ | actualPol == pol -> Just actualPol
                          | otherwise        -> Just $ Policy_Many (pol:[actualPol])

runCmd (CmdCheckPolicy (map PackageName -> pkgs)) = do
    availablePackages <- loadAvailablePackages
    matched <- foldallLatest availablePackages M.empty $ \acc pkgName pkgDesc ->
        if pkgName `elem` pkgs
            then return $ M.insert pkgName pkgDesc acc
            else return acc
    forM_ (M.toList matched) $ \(pkgName, pkgDesc) -> do
        putStrLn ("== " ++ unPackageName pkgName)
        let depends = map (\d -> (dependencyName d, getPolicy availablePackages $ dependencyConstraints d))
                    $ buildDepends pkgDesc
        let sums = map (\l -> (head l, length l)) $ group $ sort $ map snd depends
        forM_ depends $ \(n,p) ->
            ppLine 4 $ PP.hcat [ PP.string "* ", PP.string (unPackageName n), PP.string " = ", showPolicy p]
        forM_ sums $ \(policy, n) ->
            ppLine 2 $ PP.hcat [ showPolicy policy, PP.string " = ", PP.string (show n), PP.string " packages"]

runCmd (CmdForAll) = do
    return ()

-----------------------------------------------------------------------
main = getOptions >>= runCmd
