{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
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

import System.IO.Error
import System.Process
import System.Exit
import System.Directory

import qualified Data.Map as M
import Data.Either
import Data.List
import Data.Maybe (catMaybes)
import Data.String
import Data.Tuple (swap)

import Options
import Graph
import Env
import Ver
import Printing
import Policy
import Database

import qualified Text.PrettyPrint.ANSI.Leijen as PP

#if !MIN_VERSION_Cabal(1,22,0)
unPackageName :: PackageName -> String
unPackageName (PackageName n) = n
#endif

dependencyName :: Dependency -> PackageName
dependencyName (Dependency n _) = n

dependencyConstraints :: Dependency -> VersionRange
dependencyConstraints (Dependency _ v) = v

#if MIN_VERSION_Cabal(1,22,0)
finPkgDesc :: GenericPackageDescription -> Either [Dependency] (PackageDescription, FlagAssignment)
finPkgDesc = finalizePackageDescription [] (const True) buildPlatform compilerInfo []
  where compilerInfo = unknownCompilerInfo (CompilerId buildCompilerFlavor (Version []{-[7, 6, 2]-} [])) NoAbiTag
#else
finPkgDesc :: GenericPackageDescription -> Either [Dependency] (PackageDescription, FlagAssignment)
finPkgDesc = finalizePackageDescription [] (const True) buildPlatform (CompilerId buildCompilerFlavor (Version []{-[7, 6, 2]-} [])) []
#endif

showVerconstr c = render $ Distribution.Text.disp c

getPackageDependencies :: AvailablePackages -> PackageName -> IO [Dependency]
getPackageDependencies apkgs pn = do
    let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
    case desc of
        Just (Right (d,_)) -> return $ buildDepends d
        _                  -> return []

getPackageDependencyNames apkgs pn =
    map dependencyName <$> getPackageDependencies apkgs pn

-- | partition a list of arguments that refer to package
-- either by their package name, or directly to a cabal file
packageArgs :: [String] -> IO ([PackageName], [FilePath])
packageArgs args = partitionEithers <$> mapM classifyArgs args
  where classifyArgs :: String -> IO (Either PackageName FilePath)
        classifyArgs arg = do
            existingFile <- doesFileExist arg
            return $ if existingFile && hasCabalExtension
                then Right arg
                else Left $ PackageName arg
          where hasCabalExtension = ".cabal" `isSuffixOf` arg

foldallLatest :: Monad m => AvailablePackages -> a -> (a -> PackageName -> PackageDescription -> m a) -> m a
foldallLatest apkgs acc f = foldM process acc (getAllPackageName apkgs)
        where process a pn = case finPkgDesc <$> getPackageDescription apkgs pn Nothing of
                                Just (Right (pd, _)) -> f a pn pd
                                _                    -> return a

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

vcs :: PackageDescription -> [(String,String)]
vcs = catMaybes . fmap f . sourceRepos
  where f r = do loc <- repoLocation r
                 let rev = case (repoTag r, repoBranch r) of
                       (Just a, _)        -> a
                       (Nothing, Just b)  -> b
                       (Nothing, Nothing) -> "master"
                 return (loc,rev)

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
runCmd (CmdGraph (map PackageName -> hidden) hidePlatform rawArgs) = do
    (pkgNames, pkgFileNames) <- packageArgs rawArgs
    when (pkgNames == []) $ error "graph: no package(s) specified"
    availablePackages        <- loadAvailablePackages pkgFileNames
    run availablePackages hidePlatform hidden pkgNames

-----------------------------------------------------------------------
runCmd (CmdBumpable pkgs) = do
    apkgs <- loadAvailablePackages []
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
            availablePackages <- loadAvailablePackages []
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
runCmd (CmdDeps (PackageName -> pname) (fromString -> v1)) = do
        (_pkgNames, pkgFileNames) <- packageArgs []
        availablePackages <- loadAvailablePackages pkgFileNames
        let mvers = getPackageVersions availablePackages pname
        case mvers of
          Nothing -> error ("no such package : " ++ show pname)
          Just vers -> do
                     when (not $ elem v1 vers) $ error ("package doesn't have version " ++ show v1)
                     let pdesc = finPkgDesc <$> getPackageDescription availablePackages pname (Just v1)
                     case pdesc of
                       Just (Right (d,_)) ->
                           mapM_ (putStrLn . showDep) (buildDepends d)
                       _                  -> error "cannot resolve package"
        where showDep (Dependency p v) = unPackageName p ++ " (" ++ showVerconstr v ++ ")"

-----------------------------------------------------------------------
runCmd (CmdRevdeps rawArgs)
    | null rawArgs = exitSuccess
    | otherwise = do
        (pkgNames, pkgFileNames) <- packageArgs rawArgs
        availablePackages <- loadAvailablePackages pkgFileNames
    
        founds <- foldallLatest availablePackages [] $ \a pkgname pkgDesc -> do
            let found = any (\(Dependency n _) -> n `elem` pkgNames) (buildDepends pkgDesc)
            if found
                then return ((pkgname, pkgDesc):a)
                else return a
        forM_ founds $ \(pkgname,pdesc) -> do
            let deps = filter (\(Dependency n _) -> n `elem` pkgNames) $ buildDepends pdesc
            putStrLn (unPackageName pkgname ++ ": " ++ intercalate ", " (map showDep deps))
        where showDep (Dependency p v) = unPackageName p ++ " (" ++ showVerconstr v ++ ")"

-----------------------------------------------------------------------
runCmd (CmdInfo (map PackageName -> args)) = do
    availablePackages <- loadAvailablePackages []
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
runCmd (CmdLicense printTree printSummary rawArgs) = do
    (pkgNames, pkgFileNames) <- packageArgs rawArgs

    availablePackages <- loadAvailablePackages pkgFileNames

    t <- M.fromList . unindexify <$> withGraph (mapM_ (graphLoop (getPackageDependencyNames availablePackages)) pkgNames)
    foundLicenses <- foldM (loop availablePackages t 0) M.empty pkgNames

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
runCmd (CmdVCS rawArgs) = do
    (pkgNames, pkgFileNames) <- packageArgs rawArgs

    availablePackages <- loadAvailablePackages pkgFileNames

    let depNames = getPackageDependencyNames availablePackages
        mkGraph = mapM_ (graphLoop depNames) pkgNames
    t <- M.fromList . unindexify <$> withGraph mkGraph
    void $ foldM (loop availablePackages t 0) M.empty pkgNames
  where
        showVCS (url,rev) = col Yellow $ concat [url, "@",  rev]
        loop apkgs tbl indentSpaces founds pn@(PackageName name)
            | M.member pn founds = return founds
            | otherwise = do
                let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
                case desc of
                    Just (Right (d,_)) -> do
                        let found = vcs d
                            uriTexts = showVCS <$> found
                        ppLine indentSpaces $ PP.text name PP.<> PP.colon
                        case uriTexts of
                          [] -> ppLine (indentSpaces+2) $ col Red "No associated repo"
                          _ -> mapM_ (ppLine(indentSpaces+2)) uriTexts
                        case M.lookup pn tbl of
                            Just l  -> foldM (loop apkgs tbl (indentSpaces + 2)) (M.insert pn found founds) l
                            Nothing -> error "internal error"
                    _ -> return founds

-----------------------------------------------------------------------
runCmd (CmdSearch term vals) = do
    availablePackages <- loadAvailablePackages []
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

runCmd (CmdCheckRevdepsPolicy rawArgs) = do
    (pkgNames, pkgFileNames) <- packageArgs rawArgs
    availablePackages <- loadAvailablePackages pkgFileNames

    ret <- foldallLatest availablePackages M.empty $ \accOuter pkgname pkgDesc -> do
        let deps = buildDepends pkgDesc
        foldM (accOnDep pkgNames availablePackages pkgname) accOuter deps
    forM_ (M.toList ret) $ \(p, z) -> do
        let sums = map (\l -> (head l, length l)) $ group $ sort $ map snd $ M.toList z
        putStrLn ("== " ++ show p)
        forM_ (M.toList z) $ \(revDep, policy) -> do
            ppLine 2 $ PP.hcat [PP.string (unPackageName revDep), PP.colon, showPolicy policy]
        forM_ sums $ \(policy, n) ->
            ppLine 0 $ PP.hcat [PP.string (show n), PP.string " packages have a constraint set to ", showPolicy policy ]
    return ()
  where accOnDep pkgNames allPackages pkg a dep =
            case find (== dependencyName dep) pkgNames of
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

runCmd (CmdCheckPolicy rawArgs) = do
    (pkgNames, pkgFileNames) <- packageArgs rawArgs

    availablePackages <- loadAvailablePackages pkgFileNames

    matched <- foldallLatest availablePackages M.empty $ \acc name pkgDesc ->
        if name `elem` pkgNames
            then return $ M.insert name pkgDesc acc
            else return acc
    forM_ (M.toList matched) $ \(name, pkgDesc) -> do
        putStrLn ("== " ++ unPackageName name)
        let packageDepends = map (\d -> (dependencyName d, getPolicy availablePackages $ dependencyConstraints d))
                           $ buildDepends pkgDesc
        let sums = map (\l -> (head l, length l)) $ group $ sort $ map snd packageDepends

        forM_ packageDepends $ \(n,p) ->
            ppLine 4 $ PP.hcat [ PP.string "* ", PP.string (unPackageName n), PP.string " = ", showPolicy p]
        forM_ sums $ \(policy, n) ->
            ppLine 2 $ PP.hcat [ showPolicy policy, PP.string " = ", PP.string (show n), PP.string " packages"]

runCmd (CmdForAll) = do
    return ()

-----------------------------------------------------------------------
main = getOptions >>= runCmd
