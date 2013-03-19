{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription hiding (options)
import Distribution.Package
import Distribution.Compiler
import Distribution.System
import Distribution.Version
import Distribution.Text
import Text.PrettyPrint

import Control.Applicative
import Control.Monad
import Control.Monad.State

import System.Console.GetOpt
import System.Environment
import qualified Codec.Archive.Tar as Tar

import System.Process
import System.Exit
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 as UTF8

import qualified Data.Map as M
import Data.List

platformPackages = map PackageName $
    ["array"
    ,"base", "bytestring"
    ,"containers", "cgi"
    ,"deepseq","directory"
    ,"extensible-extensions"
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
    ] ++ -- not stricly platform package, but ask as such
    ["GHC"
    ,"Cabal"
    ,"ghc-prim"
    ]

-- FIXME use cabal's Version type.
type Ver = String
newtype AvailablePackages = AvailablePackages (M.Map PackageName [(String,L.ByteString)])

unPackageName :: PackageName -> String
unPackageName (PackageName n) = n

finPkgDesc :: GenericPackageDescription -> Either [Dependency] (PackageDescription, FlagAssignment)
finPkgDesc = finalizePackageDescription [] (const True) buildPlatform (CompilerId buildCompilerFlavor (Version []{-[7, 6, 2]-} [])) []

packageDescOfBS bs =
    case parsePackageDescription $ UTF8.toString bs of
         ParseFailed _ -> Nothing
         ParseOk _ a   -> Just a

getAllPackageName :: AvailablePackages -> [PackageName]
getAllPackageName (AvailablePackages apkgs) = M.keys apkgs

getPackageVersions :: AvailablePackages -> PackageName -> Maybe [Ver]
getPackageVersions (AvailablePackages apkgs) pn =
    sort . map fst <$> M.lookup pn apkgs

getPackageDescription :: AvailablePackages -> PackageName -> Maybe Ver -> Maybe GenericPackageDescription
getPackageDescription (AvailablePackages apkgs) pn mver =
    M.lookup pn apkgs >>= resolveVer mver >>= packageDescOfBS
    where resolveVer Nothing pdescs  = lookup (maximum $ map fst pdescs) pdescs
          resolveVer (Just v) pdescs = lookup v pdescs

foldallLatest :: Monad m => AvailablePackages -> a -> (a -> PackageName -> PackageDescription -> m a) -> m a
foldallLatest apkgs acc f = foldM process acc (getAllPackageName apkgs)
        where process a pn = case finPkgDesc <$> getPackageDescription apkgs pn Nothing of
                                Just (Right (pd, _)) -> f a pn pd
                                _                    -> return a

loadAvailablePackages :: IO AvailablePackages
loadAvailablePackages = do
    cabalAppDir <- getAppUserDataDirectory "cabal"
    let tarFile = cabalAppDir </> "packages" </> "hackage.haskell.org" </> "00-index.tar"
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

data St = St
    { nextIndex  :: !Int 
    , indexTable :: M.Map PackageName Int 
    , depsTable  :: M.Map Int [Int]
    } deriving (Show,Eq)

resolve pn = get >>= addOrGet
    where addOrGet st = maybe (add st) return $ M.lookup pn (indexTable st)
          add st = put (st { nextIndex = ni+1, indexTable = M.insert pn ni (indexTable st) }) >> return ni
                    where ni = nextIndex st

isProcessed pn = M.member pn <$> gets depsTable
modifyDepsTable k f = modify (\st -> st { depsTable = M.alter f k (depsTable st) })
insertDep i j = modifyDepsTable i f
    where f Nothing  = Just [j]
          f (Just z) = Just (j:z)

generateDotM boxToColor f = do
    st <- execStateT f (St 1 M.empty M.empty)
    putStrLn "digraph projects {"
    forM_ (M.toList $ indexTable st) $ \((PackageName pn), i) -> do
        let extra = case boxToColor (PackageName pn) of
                        Nothing -> ""
                        Just c  -> ", style=filled, fillcolor=" ++ c
        putStrLn (show i ++ " [label=\"" ++ pn ++ "\"" ++ extra ++ "];")
    forM_ (M.toList $ depsTable st) $ \(src,dsts) ->
        mapM_ (\dst -> putStrLn (show src ++ " -> " ++ show dst ++ ";")) dsts
    putStrLn "}"

-----------------------------------------------------------------------
run apkgs hidePlatform hiddenPackages specifiedPackages = generateDotM colorize $ mapM_ loop specifiedPackages
    where colorize pn
                 | pn `elem` specifiedPackages = Just "red"
                 | pn `elem` platformPackages  = Just "green"
                 | otherwise                   = Nothing

          loop :: PackageName -> StateT St IO ()
          loop pn = do
            pni       <- resolve pn
            processed <- isProcessed pni
            unless processed $ do
                let desc = finPkgDesc <$> getPackageDescription apkgs pn Nothing
                case desc of
                    Just (Right (d,_)) -> do
                        let depNames = (if hidePlatform
                                            then filter (not . flip elem platformPackages)
                                            else id)
                                     $ filter (not . flip elem hiddenPackages)
                                     $ map (\(Dependency n _) -> n) $ buildDepends d
                        mapM_ loop depNames
                        mapM_ (resolve >=> insertDep pni) depNames
                    _ -> return ()

-----------------------------------------------------------------------
data GraphFlag = Hide String | HidePlatform
          deriving (Show,Eq)

graphOptions =
    [ Option ['h'] ["hide"] (ReqArg Hide "hide package") "package to hide"
    , Option []    ["hide-platform"] (NoArg HidePlatform) "hide all packages from the platform"
    ]

doGraph args =
    case getOpt Permute graphOptions args of
        (o,n,[])   -> do availablePackages <- loadAvailablePackages
                         let hidden = foldl' (\acc f ->
                                                case f of
                                                    Hide p -> PackageName p : acc
                                                    _      -> acc) [] o
                         let hidePlatform = HidePlatform `elem` o
                         run availablePackages hidePlatform hidden (map PackageName n)
        (_,_,errs) -> putStrLn "error parsing arguments:" >> mapM_ putStrLn errs
-----------------------------------------------------------------------
diffOptions =
    [
    ]

doDiff args =
    case getOpt Permute diffOptions args of
        (_,n,[])   -> case n of
                            [pname,v1,v2] -> runDiff (PackageName pname) v1 v2
                            _             -> error "  diff <package-name> <ver1> <ver2>"
        (_,_,errs) -> putStrLn "error parsing arguments:" >> mapM_ putStrLn errs
    where runDiff pname v1 v2 = do
             availablePackages <- loadAvailablePackages
             let mvers = getPackageVersions availablePackages pname
             case mvers of
                Nothing -> error ("no such package : " ++ show pname)
                Just vers -> do
                    when (not $ elem v1 vers) $ error ("package doesn't have version " ++ show v1)
                    when (not $ elem v2 vers) $ error ("package doesn't have version " ++ show v2)
                    cabalUnpack pname v1
                    cabalUnpack pname v2
                    diff pname v1 v2
                    cleanup pname v1 v2
                    
          cabalUnpack (PackageName pn) v = do
              ec <- rawSystem "cabal" ["unpack", pn ++ "-" ++ v]
              case ec of
                 ExitSuccess   -> return ()
                 ExitFailure i -> error ("cabal unpack failed with error code: " ++ show i)
          diff (PackageName pn) v1 v2 = do
              let dir1 = pn ++ "-" ++ v1
              let dir2 = pn ++ "-" ++ v2
              _ <- rawSystem "diff" ["-Naur", dir1, dir2]
              return ()
          cleanup (PackageName pn) v1 v2 = do
              mapM_ removeDirectoryRecursive [pn ++ "-" ++ v1, pn ++ "-" ++ v2]

showVerconstr c = render $ Distribution.Text.disp c

-----------------------------------------------------------------------
doRevDeps (map PackageName -> args)
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
doInfo (map PackageName -> args) = do
    availablePackages <- loadAvailablePackages
    forM_ args $ \arg -> do
        let vers = maybe (error ("no package " ++ show arg)) id $ getPackageVersions availablePackages arg
        let pdesc = finPkgDesc <$> getPackageDescription availablePackages arg Nothing
        case pdesc of
            Just (Right (d,_)) -> do
                putStrLn (show arg)
                putStrLn ("  synopsis: " ++ synopsis d)
                putStrLn ("  versions: " ++ intercalate ", " vers)
                putStrLn ("  dependencies:")
                mapM_ (\(Dependency p v) -> putStrLn ("    " ++ unPackageName p ++ " (" ++ showVerconstr v ++ ")")) (buildDepends d)
            _                  -> error "cannot resolve package"

-----------------------------------------------------------------------
doSearchBy accessor args = do
    availablePackages <- loadAvailablePackages
    founds <- foldallLatest availablePackages [] $ \a pkgname pkgDesc -> do
        let found = any (\arg -> contains arg (accessor pkgDesc)) args
        if found
             then return ((pkgname, pkgDesc):a)
             else return a
    mapM_ (putStrLn . unPackageName . fst) founds
    where contains searching searched = maybe False (const True) $ find (isPrefixOf searching) $ tails searched

-----------------------------------------------------------------------
commands =
    [ ("graph", doGraph)
    , ("diff", doDiff)
    , ("revdeps", doRevDeps)
    , ("info", doInfo)
    , ("search-author", doSearchBy author)
    , ("search-maintainer", doSearchBy maintainer)
    ]

usage = do
    mapM_ putStrLn
        (["usage: cabal-db <command> [args..]"
         ,""
         ,"known commands:"
         ] ++ map (("  " ++) . fst ) commands)
    exitFailure

main = do
    args <- getArgs
    case args of
        []        -> usage
        cmd:args' -> case lookup cmd commands of
                          Nothing -> usage
                          Just f  -> f args'
