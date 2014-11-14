{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Database
    ( AvailablePackages
    -- * load method
    , loadAvailablePackages
    -- * query methods
    , getPackageDescription
    , getPackageLatestMajorVersion
    , getPackageVersions
    , getAllPackageName
    ) where

import Control.Applicative

import Distribution.PackageDescription.Parse
import Distribution.PackageDescription hiding (options)
import Distribution.Package

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.String

import System.Environment
import System.Directory
import System.FilePath

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Ver

-- FIXME use cabal's Version type.
newtype AvailablePackages = AvailablePackages (M.Map PackageName [(Ver,L.ByteString)])

-- | Load the available packages from the tarball then append
-- an extra list of packages that are directly refered by filepath to their .cabal
loadAvailablePackages :: [String] -> IO AvailablePackages
loadAvailablePackages extras = do
    tarFile <- readCabalConfig

    m1 <- foldr mkMap (AvailablePackages M.empty) . listTar . Tar.read <$> L.readFile tarFile
    foldr mkMap m1 <$> mapM (\f -> L.readFile f >>= \l -> return (cabalFileToPkgName f, l)) extras

    where listTar :: Show e => Tar.Entries e -> [([FilePath],L.ByteString)]
          listTar (Tar.Next ent nents) =
                case Tar.entryContent ent of
                    Tar.NormalFile bs _ -> (splitPath $ Tar.entryPath ent, bs) : listTar nents
                    _                   -> listTar nents
          listTar Tar.Done             = []
          listTar (Tar.Fail err)       = error ("failed: " ++ show err)

          mkMap :: ([FilePath], L.ByteString) -> AvailablePackages -> AvailablePackages
          mkMap ([(dropTrailingPathSeparator -> packagename),packageVer,_],entBS) (AvailablePackages acc)
                | packagename == "." = AvailablePackages acc
                | otherwise          = AvailablePackages $ tweak (PackageName packagename)
                                                                 (fromString $ dropTrailingPathSeparator packageVer)
                                                                 entBS acc
                          where tweak !pname !pver !cfile !m = M.alter alterF pname m
                                  where alterF Nothing  = Just [(pver,cfile)]
                                        alterF (Just z) = Just ((pver,cfile) : z)
          mkMap _ nacc = nacc

cabalFileToPkgName f = [takeBaseName f, "10000", ""]

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

getPackageDescription :: AvailablePackages -> PackageName -> Maybe Ver -> Maybe GenericPackageDescription
getPackageDescription (AvailablePackages apkgs) pn mver =
    M.lookup pn apkgs >>= resolveVer mver >>= packageDescOfBS
  where resolveVer Nothing pdescs  = lookup (last $ sortVers $ map fst pdescs) pdescs
        resolveVer (Just v) pdescs = lookup v pdescs

        packageDescOfBS bs =
            case parsePackageDescription $ UTF8.toString bs of
                 ParseFailed _ -> Nothing
                 ParseOk _ a   -> Just a

        -- | sort versions, lowest first
        sortVers :: [Ver] -> [Ver]
        sortVers = sort

getPackageLatestMajorVersion :: AvailablePackages -> PackageName -> Maybe (Int,Int)
getPackageLatestMajorVersion apkgs pn = versionMajor =<< last <$> getPackageVersions apkgs pn

getPackageVersions :: AvailablePackages -> PackageName -> Maybe [Ver]
getPackageVersions (AvailablePackages apkgs) pn =
    sort . map fst <$> M.lookup pn apkgs

getAllPackageName :: AvailablePackages -> [PackageName]
getAllPackageName (AvailablePackages apkgs) = M.keys apkgs
