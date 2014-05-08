module Options
    ( Command(..)
    , SearchTerm(..)
    , getOptions
    ) where

import Options.Applicative
import Paths_cabal_db (version)
import Data.Version
import Data.List (intercalate)

data Command =
      CmdGraph
        { graphHide         :: [String]
        , graphHidePlatform :: Bool
        , graphPackages     :: [String]
        }
    | CmdDiff
        { diffPackage :: String
        , diffVer1    :: String
        , diffVer2    :: String
        }
    | CmdRevdeps
        { revdepPackages :: [String]
        }
    | CmdInfo
        { infoPackages :: [String]
        }
    | CmdSearch
        { searchTerm   :: SearchTerm
        , searchValues :: [String]
        }
    | CmdLicense
        { licensePrintTree :: Bool
        , licensePrintSummary :: Bool
        , licensePackages :: [String]
        }
    | CmdBumpable
        { bumpablePackages :: [String]
        }
    | CmdCheckPolicy
        { checkPolicyPackage :: [String]
        }
    | CmdCheckRevdepsPolicy
        { checkRevdepsPolicyPackage :: [String]
        }

data SearchTerm = SearchMaintainer | SearchAuthor

parseCArgs = subparser
    (  command "graph" (info cmdGraph (progDesc "generate a .dot dependencies graph of all the packages in argument"))
    <> command "diff" (info cmdDiff (progDesc "generate a diff between two versions of a package"))
    <> command "revdeps" (info cmdRevdeps (progDesc "list all reverse dependencies of a set of packages"))
    <> command "info" (info cmdInfo (progDesc "list some information about a set of packages"))
    <> command "search-author" (info (cmdSearch SearchAuthor) (progDesc "search the cabal database by author(s)"))
    <> command "search-maintainer" (info (cmdSearch SearchMaintainer) (progDesc "search the cabal database by maintainer(s)"))
    <> command "license" (info cmdLicense (progDesc "list all licenses of a set of packages and their dependencies"))
    <> command "bumpable" (info cmdBumpable (progDesc "list all dependencies that could receive an upper-bound version bump"))
    <> command "check-revdeps-policy" (info cmdCheckRevdepsPolicy (progDesc "check dependencies policy for reverse dependencies of a list of packages"))
    <> command "check-policy" (info cmdCheckPolicy (progDesc "check dependencies policy for packages"))
    )
  where cmdGraph = CmdGraph
                <$> many (strOption (long "hide" <> short 'h' <> metavar "PACKAGE" <> help "package to hide"))
                <*> switch (long "hide-platform" <> help "Hide all packages from the platform")
                <*> packages
        cmdDiff = CmdDiff
                <$> argument Just (metavar "<package>")
                <*> argument Just (metavar "<ver1>")
                <*> argument Just (metavar "<ver2>")
        cmdRevdeps = CmdRevdeps
                <$> packages
        cmdInfo = CmdInfo
                <$> packages
        cmdSearch accessor = CmdSearch accessor
                <$> many (argument Just (metavar "<search-term>"))
        cmdLicense = CmdLicense
                <$> switch (short 't' <> long "tree" <> help "show the tree dependencies of license")
                <*> switch (short 's' <> long "summary" <> help "Show the summary")
                <*> packages
        cmdBumpable = CmdBumpable
                <$> packages
        cmdCheckRevdepsPolicy = CmdCheckRevdepsPolicy
                <$> packages
        cmdCheckPolicy = CmdCheckPolicy
                <$> packages
        packages = some (argument Just (metavar "<packages..>"))

getOptions :: IO Command
getOptions = execParser (info (pVersion *> parseCArgs <**> helper) idm)
  where pVersion = infoOption ver (long "version" <> help "Print version information")
        ver = intercalate "." $ map show $ versionBranch version
