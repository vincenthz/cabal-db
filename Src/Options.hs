module Options
    ( Command(..)
    , SearchTerm(..)
    , getOptions
    ) where

import Options.Applicative
import Data.Monoid (mconcat)

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
    | CmdVCS
        { vcsPackages :: [String]
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
    | CmdForAll

data SearchTerm = SearchMaintainer | SearchAuthor

commands =
    [ ("graph", cmdGraph, "generate a .dot dependencies graph of all the packages in argument")
    , ("diff", cmdDiff, "generate a diff between two versions of a package")
    , ("revdeps", cmdRevdeps, "list all reverse dependencies of a set of packages")
    , ("info", cmdInfo, "list some information about a set of packages")
    , ("search-author", cmdSearch SearchAuthor, "search the cabal database by author(s)")
    , ("search-maintainer", cmdSearch SearchMaintainer, "search the cabal database by maintainer(s)")
    , ("license", cmdLicense, "list all licenses of a set of packages and their dependencies")
    , ("vcs", cmdVCS, "list all registered repo URLs for each a set of packages and their dependencies")
    , ("bumpable", cmdBumpable, "list all dependencies that could receive an upper-bound version bump")
    , ("check-revdeps-policy", cmdCheckRevdepsPolicy, "check dependencies policy for reverse dependencies of a list of packages")
    , ("check-policy", cmdCheckPolicy, "check dependencies policy for packages")
    , ("forall", cmdForAll, "forall packages (debug)")
    ]
  where cmdGraph = CmdGraph
                <$> many (strOption (long "hide" <> short 'h' <> metavar "PACKAGE" <> help "package to hide"))
                <*> switch (long "hide-platform" <> help "Hide all packages from the platform")
                <*> packages
        cmdDiff = CmdDiff
                <$> argument str (metavar "<package>")
                <*> argument str (metavar "<ver1>")
                <*> argument str (metavar "<ver2>")
        cmdRevdeps = CmdRevdeps
                <$> packages
        cmdInfo = CmdInfo
                <$> packages
        cmdSearch accessor = CmdSearch accessor
                <$> many (argument str (metavar "<search-term>"))
        cmdLicense = CmdLicense
                <$> switch (short 't' <> long "tree" <> help "show the tree dependencies of license")
                <*> switch (short 's' <> long "summary" <> help "Show the summary")
                <*> packages
        cmdVCS = CmdVCS
                <$> packages
        cmdBumpable = CmdBumpable
                <$> packages
        cmdCheckRevdepsPolicy = CmdCheckRevdepsPolicy
                <$> packages
        cmdCheckPolicy = CmdCheckPolicy
                <$> packages
        cmdForAll = pure CmdForAll
        packages = some (argument str (metavar "<packages..>"))

getOptions :: IO Command
getOptions = execParser (info (parseCArgs <**> helper) idm)
  where parseCArgs = subparser $ mconcat $ map (\(name, v, desc) -> command name (info v (progDesc desc))) commands
