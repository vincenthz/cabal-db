module Policy
    ( Policy(..)
    , getPolicy
    , showPolicy
    ) where

import Database
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Distribution.Version

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
getPolicy _apkgs vr
    | isAnyVersion vr = Policy_Any
    | otherwise       =
        let vi = asVersionIntervals vr
         in if and $ map isBounded vi
                then Policy_StrictPVP
                else Policy_LowerBoundOnly
  where
        isBounded (_, NoUpperBound)   = False
        isBounded (_, UpperBound _ _) = True

