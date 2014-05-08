module Ver
    ( Ver(..)
    , ver
    ) where

import Data.String
import Data.List
import Numeric (readSigned, readDec)

newtype Ver = Ver [Int]
    deriving (Eq,Ord)

ver :: [Int] -> Ver
ver [] = error "empty version"
ver l  = Ver l

instance Show Ver where
    show (Ver l) = intercalate "." $ map show l

instance Read Ver where
    readsPrec _ r = case loop r of
                         [] -> []
                         l  -> [(Ver l, "")]
      where loop s =
                case break (== '.') s of
                    (v, '.':xs) -> read v : loop xs
                    (v, "")     -> [read v]
                    (v, _)      -> error ("cannot parse version: " ++ show s)

instance IsString Ver where
    fromString s =
        case readsPrec 0 s of
            []       -> error ("cannot parse version: " ++ show s)
            [(v, _)] -> v
            _        -> error ("many versions parsed: " ++ show s)
