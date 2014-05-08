module Ver
    ( Ver(..)
    , ver
    , versionMajor
    ) where

import Data.String
import Data.List

newtype Ver = Ver String -- [Int]
    deriving (Eq)

instance Ord Ver where
    compare (Ver v1) (Ver v2) = compareVer v1 v2

compareVer v1 v2 =
    case (break (== '.') v1, break (== '.') v2) of
        (("",_),("",_))   -> EQ
        (("",_),_)        -> LT
        (_,("",_))        -> GT
        ((i1,r1),(i2,r2)) | i1 == i2  -> compareVer (dropDot r1) (dropDot r2)
                          | otherwise -> compare (read i1 :: Int) (read i2)
  where dropDot s | s == []       = s
                  | head s == '.' = drop 1 s
                  | otherwise     = s

ver :: [Int] -> Ver
ver [] = error "empty version"
ver l  = Ver $ intercalate "." $ map show l

versionMajor :: Ver -> Maybe (Int,Int)
versionMajor (Ver l) =
    case toInts l of
        a:b:_ -> Just (a,b)
        _     -> Nothing

instance Show Ver where
    show (Ver s) = s

instance IsString Ver where
    fromString s = Ver s

toInts :: String -> [Int]
toInts verstr = loop verstr
  where loop s = case break (== '.') s of
            (v, '.':xs) -> read v : loop xs
            (v, "")     -> [read v]
            (_, _)      -> error ("cannot parse version: " ++ show verstr)
