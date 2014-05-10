module Printing
    ( col
    , Color(..)
    , ppLine
    ) where

import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Color = Green | Yellow | Red | Blue | Magenta

col :: Color -> String -> PP.Doc
col Green txt   = PP.green $ PP.text txt
col Yellow txt  = PP.yellow $ PP.text txt
col Red txt     = PP.red $ PP.text txt
col Blue txt    = PP.blue $ PP.text txt
col Magenta txt = PP.magenta $ PP.text txt

ppLine :: Int -> PP.Doc -> IO ()
ppLine idt cont = PP.putDoc (PP.indent idt cont PP.<> PP.line)

--ppKV :: String -> PP.Doc -> IO ()
