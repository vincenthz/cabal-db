module Env
    ( isPlatformPackage
    ) where

import Distribution.Package (PackageName(..))

platformPackages = map PackageName $
    ["array"
    ,"base", "bytestring"
    ,"containers", "cgi"
    ,"deepseq","directory"
    ,"extensible-exceptions"
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
    ] ++ -- not stricly platform package, but act as such
    ["GHC"
    ,"Cabal"
    ,"ghc-prim"
    ]

isPlatformPackage pn = pn `elem` platformPackages
