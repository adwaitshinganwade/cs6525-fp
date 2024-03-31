{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_agario (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "agario"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A Haskell-based implementation of the popular game Agario (https://agar.io)."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
