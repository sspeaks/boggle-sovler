{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_scientific (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "scientific"
version :: Version
version = Version [0,3,8,0] []

synopsis :: String
synopsis = "Numbers represented using scientific notation"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/basvandijk/scientific"
