{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_data_fix (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "data_fix"
version :: Version
version = Version [0,3,4] []

synopsis :: String
synopsis = "Fixpoint data types"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/spell-music/data-fix"
