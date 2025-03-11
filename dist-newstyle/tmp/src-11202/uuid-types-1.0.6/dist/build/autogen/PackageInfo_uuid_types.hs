{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_uuid_types (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "uuid_types"
version :: Version
version = Version [1,0,6] []

synopsis :: String
synopsis = "Type definitions for Universally Unique Identifiers"
copyright :: String
copyright = "(c) 2017-2018 Herbert Valerio Riedel\n(c) 2008-2014 Antoine Latter"
homepage :: String
homepage = "https://github.com/haskell-hvr/uuid"
