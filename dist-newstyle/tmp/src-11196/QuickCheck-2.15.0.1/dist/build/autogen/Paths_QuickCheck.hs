{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_QuickCheck (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [2,15,0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/sspeaks/.cabal/store/ghc-9.6.6/QuickCheck-2.15.0.1-c94e2ced9f5f0dd3f594a04519b82805819b2b0379b9af74da0bd21726807fc4/bin"
libdir     = "/home/sspeaks/.cabal/store/ghc-9.6.6/QuickCheck-2.15.0.1-c94e2ced9f5f0dd3f594a04519b82805819b2b0379b9af74da0bd21726807fc4/lib"
dynlibdir  = "/home/sspeaks/.cabal/store/ghc-9.6.6/QuickCheck-2.15.0.1-c94e2ced9f5f0dd3f594a04519b82805819b2b0379b9af74da0bd21726807fc4/lib"
datadir    = "/home/sspeaks/.cabal/store/ghc-9.6.6/QuickCheck-2.15.0.1-c94e2ced9f5f0dd3f594a04519b82805819b2b0379b9af74da0bd21726807fc4/share"
libexecdir = "/home/sspeaks/.cabal/store/ghc-9.6.6/QuickCheck-2.15.0.1-c94e2ced9f5f0dd3f594a04519b82805819b2b0379b9af74da0bd21726807fc4/libexec"
sysconfdir = "/home/sspeaks/.cabal/store/ghc-9.6.6/QuickCheck-2.15.0.1-c94e2ced9f5f0dd3f594a04519b82805819b2b0379b9af74da0bd21726807fc4/etc"

getBinDir     = catchIO (getEnv "QuickCheck_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "QuickCheck_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "QuickCheck_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "QuickCheck_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "QuickCheck_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "QuickCheck_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
