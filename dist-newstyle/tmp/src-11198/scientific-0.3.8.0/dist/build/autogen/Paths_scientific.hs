{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_scientific (
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
version = Version [0,3,8,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/sspeaks/.cabal/store/ghc-9.6.6/scientific-0.3.8.0-213e0d5b27db9cf0e9a2f1e0e3f4e6f483f07f0cfdbb5ea29304371e09d8b857/bin"
libdir     = "/home/sspeaks/.cabal/store/ghc-9.6.6/scientific-0.3.8.0-213e0d5b27db9cf0e9a2f1e0e3f4e6f483f07f0cfdbb5ea29304371e09d8b857/lib"
dynlibdir  = "/home/sspeaks/.cabal/store/ghc-9.6.6/scientific-0.3.8.0-213e0d5b27db9cf0e9a2f1e0e3f4e6f483f07f0cfdbb5ea29304371e09d8b857/lib"
datadir    = "/home/sspeaks/.cabal/store/ghc-9.6.6/scientific-0.3.8.0-213e0d5b27db9cf0e9a2f1e0e3f4e6f483f07f0cfdbb5ea29304371e09d8b857/share"
libexecdir = "/home/sspeaks/.cabal/store/ghc-9.6.6/scientific-0.3.8.0-213e0d5b27db9cf0e9a2f1e0e3f4e6f483f07f0cfdbb5ea29304371e09d8b857/libexec"
sysconfdir = "/home/sspeaks/.cabal/store/ghc-9.6.6/scientific-0.3.8.0-213e0d5b27db9cf0e9a2f1e0e3f4e6f483f07f0cfdbb5ea29304371e09d8b857/etc"

getBinDir     = catchIO (getEnv "scientific_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "scientific_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "scientific_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "scientific_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "scientific_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "scientific_sysconfdir") (\_ -> return sysconfdir)



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
