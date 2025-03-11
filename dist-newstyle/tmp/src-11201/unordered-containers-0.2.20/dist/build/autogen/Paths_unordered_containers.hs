{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_unordered_containers (
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
version = Version [0,2,20] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/sspeaks/.cabal/store/ghc-9.6.6/unordered-containers-0.2.20-ca82c2e4c91b5b796f69ccd561722fcfd79a69fb355daa4ea294dbcd92d0ccdc/bin"
libdir     = "/home/sspeaks/.cabal/store/ghc-9.6.6/unordered-containers-0.2.20-ca82c2e4c91b5b796f69ccd561722fcfd79a69fb355daa4ea294dbcd92d0ccdc/lib"
dynlibdir  = "/home/sspeaks/.cabal/store/ghc-9.6.6/unordered-containers-0.2.20-ca82c2e4c91b5b796f69ccd561722fcfd79a69fb355daa4ea294dbcd92d0ccdc/lib"
datadir    = "/home/sspeaks/.cabal/store/ghc-9.6.6/unordered-containers-0.2.20-ca82c2e4c91b5b796f69ccd561722fcfd79a69fb355daa4ea294dbcd92d0ccdc/share"
libexecdir = "/home/sspeaks/.cabal/store/ghc-9.6.6/unordered-containers-0.2.20-ca82c2e4c91b5b796f69ccd561722fcfd79a69fb355daa4ea294dbcd92d0ccdc/libexec"
sysconfdir = "/home/sspeaks/.cabal/store/ghc-9.6.6/unordered-containers-0.2.20-ca82c2e4c91b5b796f69ccd561722fcfd79a69fb355daa4ea294dbcd92d0ccdc/etc"

getBinDir     = catchIO (getEnv "unordered_containers_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "unordered_containers_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "unordered_containers_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "unordered_containers_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unordered_containers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unordered_containers_sysconfdir") (\_ -> return sysconfdir)



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
