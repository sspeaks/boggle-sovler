{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_text_short (
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
version = Version [0,1,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/sspeaks/.cabal/store/ghc-9.6.6/text-short-0.1.6-662ac4f8bb2f349ae6d506b1834638219dce4453004fb077f179cb16b3c6e2b3/bin"
libdir     = "/home/sspeaks/.cabal/store/ghc-9.6.6/text-short-0.1.6-662ac4f8bb2f349ae6d506b1834638219dce4453004fb077f179cb16b3c6e2b3/lib"
dynlibdir  = "/home/sspeaks/.cabal/store/ghc-9.6.6/text-short-0.1.6-662ac4f8bb2f349ae6d506b1834638219dce4453004fb077f179cb16b3c6e2b3/lib"
datadir    = "/home/sspeaks/.cabal/store/ghc-9.6.6/text-short-0.1.6-662ac4f8bb2f349ae6d506b1834638219dce4453004fb077f179cb16b3c6e2b3/share"
libexecdir = "/home/sspeaks/.cabal/store/ghc-9.6.6/text-short-0.1.6-662ac4f8bb2f349ae6d506b1834638219dce4453004fb077f179cb16b3c6e2b3/libexec"
sysconfdir = "/home/sspeaks/.cabal/store/ghc-9.6.6/text-short-0.1.6-662ac4f8bb2f349ae6d506b1834638219dce4453004fb077f179cb16b3c6e2b3/etc"

getBinDir     = catchIO (getEnv "text_short_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "text_short_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "text_short_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "text_short_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "text_short_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "text_short_sysconfdir") (\_ -> return sysconfdir)



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
