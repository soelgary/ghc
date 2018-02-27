{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_warp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [3,2,15] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/x86_64-linux-ghc-8.2.2/warp-3.2.15-3fYQtS4QxBtIUH149Jra7r"
dynlibdir  = "/usr/local/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/usr/local/share/x86_64-linux-ghc-8.2.2/warp-3.2.15"
libexecdir = "/usr/local/libexec/x86_64-linux-ghc-8.2.2/warp-3.2.15"
sysconfdir = "/usr/local/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "warp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "warp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "warp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "warp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "warp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "warp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
