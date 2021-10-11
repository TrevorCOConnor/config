{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mypretty (
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/trevor/bin"
libdir     = "/home/trevor/lib/x86_64-linux-ghc-8.6.5/mypretty-0.1-IkqqihFXP4SA4kDfnfVXq7"
dynlibdir  = "/home/trevor/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/trevor/share/x86_64-linux-ghc-8.6.5/mypretty-0.1"
libexecdir = "/home/trevor/libexec/x86_64-linux-ghc-8.6.5/mypretty-0.1"
sysconfdir = "/home/trevor/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mypretty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mypretty_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mypretty_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mypretty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mypretty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mypretty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
