{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hblas (
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
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mamtajakter/.cabal/bin"
libdir     = "/Users/mamtajakter/.cabal/lib/x86_64-osx-ghc-8.6.4/hblas-0.0.0.0-inplace-test_level1"
dynlibdir  = "/Users/mamtajakter/.cabal/lib/x86_64-osx-ghc-8.6.4"
datadir    = "/Users/mamtajakter/.cabal/share/x86_64-osx-ghc-8.6.4/hblas-0.0.0.0"
libexecdir = "/Users/mamtajakter/.cabal/libexec/x86_64-osx-ghc-8.6.4/hblas-0.0.0.0"
sysconfdir = "/Users/mamtajakter/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hblas_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hblas_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hblas_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hblas_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hblas_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hblas_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
