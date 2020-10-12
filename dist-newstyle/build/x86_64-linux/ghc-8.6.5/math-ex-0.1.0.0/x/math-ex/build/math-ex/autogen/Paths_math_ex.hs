{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_math_ex (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/tomasz/.cabal/bin"
libdir     = "/home/tomasz/.cabal/lib/x86_64-linux-ghc-8.6.5/math-ex-0.1.0.0-inplace-math-ex"
dynlibdir  = "/home/tomasz/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/tomasz/.cabal/share/x86_64-linux-ghc-8.6.5/math-ex-0.1.0.0"
libexecdir = "/home/tomasz/.cabal/libexec/x86_64-linux-ghc-8.6.5/math-ex-0.1.0.0"
sysconfdir = "/home/tomasz/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "math_ex_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "math_ex_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "math_ex_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "math_ex_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "math_ex_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "math_ex_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
