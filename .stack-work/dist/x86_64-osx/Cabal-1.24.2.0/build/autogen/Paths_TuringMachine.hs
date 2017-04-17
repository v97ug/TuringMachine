{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_TuringMachine (
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

bindir     = "/Users/v97ug/dev/haskell/TuringMachine/.stack-work/install/x86_64-osx/lts-8.11/8.0.2/bin"
libdir     = "/Users/v97ug/dev/haskell/TuringMachine/.stack-work/install/x86_64-osx/lts-8.11/8.0.2/lib/x86_64-osx-ghc-8.0.2/TuringMachine-0.1.0.0-6E5qAZ6FcsnSKyly6q6S6"
dynlibdir  = "/Users/v97ug/dev/haskell/TuringMachine/.stack-work/install/x86_64-osx/lts-8.11/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/v97ug/dev/haskell/TuringMachine/.stack-work/install/x86_64-osx/lts-8.11/8.0.2/share/x86_64-osx-ghc-8.0.2/TuringMachine-0.1.0.0"
libexecdir = "/Users/v97ug/dev/haskell/TuringMachine/.stack-work/install/x86_64-osx/lts-8.11/8.0.2/libexec"
sysconfdir = "/Users/v97ug/dev/haskell/TuringMachine/.stack-work/install/x86_64-osx/lts-8.11/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TuringMachine_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TuringMachine_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TuringMachine_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TuringMachine_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TuringMachine_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TuringMachine_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
