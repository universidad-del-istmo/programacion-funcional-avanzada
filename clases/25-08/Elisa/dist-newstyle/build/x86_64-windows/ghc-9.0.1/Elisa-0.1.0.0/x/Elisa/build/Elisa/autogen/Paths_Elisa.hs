{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_Elisa (
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

bindir     = "C:\\Users\\Elisa\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Elisa\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\Elisa-0.1.0.0-inplace-Elisa"
dynlibdir  = "C:\\Users\\Elisa\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\Elisa\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\Elisa-0.1.0.0"
libexecdir = "C:\\Users\\Elisa\\AppData\\Roaming\\cabal\\Elisa-0.1.0.0-inplace-Elisa\\x86_64-windows-ghc-9.0.1\\Elisa-0.1.0.0"
sysconfdir = "C:\\Users\\Elisa\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Elisa_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Elisa_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Elisa_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Elisa_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Elisa_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Elisa_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
