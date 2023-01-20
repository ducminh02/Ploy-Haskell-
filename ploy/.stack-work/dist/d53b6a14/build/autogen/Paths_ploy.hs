{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_ploy (
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
version = Version [1,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\MSI\\Documents\\TU\\Semester 3\\Softwaretechnik und Programmierparadigmen\\HA\\ploy\\.stack-work\\install\\0991aba0\\bin"
libdir     = "C:\\Users\\MSI\\Documents\\TU\\Semester 3\\Softwaretechnik und Programmierparadigmen\\HA\\ploy\\.stack-work\\install\\0991aba0\\lib\\x86_64-windows-ghc-9.0.2\\ploy-1.0.0.0-4z3rZRssoFRBBJV8TzMrbk"
dynlibdir  = "C:\\Users\\MSI\\Documents\\TU\\Semester 3\\Softwaretechnik und Programmierparadigmen\\HA\\ploy\\.stack-work\\install\\0991aba0\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\MSI\\Documents\\TU\\Semester 3\\Softwaretechnik und Programmierparadigmen\\HA\\ploy\\.stack-work\\install\\0991aba0\\share\\x86_64-windows-ghc-9.0.2\\ploy-1.0.0.0"
libexecdir = "C:\\Users\\MSI\\Documents\\TU\\Semester 3\\Softwaretechnik und Programmierparadigmen\\HA\\ploy\\.stack-work\\install\\0991aba0\\libexec\\x86_64-windows-ghc-9.0.2\\ploy-1.0.0.0"
sysconfdir = "C:\\Users\\MSI\\Documents\\TU\\Semester 3\\Softwaretechnik und Programmierparadigmen\\HA\\ploy\\.stack-work\\install\\0991aba0\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ploy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ploy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ploy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ploy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ploy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ploy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
