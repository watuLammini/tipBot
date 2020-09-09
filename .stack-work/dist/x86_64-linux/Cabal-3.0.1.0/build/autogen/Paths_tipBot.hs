{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tipBot (
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

bindir     = "/media/watulammini/daten/Uni/Medieninformatik/Master/FoFunPro/Projekt/tipBot/.stack-work/install/x86_64-linux/ba3b44df44d4d1c52b2ef315b59a5007af3bef46dc25466ca99106d251a0843b/8.8.3/bin"
libdir     = "/media/watulammini/daten/Uni/Medieninformatik/Master/FoFunPro/Projekt/tipBot/.stack-work/install/x86_64-linux/ba3b44df44d4d1c52b2ef315b59a5007af3bef46dc25466ca99106d251a0843b/8.8.3/lib/x86_64-linux-ghc-8.8.3/tipBot-0.1.0.0-82BxTVw9z7oGw7EixaOtva"
dynlibdir  = "/media/watulammini/daten/Uni/Medieninformatik/Master/FoFunPro/Projekt/tipBot/.stack-work/install/x86_64-linux/ba3b44df44d4d1c52b2ef315b59a5007af3bef46dc25466ca99106d251a0843b/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/media/watulammini/daten/Uni/Medieninformatik/Master/FoFunPro/Projekt/tipBot/.stack-work/install/x86_64-linux/ba3b44df44d4d1c52b2ef315b59a5007af3bef46dc25466ca99106d251a0843b/8.8.3/share/x86_64-linux-ghc-8.8.3/tipBot-0.1.0.0"
libexecdir = "/media/watulammini/daten/Uni/Medieninformatik/Master/FoFunPro/Projekt/tipBot/.stack-work/install/x86_64-linux/ba3b44df44d4d1c52b2ef315b59a5007af3bef46dc25466ca99106d251a0843b/8.8.3/libexec/x86_64-linux-ghc-8.8.3/tipBot-0.1.0.0"
sysconfdir = "/media/watulammini/daten/Uni/Medieninformatik/Master/FoFunPro/Projekt/tipBot/.stack-work/install/x86_64-linux/ba3b44df44d4d1c52b2ef315b59a5007af3bef46dc25466ca99106d251a0843b/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tipBot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tipBot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tipBot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tipBot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tipBot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tipBot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
