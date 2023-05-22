{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_proyecto_stack_vacio (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Nico\\Desktop\\2023\\PdP\\heskell\\tp02\\.stack-work\\install\\f61af325\\bin"
libdir     = "C:\\Users\\Nico\\Desktop\\2023\\PdP\\heskell\\tp02\\.stack-work\\install\\f61af325\\lib\\x86_64-windows-ghc-9.2.7\\proyecto-stack-vacio-0.1.0.0-Jci1s5j3WBCK9mDU03Owte"
dynlibdir  = "C:\\Users\\Nico\\Desktop\\2023\\PdP\\heskell\\tp02\\.stack-work\\install\\f61af325\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\Users\\Nico\\Desktop\\2023\\PdP\\heskell\\tp02\\.stack-work\\install\\f61af325\\share\\x86_64-windows-ghc-9.2.7\\proyecto-stack-vacio-0.1.0.0"
libexecdir = "C:\\Users\\Nico\\Desktop\\2023\\PdP\\heskell\\tp02\\.stack-work\\install\\f61af325\\libexec\\x86_64-windows-ghc-9.2.7\\proyecto-stack-vacio-0.1.0.0"
sysconfdir = "C:\\Users\\Nico\\Desktop\\2023\\PdP\\heskell\\tp02\\.stack-work\\install\\f61af325\\etc"

getBinDir     = catchIO (getEnv "proyecto_stack_vacio_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "proyecto_stack_vacio_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "proyecto_stack_vacio_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "proyecto_stack_vacio_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "proyecto_stack_vacio_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "proyecto_stack_vacio_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
