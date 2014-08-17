module Paths_redo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/zach/Projects/redo/cabal-dev//bin"
libdir     = "/home/zach/Projects/redo/cabal-dev//lib/redo-0.0.1.0/ghc-7.4.1"
datadir    = "/home/zach/Projects/redo/cabal-dev//share/redo-0.0.1.0"
libexecdir = "/home/zach/Projects/redo/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "redo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "redo_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "redo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "redo_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
