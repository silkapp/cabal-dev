{-# LANGUAGE
    EmptyDataDecls
  , GADTs
  #-}
module Distribution.Dev.Sandbox
  ( KnownVersion
  , Sandbox
  , UnknownVersion
  , cabalConf
  , indexCache
  , indexCacheBase
  , indexTar
  , indexTarBase
  , localRepoPath
  , newSandbox
  , pkgConf
  , resolveSandbox
  , sandbox
  , setVersion
  ) where

import Control.Monad (unless)
import Data.Version (Version, showVersion)
import Distribution.Simple.Utils (debug)
import Distribution.Verbosity (Verbosity)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Distribution.Dev.Utilities (ensureAbsolute)
import Paths_cabal_dev (getDataFileName)
import qualified Distribution.Dev.Flags as F (Config, getSandbox, getVerbosity, sandboxSpecified)

-- A sandbox directory that we may or may not know what kind of
-- package format it uses
data UnknownVersion
data KnownVersion

data Sandbox a where
  UnknownVersion :: FilePath -> Sandbox UnknownVersion
  KnownVersion :: FilePath -> Version -> Sandbox KnownVersion

setVersion :: Sandbox UnknownVersion -> Version -> Sandbox KnownVersion
setVersion (UnknownVersion p) v = KnownVersion p v

sandbox :: Sandbox a -> FilePath
sandbox (UnknownVersion p) = p
sandbox (KnownVersion p _) = p

sPath :: FilePath -> Sandbox a -> FilePath
sPath p s = sandbox s </> p

localRepoPath :: Sandbox a -> FilePath
localRepoPath = sPath "packages"

pkgConf :: Sandbox KnownVersion -> FilePath
pkgConf s@(KnownVersion _ v) = sPath packageDbName s
    where
      packageDbName = "packages-" ++ showVersion v ++ ".conf"

cabalConf :: Sandbox a -> FilePath
cabalConf = sPath "cabal.config"

newSandbox :: Verbosity -> FilePath -> IO (Sandbox UnknownVersion)
newSandbox v relSandboxDir = do
  debug v $ "Using " ++ relSandboxDir ++ " as the relative cabal-dev sandbox"
  sandboxDir <- ensureAbsolute relSandboxDir
  debug v $ "Using " ++ sandboxDir ++ " as the cabal-dev sandbox"
  createDirectoryIfMissing True sandboxDir
  let sb = UnknownVersion sandboxDir
  debug v $ "Creating local repo " ++ localRepoPath sb
  createDirectoryIfMissing True $ localRepoPath sb
  extant <- doesFileExist (indexTar sb)
  unless extant $ do
    emptyIdxFile <- getDataFileName $ "admin" </> indexTarBase
    copyFile emptyIdxFile (indexTar sb)
  return sb

resolveSandbox :: F.Config -> IO (Sandbox UnknownVersion)
resolveSandbox cfg = do
  unless (F.sandboxSpecified cfg) $
    debug v $ "No sandbox specified. Using " ++ relSandbox
  newSandbox v relSandbox
  where
   v          = F.getVerbosity cfg
   relSandbox = F.getSandbox cfg

-- |The name of the cabal-install package index
indexCacheBase :: FilePath
indexCacheBase = "00-index.cache"

indexCache :: Sandbox a -> FilePath
indexCache sb = localRepoPath sb </> indexCacheBase

indexTarBase :: FilePath
indexTarBase = "00-index.tar"

indexTar :: Sandbox a -> FilePath
indexTar sb = localRepoPath sb </> indexTarBase
