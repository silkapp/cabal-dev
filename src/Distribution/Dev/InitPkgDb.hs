module Distribution.Dev.InitPkgDb (initPkgDb) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Distribution.Simple.Program (ConfiguredProgram, ghcPkgProgram, programVersion, requireProgram)
import Distribution.Simple.Program (emptyProgramConfiguration, rawSystemProgram)
import Distribution.Simple.Utils (info)
import Distribution.Text (display)
import Distribution.Verbosity (Verbosity)
import Distribution.Version (Version (..))
import System.Directory (doesDirectoryExist)

import Distribution.Dev.Sandbox (KnownVersion, Sandbox, UnknownVersion, pkgConf, setVersion)

-- |Initialize a package database.
--
-- XXX: This is GHC-only. Cabal supports other compilers, so we should, too.
--
-- XXX: If a compilation happens in a sandbox for that was used for a
-- GHC version with a different package config type, this function
-- will just fail. Ideally, we'd have different package DBs for
-- different GHC versions, but the cabal-install config file just sets
-- one location. We'd have to have the GHC version before writing the
-- cabal config file.
initPkgDb :: Verbosity -> Sandbox UnknownVersion -> IO (Sandbox KnownVersion)
initPkgDb v s = do
  let require p = requireProgram v p emptyProgramConfiguration
      run     = rawSystemProgram

  ghcPkg <- fst `fmap` require ghcPkgProgram
  let ver = ghcPackageDbType ghcPkg
      s' = setVersion s ver
      pth = pkgConf s'

  e <- doesDirectoryExist pth
  unless e $ run v ghcPkg ["init", pth]

  info v $ "Using ghc-pkg " ++ display ver

  return s'

ghcPackageDbType :: ConfiguredProgram -> Version
ghcPackageDbType = fromMaybe (error "Unknown ghc version!") . programVersion
