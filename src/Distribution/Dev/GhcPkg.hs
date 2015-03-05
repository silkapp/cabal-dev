{-|

Invoke ghc-pkg with the appropriate arguments to run in the cabal-dev
sandbox.

-}
module Distribution.Dev.GhcPkg (actions) where

import Distribution.Simple.Program (emptyProgramConfiguration, ghcPkgProgram, requireProgram, runProgram)
import System.Console.GetOpt (OptDescr)

import Distribution.Dev.Command (CommandActions (..), CommandResult (..))
import Distribution.Dev.Flags (Config, getVerbosity)
import Distribution.Dev.InitPkgDb (initPkgDb)
import Distribution.Dev.Sandbox (pkgConf, resolveSandbox)

actions :: CommandActions
actions = CommandActions
  { cmdDesc      = "Invoke ghc-pkg on the package database in"
  , cmdRun       = \cfg _ args -> invokeGhcPkg cfg args
  , cmdOpts      = [] :: [OptDescr ()]
  , cmdPassFlags = True
  }

invokeGhcPkg :: Config -> [String] -> IO CommandResult
invokeGhcPkg cfg args = do
  let v = getVerbosity cfg
  s <- initPkgDb v =<< resolveSandbox cfg
  (ghcPkg, _) <- requireProgram v ghcPkgProgram emptyProgramConfiguration
  runProgram v ghcPkg $ "--no-user-package-db" : "--global" : "--package-db" : pkgConf s : args
  return CommandOk
