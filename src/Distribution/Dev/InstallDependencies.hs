module Distribution.Dev.InstallDependencies (actions) where

import Distribution.Simple.Program (requireProgram, runProgram)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Simple.Utils (notice)
import System.Console.GetOpt (OptDescr (..))

import Distribution.Dev.Command (CommandActions (..), CommandResult (..))
import Distribution.Dev.Flags (Config, cfgCabalInstall, getVerbosity)
import Distribution.Dev.InitPkgDb (initPkgDb)
import Distribution.Dev.InvokeCabal (setup)
import Distribution.Dev.Sandbox (resolveSandbox)
import qualified Distribution.Dev.CabalInstall as CI

actions :: CommandActions
actions = CommandActions
              { cmdDesc = "Install the dependencies for this package"
              , cmdRun = \cfg _ args -> installDependencies cfg args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

installDependencies :: Config -> [String] -> IO CommandResult
installDependencies flgs pkgNames = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  (cabal, _) <- requireProgram v (CI.program $ cfgCabalInstall flgs) emptyProgramDb
  eFeatures <- CI.getFeatures v cabal
  setupRes <- setup s cabal flgs CI.Install
  case (setupRes, eFeatures) of
    (Left err, _) -> return $ CommandError err
    (_, Just err) -> return $ CommandError err
    (Right args, Nothing) -> do
      notice v "You are using a version of cabal-install that has \
               \the --only-dependencies flag to the install command.\
               \ Invoking that instead..."
      runProgram v cabal $ concat [ args
                                  , ["--only-dependencies"]
                                  , pkgNames
                                  ]
      return CommandOk
