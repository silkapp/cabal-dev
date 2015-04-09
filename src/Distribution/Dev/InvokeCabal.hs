{-# LANGUAGE CPP #-}
module Distribution.Dev.InvokeCabal
  ( actions
  , setup
  , cabalArgs
  ) where

import Control.Applicative
import Distribution.ParseUtils (Field)
import Distribution.Simple.Program (ConfiguredProgram, runProgram)
import Distribution.Simple.Utils (debug, writeUTF8File)
import Distribution.Verbosity (Verbosity, showForCabal)
import System.Console.GetOpt (OptDescr)
import System.Environment

import Distribution.Dev.Command (CommandActions (..), CommandResult (..))
import Distribution.Dev.Flags (Config, cfgCabalInstall, extraConfigFiles, getCabalConfig, getVerbosity, passthroughArgs, useUserConfig)
import Distribution.Dev.InitPkgDb (initPkgDb)
import Distribution.Dev.MergeCabalConfig (mergeFields, removeFlaggedFields)
import Distribution.Dev.Sandbox (KnownVersion, Sandbox, cabalConf, pkgConf, resolveSandbox, sandbox)
import qualified Distribution.Dev.CabalInstall       as CI (CabalCommand, commandToString, configDir, findOnPath, getFeatures, getUserConfig, supportsLongOption)
import qualified Distribution.Dev.RewriteCabalConfig as R (Rewrite (Rewrite), ppTopLevel, readConfigF, readConfigF_, rewriteCabalConfig)

actions :: CI.CabalCommand -> CommandActions
actions cc = CommandActions
              { cmdDesc = "Invoke cabal-install with the development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs cc args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeCabal :: Config -> CI.CabalCommand -> [String] -> IO CommandResult
invokeCabal flgs cc args = do
  let v = getVerbosity flgs
  cabal <- CI.findOnPath v $ cfgCabalInstall flgs
  res <- cabalArgs cabal flgs cc
  case res of
    Left err -> return $ CommandError err
    Right args' -> do
      runProgram v cabal $ args' ++ args
      return CommandOk

cabalArgs :: ConfiguredProgram -> Config -> CI.CabalCommand -> IO (Either String [String])
cabalArgs cabal flgs cc = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  setup s cabal flgs cc

getUserConfigFields :: IO [Field]
getUserConfigFields =
    -- If we fail to read the file, then it could be that it doesn't yet
    -- exist, and it's OK to ignore.
    either (const []) id <$> (R.readConfigF =<< CI.getUserConfig)

-- XXX: this should return an error string instead of calling "error"
-- on failure.
getDevConfigFields :: Config -> IO [Field]
getDevConfigFields cfg = R.readConfigF_ =<< getCabalConfig cfg

setup :: Sandbox KnownVersion -> ConfiguredProgram -> Config -> CI.CabalCommand -> IO (Either String [String])
setup s cabal flgs cc = do
  let v = getVerbosity flgs
  cVer <- CI.getFeatures v cabal
  devFields <- getDevConfigFields flgs
  extraConfigs <- mapM R.readConfigF_ $ extraConfigFiles flgs
  let cfgOut = cabalConf s
  case cVer of
    Just err -> return $ Left err
    Nothing -> do
      userFields <- if useUserConfig flgs then getUserConfigFields else return []
      cabalHome <- CI.configDir
      let rew = R.Rewrite cabalHome (sandbox s) (pkgConf s)
          cOut = show . R.ppTopLevel . (concat :: [[Field]] -> [Field])
               . R.rewriteCabalConfig rew
               . removeFlaggedFields
               $ foldr (flip mergeFields) userFields (devFields:extraConfigs)

      writeUTF8File cfgOut cOut

      -- Super wizard hackery to append constraints to the sandbox cabal.config
      env <- getEnvironment
      let l nm = lookup nm env
      let ress = (,) <$> l "C_FREEZE_FILE" <*> l "C_SANDBOX_FILE"
      case ress of
        Nothing -> return ()
        Just (cFreezeFile, cSandboxConf) -> do
          freeze <- readFile cFreezeFile
          appendFile cSandboxConf $ "\n" ++ freeze

      (gOpts, cOpts) <- extraArgs v cfgOut
      let gFlags = map toArg gOpts
          cFlags = map toArg $ filter (CI.supportsLongOption cc . fst) cOpts
          args = concat
                 [ -- global cabal-install flags, as
                   -- generated by cabal-dev
                   gFlags

                 -- The cabal command name
                 , [ CI.commandToString cc ]

                 -- command-specific flags, as generated
                 -- by cabal-dev
                 , cFlags

                 -- Arguments that the user specified
                 -- that we pass through
                 , passthroughArgs flgs
                 ]

      debug v $ "Complete arguments to cabal-install: " ++ show args
      return $ Right args

toArg :: Option -> String
toArg (a, mb) = showString "--" . showString a $ maybe "" ('=':) mb

-- option name, value
type Option = (String, Maybe String)

extraArgs :: Verbosity -> FilePath -> IO ([Option], [Option])
extraArgs v cfg = do
  pdbArgs <- getPdbArgs
  return ([cfgFileArg], verbosityArg:pdbArgs)
  where
    longArg s = (,) s . Just
    cfgFileArg = longArg "config-file" cfg
    verbosityArg = longArg "verbose" $ showForCabal v
    getPdbArgs = return []
