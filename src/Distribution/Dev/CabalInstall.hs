{-# LANGUAGE TemplateHaskell #-}
module Distribution.Dev.CabalInstall
  ( findOnPath
  , program
  , getFeatures
  , configDir
  , CabalCommand(..)
  , commandToString
  , stringToCommand
  , allCommands
  , commandOptions
  , supportsLongOption
  , getUserConfig
  ) where

import Control.Applicative (pure, (<$>))
import Data.Maybe (fromMaybe)
import Distribution.Simple.Program (ConfiguredProgram, Program (programFindVersion), emptyProgramConfiguration, findProgramVersion, getProgramOutput,
                                    programLocation, programVersion, requireProgram, simpleProgram)
import Distribution.Simple.Utils (debug)
import Distribution.Text (display, simpleParse)
import Distribution.Verbosity (Verbosity)
import Distribution.Version (Version)
import System.Environment (getEnvironment)
import System.FilePath ((</>))

import System.Directory (getAppUserDataDirectory)

import Distribution.Dev.InterrogateCabalInstall (ArgType (..), Option (..), OptionName (..))
import Distribution.Dev.TH.DeriveCabalCommands (deriveCabalCommands)

-- XXX This is duplicated in Setup.hs
-- |Definition of the cabal-install program
program :: Maybe String -> Program
program p =
  (simpleProgram $ fromMaybe "cabal" p)
    { programFindVersion = findProgramVersion "--numeric-version" id }

-- |Find cabal-install on the user's PATH
findOnPath :: Verbosity -> Maybe FilePath -> IO ConfiguredProgram
findOnPath v ci = do
  (cabal, _) <- requireProgram v (program ci) emptyProgramConfiguration
  debug v $ concat [ "Using cabal-install "
                   , maybe "(unknown version)" display $ programVersion cabal
                   , " at "
                   , show (programLocation cabal)
                   ]
  return cabal

-- |Parse the Cabal library version from the output of cabal --version
parseVersionOutput :: String -> Either String Version
parseVersionOutput str =
  case lines str of
    []      -> Left "No version string provided."
    [_]     -> Left "Could not find Cabal version line."
    (_:ln:_) -> case simpleParse ((words ln)!!2) of
      Just v  -> Right v
      Nothing -> Left $ err ln
        where
          err line = "Could not parse Cabal verison.\n" ++ "(simpleParse "++show line++")"

-- |Extract the features of this cabal-install executable
getFeatures :: Verbosity -> ConfiguredProgram -> IO (Maybe String)
getFeatures v cabal = case programVersion cabal of
  Nothing -> return $ Just "Failed to find cabal-install version"
  Just _ -> do
    verRes <- parseVersionOutput <$> getProgramOutput v cabal ["--version"]
    case verRes of
      Left err -> return $ Just $ "Detecting cabal-install's Cabal: " ++ err
      Right _ -> return Nothing

deriveCabalCommands

supportsLongOption :: CabalCommand -> String -> Bool
supportsLongOption cc s = any ((`matchLongOption` s) . optionName) $ supportedOptions cc

optionName :: Option -> OptionName
optionName (Option n _) = n

supportedOptions :: CabalCommand -> [Option]
supportedOptions cc = commonOptions ++ commandOptions cc

matchLongOption :: OptionName -> String -> Bool
matchLongOption (Short _) = const False
matchLongOption (LongOption s) = (== s)

commonOptions :: [Option]
commonOptions = [Option (LongOption "config-file") Req]

-- |What is the configuration directory for this cabal-install executable?

-- XXX: This needs to do something different for certain platforms for
-- new versions of cabal-install (look at the tickets on creswick's
-- cabal-dev repo)
configDir :: IO FilePath
configDir = getAppUserDataDirectory "cabal"

getUserConfig :: IO FilePath
getUserConfig = do
  env <- lookup "CABAL_CONFIG" <$> getEnvironment
  case env of
    Nothing -> (</> "config") <$> configDir
    Just f  -> pure f
