import Control.Monad (unless)
import Distribution.PackageDescription (PackageDescription (..), buildInfo, exeName, executables, hsSourceDirs)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import Distribution.Simple.Program.Types (Program (..), simpleProgram)
import Distribution.Simple.Setup (BuildFlags, buildVerbose)
import Distribution.Simple.Utils (findProgramVersion, rawSystemExit)
import Distribution.Verbosity (Verbosity)

import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

main = defaultMainWithHooks $
       simpleUserHooks { hookedPrograms = [cabalInstallProgram]
                       }

cabalInstallProgram :: Program
cabalInstallProgram = (simpleProgram "cabal") {
  programFindVersion = findProgramVersion "--numeric-version" id
  }
