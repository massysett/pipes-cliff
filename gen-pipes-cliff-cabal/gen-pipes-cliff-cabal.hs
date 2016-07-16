module Main (main) where

import qualified Cartel

version :: [Word]
version = [0,12,0,0]

properties :: Cartel.Properties
properties = Cartel.Properties
  { Cartel.name = "pipes-cliff"
  , Cartel.version = version
  , Cartel.cabalVersion = Just (1,10)
  , Cartel.buildType = Just Cartel.simple
  , Cartel.license = Just Cartel.bsd3
  , Cartel.licenseFile = "LICENSE"
  , Cartel.licenseFiles = []
  , Cartel.copyright = "Copyright (c) 2015-2016 Omari Norman"
  , Cartel.author = "Omari Norman"
  , Cartel.maintainer = "omari@smileystation.com"
  , Cartel.stability = "Experimental"
  , Cartel.homepage = "http://www.github.com/massysett/pipes-cliff"
  , Cartel.bugReports = "http://www.github.com/massysett/pipes-cliff/issues"
  , Cartel.packageUrl = ""
  , Cartel.synopsis = "Streaming to and from subprocesses using Pipes"
  , Cartel.description =
    [ "pipes-cliff helps you spawn subprocesses and send data to and"
    , "from them with the Pipes library."
    , "Subprocesses are opened using the"
    , "process library, and you stream data in and out using the various"
    , "Pipes abstractions."
    , ""
    , "Though this library uses the Pipes library, I have not coordinated"
    , "with the author of the Pipes library in any way.  Any bugs or design"
    , "flaws are mine and should be reported to"
    , ""
    , "<http://www.github.com/massysett/pipes-cliff/issues>"
    , ""
    , "For more information, see the README.md file, which is located in the"
    , "source tarball and at"
    , ""
    , "<https://github.com/massysett/pipes-cliff>"
    ]
  , Cartel.category = "Pipes, Concurrency"
  , Cartel.testedWith = []
  , Cartel.dataFiles = []
  , Cartel.dataDir = ""
  , Cartel.extraSourceFiles = ["README.md"]
  , Cartel.extraDocFiles = []
  , Cartel.extraTmpFiles = []
  }

ghcOptions :: Cartel.HasBuildInfo a => a
ghcOptions = Cartel.ghcOptions
  [ "-Wall"
  ]

commonOptions :: Cartel.HasBuildInfo a => [a]
commonOptions
  = ghcOptions
  : Cartel.haskell2010
  : Cartel.hsSourceDirs ["lib"]
  : []

exeOptions
  :: Cartel.HasBuildInfo a
  => [Cartel.NonEmptyString]
  -- ^ Library modules
  -> [Cartel.NonEmptyString]
  -- ^ Executable modules
  -> [a]
exeOptions libMods exeMods =
  [ Cartel.hsSourceDirs ["lib", "tests"]
  , Cartel.otherModules (exeMods ++ libMods)
  , Cartel.buildDepends libraryDepends
  , Cartel.ghcOptions ["-threaded"]
  ]

testExe
  :: Cartel.FlagName
  -- ^ Tests flag
  -> [Cartel.NonEmptyString]
  -- ^ Library modules
  -> [Cartel.NonEmptyString]
  -- ^ Test modules
  -> String
  -- ^ Name of executable
  -> Cartel.Section
testExe fl libMods testMods nm = Cartel.executable nm $
  [ Cartel.mainIs (nm ++ ".hs")
  , Cartel.condBlock (Cartel.flag fl)
    (Cartel.buildable True, commonOptions ++ exeOptions libMods testMods)
    [Cartel.buildable False]
  ]

sections
  :: Cartel.FlagName
  -- ^ Tests flag
  -> [Cartel.NonEmptyString]
  -- ^ Library modules
  -> [Cartel.NonEmptyString]
  -- ^ Test modules
  -> [Cartel.Section]
sections fl libMods testMods =
  [ Cartel.githubHead "massysett" "pipes-cliff"
  ] ++ map (testExe fl libMods testMods)
           [ "numsToLess", "alphaNumbers", "limitedAlphaNumbers",
             "alphaNumbersByteString", "standardOutputAndError" ]

libraryDepends :: [Cartel.Package]
libraryDepends =
  [ Cartel.closedOpen "base" [4,9] [5]
  , Cartel.atLeast "pipes" [4,1]
  , Cartel.atLeast "pipes-safe" [2,2]
  , Cartel.atLeast "bytestring" [0,10,4]
  , Cartel.atLeast "process" [1,2,0,0]
  , Cartel.atLeast "async" [2,0]
  , Cartel.atLeast "stm" [2,4,4]
  , Cartel.atLeast "unix" [2,7,2]
  ]

library
  :: [Cartel.NonEmptyString]
  -- ^ List of library modules
  -> [Cartel.LibraryField]
library libModules
  = Cartel.buildDepends libraryDepends
  : Cartel.exposedModules libModules
  : commonOptions

main :: IO ()
main = Cartel.defaultMain $ do
  libModules <- Cartel.modules "../pipes-cliff/lib"
  testModules <- Cartel.modules "../pipes-cliff/tests"
  fl <- Cartel.makeFlag "tests" $ Cartel.FlagOpts
    { Cartel.flagDescription = "Build test executables"
    , Cartel.flagDefault = False
    , Cartel.flagManual = True
    }
  return (properties, library libModules, sections fl libModules testModules)
