module Main (main) where

import Cartel

props :: Properties
props = blank
  { name = "pipes-cliff"
  , version = [0,10,0,4]
  , cabalVersion = Just (1, 18)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , licenseFiles = []
  , copyright = "Copyright 2015-2016 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/pipes-cliff"
  , bugReports = "http://www.github.com/massysett/pipes-cliff/issues"
  , synopsis = "Streaming to and from subprocesses using Pipes"
  , description =
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
  , category = "Pipes, Concurrency"
  , testedWith = map (\v -> (ghc, eq v)) [[7,10,1]]
  , extraSourceFiles = ["README.md"]
  }

defaultOptions :: HasBuildInfo a => [a]
defaultOptions =
  [ haskell2010
  , ghcOptions ["-Wall"]
  ]

library
  :: [NonEmptyString]
  -- ^ Library modules
  -> [LibraryField]
library mods =
  [ exposedModules mods
  , hsSourceDirs ["lib"]
  , libDeps
  ] ++ defaultOptions

exeOptions
  :: HasBuildInfo a
  => [NonEmptyString]
  -- ^ Library modules
  -> [NonEmptyString]
  -- ^ Executable modules
  -> [a]
exeOptions libMods exeMods =
  [ hsSourceDirs ["lib", "tests"]
  , otherModules (exeMods ++ libMods)
  , libDeps
  , ghcOptions ["-threaded"]
  ]

testExe
  :: FlagName
  -- ^ Tests flag
  -> [NonEmptyString]
  -- ^ Library modules
  -> [NonEmptyString]
  -- ^ Test modules
  -> String
  -- ^ Name of executable
  -> Section
testExe fl libMods testMods nm = executable nm $
  [ mainIs (nm ++ ".hs")
  , condBlock (flag fl)
    (buildable True, defaultOptions ++ exeOptions libMods testMods)
    [buildable False]
  ]

sections
  :: FlagName
  -- ^ Tests flag
  -> [NonEmptyString]
  -- ^ Library modules
  -> [NonEmptyString]
  -- ^ Test modules
  -> [Section]
sections fl libMods testMods =
  [ githubHead "massysett" "pipes-cliff"
  ] ++ map (testExe fl libMods testMods)
           [ "numsToLess", "alphaNumbers", "limitedAlphaNumbers",
             "alphaNumbersByteString", "standardOutputAndError" ]

-- # Packages

base :: Package
base = closedOpen "base" [4,7,0,0] [5]

pipes :: Package
pipes = package "pipes" (gtEq [4,1])

pipesSafe :: Package
pipesSafe = package "pipes-safe" (gtEq [2,2])

bytestring :: Package
bytestring = package "bytestring" (gtEq [0,10,4])

process :: Package
process = package "process" (gtEq [1,2,0,0])

async :: Package
async = package "async" (gtEq [2,0])

stm :: Package
stm = package "stm" (gtEq [2,4,4])

libDeps :: HasBuildInfo a => a
libDeps = buildDepends
  [ base
  , pipes
  , pipesSafe
  , bytestring
  , process
  , async
  , stm
  ]

main :: IO ()
main = defaultMain $ do
  libMods <- modules "lib"
  testMods <- modules "tests"
  fl <- makeFlag "tests" $ FlagOpts
    { flagDescription = "Build test executables"
    , flagDefault = False
    , flagManual = True
    }
  return ( props, library libMods, sections fl libMods testMods )
