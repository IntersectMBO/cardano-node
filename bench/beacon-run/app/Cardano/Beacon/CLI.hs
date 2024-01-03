module  Cardano.Beacon.CLI
        ( BeaconCommand(..)
        , BeaconOptions(..)
        , getOpts
        ) where

import           Data.Text as Text (Text, pack)
import           Options.Applicative

import           Cardano.Beacon.Types


data BeaconCommand =
    -- commands that can be invoked from the CLI;
    -- commands can be chained
      BeaconListChains
    | BeaconBuild       !Version
    | BeaconDoRun       !ChainName !Version !Int
    | BeaconStoreRun    !FilePath

    -- commands that can't be used directly from the CLI
    | BeaconLoadChains
    | BeaconLoadCommit  !String
    deriving Show

data BeaconOptions = BeaconOptions {
      optEchoing        :: !EchoCommand
    , optBeaconDir      :: !FilePath
    , optLockFile       :: !FilePath
    }
    deriving Show

--------------------------------------------------------------------------------
-- Command line parsing
--------------------------------------------------------------------------------

getOpts :: IO (BeaconOptions, [BeaconCommand])
getOpts = execParser $ info (parseCLI <**> helper) fullDesc

parseCLI :: Parser (BeaconOptions, [BeaconCommand])
parseCLI = (,) <$> parseOptions <*> some parseCommand

parseOptions :: Parser BeaconOptions
parseOptions =
  BeaconOptions
    <$> flag DoNotEchoCommand EchoCommand
        (mconcat
          [ short 'e'
          , long "echo"
          , help "Echo shell commands"
          ])
    <*> strOption
        (mconcat
          [ short 'd'
          , long "data-dir"
          , value "./beacon"
          , showDefault
          , metavar "DIR"
          , action "directory"
          , help "Specify beacon data directory"
          ])
    <*> strOption
        (mconcat
          [ long "lock"
          , value ""
          , metavar "FILE"
          , help "Use a lock file"
          ])

parseCommand :: Parser BeaconCommand
parseCommand =  subparser $ mconcat
  [ op "build" "Build and link target binary only"
      (BeaconBuild <$> parseVersion)
  , op "list-chains" "List registered chain fragments that beacon can be run on"
      (pure BeaconListChains)
  , op "run" "Perform a beacon run"
      (BeaconDoRun <$> (ChainName . Text.pack <$> parseChainName) <*> parseVersion <*> parseCount)
  , op "store" "Store a run"
      (BeaconStoreRun <$> parseFileName)
  , op "test-github" "Test the GitHub query on a given git ref"
      (BeaconLoadCommit <$> parseRevision)
  ]
  where
    op :: String -> String -> Parser a -> Mod CommandFields a
    op c descr p =
     command c $ info (p <**> helper) $
       mconcat [ progDesc descr ]

    parseRevision :: Parser String
    parseRevision = strOption
      (mconcat
        [ long "rev"
        , metavar "REF"
        , help "Commit hash (full or shortened) or tag or branch name"
        ])

    parseGHCVersion :: Parser String
    parseGHCVersion = strOption
      (mconcat
        [ long "ghc"
        , metavar "VER"
        , value "haskell810"
        , showDefault
        , help "Compiler version; cf. ouroboros-consensus-cardano/README.md#Assertions"
        ])

    parseChainName :: Parser String
    parseChainName = strOption
      (mconcat
        [ short 'n'
        , long "name"
        , metavar "NAME"
        , help "Chain fragment name to run on"
        ])

    parseCount :: Parser Int
    parseCount = option auto
      (mconcat
        [ short 'c'
        , value 1
        , showDefault
        , help "Number of times to perform the run"
        ])

    parseVersion :: Parser Version
    parseVersion =  Version <$> parseRevision <*> parseGHCVersion

    parseFileName :: Parser FilePath
    parseFileName = strArgument
      (mconcat
        [ metavar "FILE"
        , action "file"
        , help "JSON run file to be stored"
        ])
