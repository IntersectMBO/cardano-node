module  Cardano.Beacon.CLI
        ( BeaconCommand(..)
        , BeaconOptions(..)
        , getOpts
        ) where

import           Options.Applicative
import Data.Text as Text (Text, pack)

import           Cardano.Beacon.Types


data BeaconCommand =
    -- commands that can be invoked from the CLI;
    -- commands can be chained
      BeaconListChains
    | BeaconBuild       !Version
    | BeaconRun         !Chain !Version !Int

    -- commands that can't be used directly from the CLI
    | BeaconLoadChains
    | BeaconLoadCommit  !String
    deriving Show

data BeaconOptions = BeaconOptions {
      optEchoing        :: !EchoCommand
    , optBeaconDir      :: !FilePath
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

parseCommand :: Parser BeaconCommand
parseCommand =  subparser $ mconcat
  [ op "build" "Build and link target binary only"
      (BeaconBuild <$> parseVersion)
  , op "list-chains" "List registered chain fragments that beacon can be run on"
      (pure BeaconListChains)
  , op "run" "Perform a beacon run"
      (BeaconRun <$> (Chain . Text.pack <$> parseChainName) <*> parseVersion <*> parseCount)
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

    {-
    parseOpts :: Parser BeaconOptions_
    parseOpts =
        BeaconOptions_
          <$> parseVersion "versionA"
          <*> parseVersion "versionB"
          <*> parseNodeHome
          <*> parseConfigPath
          <*> parseDBPath
          <*> parseAnalyseFrom
          <*> parseNumBlocksToProcess



    parseNodeHome :: Parser FilePath
    parseNodeHome = strOption
                    ( mconcat
                      [ long "node-home"
                      , help "File path to the 'mainnet' data directory of the node."
                      ]
                    )

    parseConfigPath :: Parser FilePath
    parseConfigPath = strOption
                      ( mconcat
                        [ long "config-path"
                        , value "/configuration/cardano/mainnet-config.json"
                        , help "File path to the config.json file. Relative to node-home. When not passed this defaults to /configuration/cardano/mainnet-config.json"
                        ]
                      )

    parseDBPath :: Parser FilePath
    parseDBPath = strOption
                    ( mconcat
                      [ long "db-path"
                      , value "/mainnet/db"
                      , help "File path to the db file analyzed by db-analyzer. Relative to node-home. When not passed this defaults to mainnet/db"
                      ]
                    )

    -}
