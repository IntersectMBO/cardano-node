{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsers.Run
  ( commands
  , runTestnetCmd
  , pref
  , opts
  ) where

import           Cardano.Api.Shelley (ShelleyGenesis)

import           Cardano.CLI.Environment
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis)
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Types

import qualified Data.Aeson as Aeson
import           Data.Foldable
import           Data.Monoid
import           Data.Yaml (decodeFileThrow)
import           Options.Applicative
import qualified Options.Applicative as Opt
import qualified System.Directory as System
import           System.FilePath (takeDirectory, (</>))

import           Testnet.Property.Run
import           Testnet.Start.Cardano
import           Testnet.Start.Types

import           Parsers.Cardano
import           Parsers.Help
import           Parsers.Version

pref :: ParserPrefs
pref = Opt.prefs $ showHelpOnEmpty <> showHelpOnError

opts :: EnvCli -> ParserInfo CardanoTestnetCommands
opts envCli = Opt.info (commands envCli <**> helper) idm

-- TODO: Remove StartBabbageTestnet and StartShelleyTestnet
-- by allowing the user to start testnets in any era (excluding Byron)
-- via StartCardanoTestnet
data CardanoTestnetCommands
  = StartCardanoTestnet CardanoTestnetCliOptions
  | GetVersion VersionOptions
  | Help ParserPrefs (ParserInfo CardanoTestnetCommands) HelpOptions

commands :: EnvCli -> Parser CardanoTestnetCommands
commands envCli =
  asum
    [ fmap StartCardanoTestnet (subparser (cmdCardano envCli))
    , fmap GetVersion (subparser cmdVersion)
    , fmap (Help pref (opts envCli)) (subparser cmdHelp)
    ]


runTestnetCmd :: CardanoTestnetCommands -> IO ()
runTestnetCmd = \case
  StartCardanoTestnet cmdOpts -> runCardanoOptions cmdOpts
  GetVersion cmdOpts -> runVersionOptions cmdOpts
  Help pPrefs pInfo cmdOpts -> runHelpOptions pPrefs pInfo cmdOpts


runCardanoOptions :: CardanoTestnetCliOptions -> IO ()
runCardanoOptions (CardanoTestnetCliOptions testnetOptions genesisOptions) =
    case cardanoNodes testnetOptions of
      AutomaticNodeOptions _ -> runTestnet testnetOptions $ cardanoTestnetDefault testnetOptions genesisOptions
      UserProvidedNodeOptions nodeInputConfigFile -> do
        nodeConfigFile <- readNodeConfigurationFile nodeInputConfigFile
        let protocolConfig :: NodeProtocolConfiguration =
              case getLast $ pncProtocolConfig nodeConfigFile of
                Nothing -> error $ "Genesis files not specified in node configuration file: " <> nodeInputConfigFile
                Just x -> x
            adjustedProtocolConfig =
              -- Make all the files be relative to the location of the config file.
              adjustFilePaths (takeDirectory nodeInputConfigFile </>) protocolConfig
            (shelley, alonzo, conway) = getShelleyGenesises adjustedProtocolConfig
        shelleyGenesis :: UserProvidedData ShelleyGenesis <-
          genesisFilepathToData $ npcShelleyGenesisFile shelley
        alonzoGenesis :: UserProvidedData AlonzoGenesis <-
          genesisFilepathToData $ npcAlonzoGenesisFile alonzo
        conwayGenesis :: UserProvidedData ConwayGenesis <-
          genesisFilepathToData $ npcConwayGenesisFile conway
        runTestnet testnetOptions $ cardanoTestnet
          testnetOptions
          genesisOptions
          shelleyGenesis
          alonzoGenesis
          conwayGenesis
  where
    getShelleyGenesises (NodeProtocolConfigurationCardano _byron shelley alonzo conway _hardForkConfig _checkPointConfig) =
      (shelley, alonzo, conway)
    readNodeConfigurationFile :: FilePath -> IO PartialNodeConfiguration
    readNodeConfigurationFile file = do
      errOrNodeConfig <- Aeson.eitherDecodeFileStrict' file
      case errOrNodeConfig of
        Left err -> error $ "Error reading node configuration file: " <> err
        Right nodeConfig -> pure nodeConfig
    genesisFilepathToData :: Aeson.FromJSON a => GenesisFile -> IO (UserProvidedData a)
    genesisFilepathToData (GenesisFile filepath) = do
      exists <- System.doesFileExist filepath
      if exists
        then UserProvidedData <$> decodeFileThrow filepath
        else
          -- Here we forbid mixing defaulting of genesis files with providing a user node configuration file:
          --
          -- Because of this check, either the user passes a node configuration file AND he passes all the genesis files.
          -- Or the user doesn't pass a node configuration file AND cardano-testnet generates all the genesis files.
          -- See the discussion here: https://github.com/IntersectMBO/cardano-node/pull/6103#discussion_r1995431437
          error $ "Genesis file specified in node configuration file does not exist: " <> filepath
