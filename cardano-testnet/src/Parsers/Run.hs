{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsers.Run
  ( commands
  , runTestnetCmd
  , pref
  , opts
  ) where

import           Cardano.Api.Ledger (StandardCrypto)
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
        -- The paths to the genesis files have to exist in the node configuration file,
        -- but the files themselves don't have to exist. So if the user wants to pass
        -- its custom genesis file, the file has to exist (and will be mapped to a 'UserProvidedData'
        -- constructor below). But if the user wants to have cardano-testnet generate the genesis file,
        -- he should leave the filepath in the node configuration file, but omit the genesis file itself.
        -- In this latter case, the code below maps the genesis file to a 'NoUserProvidedData' constructor.
        shelleyGenesis :: UserProvidedData (ShelleyGenesis StandardCrypto) <-
          genesisFilepathToData $ npcShelleyGenesisFile shelley
        alonzoGenesis :: UserProvidedData AlonzoGenesis <-
          genesisFilepathToData $ npcAlonzoGenesisFile alonzo
        conwayGenesis :: UserProvidedData (ConwayGenesis StandardCrypto) <-
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
        else return NoUserProvidedData
