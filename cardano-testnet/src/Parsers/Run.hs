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

import           Data.Foldable
import           Data.Yaml (decodeFileThrow)
import           Options.Applicative
import qualified Options.Applicative as Opt

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
runCardanoOptions (CardanoTestnetCliOptions testnetOptions shelleyOptions) =
    case cardanoInputConfigs testnetOptions of
      Nothing -> runTestnet testnetOptions $ cardanoTestnetDefault testnetOptions shelleyOptions
      Just inputConfigFiles -> do
        let nodeConfigFile = UserNodeConfig $ icfNodeConfigFile inputConfigFiles
        shelleyGenesisFile' :: ShelleyGenesis StandardCrypto <-
          decodeFileThrow $ icfShelleyGenesisConfigFile inputConfigFiles
        alonzoGenesisFile :: AlonzoGenesis <-
          decodeFileThrow $ icfAlonzoGenesisConfigFile inputConfigFiles
        conwayGenesisFile ::  ConwayGenesis StandardCrypto <-
          decodeFileThrow $ icfConwayGenesisConfigFile inputConfigFiles
        runTestnet testnetOptions $ cardanoTestnet
          testnetOptions
          nodeConfigFile
          shelleyGenesisFile'
          alonzoGenesisFile
          conwayGenesisFile
