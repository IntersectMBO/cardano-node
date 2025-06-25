{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsers.Run
  ( commands
  , runTestnetCmd
  , pref
  , opts
  ) where


import           Cardano.CLI.Environment

import           Data.Foldable
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
  | CreateTestnetEnv CardanoTestnetCreateEnvOptions
  | GetVersion VersionOptions
  | Help ParserPrefs (ParserInfo CardanoTestnetCommands) HelpOptions

commands :: EnvCli -> Parser CardanoTestnetCommands
commands envCli =
  asum
    [ fmap StartCardanoTestnet (subparser (cmdCardano envCli))
    , fmap CreateTestnetEnv (subparser (cmdCreateEnv envCli))
    , fmap GetVersion (subparser cmdVersion)
    , fmap (Help pref (opts envCli)) (subparser cmdHelp)
    ]


runTestnetCmd :: CardanoTestnetCommands -> IO ()
runTestnetCmd = \case
  StartCardanoTestnet cmdOpts -> runCardanoOptions cmdOpts
  CreateTestnetEnv cmdOpts -> createEnvOptions cmdOpts
  GetVersion cmdOpts -> runVersionOptions cmdOpts
  Help pPrefs pInfo cmdOpts -> runHelpOptions pPrefs pInfo cmdOpts

createEnvOptions :: CardanoTestnetCreateEnvOptions -> IO ()
createEnvOptions CardanoTestnetCreateEnvOptions
  { createEnvTestnetOptions=testnetOptions
  , createEnvGenesisOptions=genesisOptions
  , createEnvOutputDir=outputDir
  } =
    testnetRoutine (UserProvidedEnv outputDir) $ \conf ->
      createTestnetEnv
        testnetOptions genesisOptions
        -- The CLI does not provide a way to provide custom genesis data by design:
        -- If the user wants to have custom genesis data, they should manually
        -- modify the files created by this command before running the testnet.
        NoUserProvidedData NoUserProvidedData NoUserProvidedData
        -- Do not add hashes to the main config file, so that genesis files
        -- can be modified without having to recompute hashes every time.
        conf{genesisHashesPolicy = WithoutHashes}

runCardanoOptions :: CardanoTestnetCliOptions -> IO ()
runCardanoOptions CardanoTestnetCliOptions
  { cliTestnetOptions=testnetOptions@CardanoTestnetOptions{cardanoOutputDir}
  , cliGenesisOptions=genesisOptions
  , cliNodeEnvironment=env
  } =
    case env of
      NoUserProvidedEnv ->
        -- Create the sandbox, then run cardano-testnet
        runTestnet cardanoOutputDir $ \conf -> do
          createTestnetEnv
            testnetOptions genesisOptions
            NoUserProvidedData NoUserProvidedData NoUserProvidedData
            conf
          cardanoTestnet testnetOptions genesisOptions conf
      UserProvidedEnv nodeEnvPath ->
        -- Run cardano-testnet in the sandbox provided by the user
        -- In that case, 'cardanoOutputDir' is not used
        runTestnet (UserProvidedEnv nodeEnvPath) $ cardanoTestnet testnetOptions genesisOptions
