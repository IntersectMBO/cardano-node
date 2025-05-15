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
  CreateTestnetEnv cmdOpts -> createCardanoEnv cmdOpts
  GetVersion cmdOpts -> runVersionOptions cmdOpts
  Help pPrefs pInfo cmdOpts -> runHelpOptions pPrefs pInfo cmdOpts

createCardanoEnv :: CardanoTestnetCreateEnvOptions -> IO ()
createCardanoEnv CardanoTestnetCreateEnvOptions
  { createEnvTestnetOptions=testnetOptions
  , createEnvGenesisOptions=genesisOptions
  , createEnvOutputDir=outputDir
  } =
    runTestnet (UserProvidedEnv outputDir) $ createTestnetEnvDefault
      testnetOptions genesisOptions

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
          createTestnetEnvDefault testnetOptions genesisOptions conf
          cardanoTestnet testnetOptions genesisOptions conf
      UserProvidedEnv nodeEnvPath ->
        -- Run cardano-testnet in the sandbox provided by the user
        runTestnet (UserProvidedEnv nodeEnvPath) $ cardanoTestnet testnetOptions genesisOptions
