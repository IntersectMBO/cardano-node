{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsers.Run
  ( commands
  , runTestnetCmd
  , pref
  , opts
  ) where
import           Cardano.CLI.Environment

import           Control.Monad (void)
import           Data.Default.Class (def)
import           Options.Applicative
import qualified Options.Applicative as Opt

import           Testnet.Start.Cardano
import           Testnet.Start.Types

import           Parsers.Cardano
import           Parsers.Help
import           Parsers.Version
import           RIO (display, forever, logInfo, runSimpleApp, threadDelay)
import           UnliftIO.Resource (runResourceT)

pref :: ParserPrefs
pref = Opt.prefs $ showHelpOnEmpty <> showHelpOnError

opts :: EnvCli -> ParserInfo CardanoTestnetCommands
opts envCli = Opt.info (commands envCli <**> helper) idm

data CardanoTestnetCommands
  = StartCardanoTestnet CardanoTestnetCliOptions
  | CreateTestnetEnv CardanoTestnetCreateEnvOptions
  | GetVersion VersionOptions
  | Help ParserPrefs (ParserInfo CardanoTestnetCommands) HelpOptions

commands :: EnvCli -> Parser CardanoTestnetCommands
commands envCli =
  asum
    [ fmap StartCardanoTestnet (subparser cmdCardano)
    , fmap CreateTestnetEnv (subparser cmdCreateEnv)
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
  , createEnvCreateEnvOptions=ceOptions
  } = do
      conf <- mkConfigAbs outputDir
      createTestnetEnv
        testnetOptions genesisOptions ceOptions
        -- Do not add hashes to the main config file, so that genesis files
        -- can be modified without having to recompute hashes every time.
        conf{genesisHashesPolicy = WithoutHashes}

runCardanoOptions :: CardanoTestnetCliOptions -> IO ()
runCardanoOptions CardanoTestnetCliOptions
  { cliTestnetOptions=testnetOptions
  , cliGenesisOptions=genesisOptions
  , cliNodeEnvironment=env
  , cliUpdateTimestamps=updateTimestamps'
  } = do
    case env of
      NoUserProvidedEnv -> do
        -- Create the sandbox, then run cardano-testnet.
        -- It is not necessary to honor `cliUpdateTimestamps` here, because
        -- the genesis files will be created with up-to-date stamps already.
        conf <- mkConfigAbs "testnet"
        runSimpleApp . runResourceT $ do
          logInfo $ "Creating environment: " <> display (tempAbsPath conf)
          createTestnetEnv testnetOptions genesisOptions def conf
          logInfo $ "Starting testnet in environment: " <> display (tempAbsPath conf)
          void $ cardanoTestnet testnetOptions conf
          logInfo "Testnet started"
          waitForShutdown
      UserProvidedEnv nodeEnvPath -> do
        -- Run cardano-testnet in the sandbox provided by the user
        -- In that case, 'cardanoOutputDir' is not used
        conf <- mkConfigAbs nodeEnvPath
        runSimpleApp . runResourceT $ do
          logInfo $ "Starting testnet in environment: " <> display (tempAbsPath conf)
          void $ cardanoTestnet
            testnetOptions
            conf{updateTimestamps=updateTimestamps'}
          logInfo "Testnet started"
          waitForShutdown
  where
    waitForShutdown = do
      logInfo "Waiting for shutdown (Ctrl+C)"
      forever (threadDelay 100_000)
