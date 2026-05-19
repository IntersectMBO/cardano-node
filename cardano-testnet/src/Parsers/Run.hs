{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import           Data.Maybe (fromMaybe)
import           Options.Applicative
import qualified Options.Applicative as Opt
import           System.Directory (doesDirectoryExist)

import           Testnet.Filepath (unTmpAbsPath)
import           Testnet.Start.Cardano
import           Testnet.Start.Types

import           Parsers.Cardano
import           Parsers.Help
import           Parsers.Version
import           RIO (display, forever, logInfo, runSimpleApp, threadDelay)
import           UnliftIO.Resource (runResourceT)
import           Cardano.Prelude (unlessM)

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
  { createEnvCreationOptions=creationOptions
  , createEnvOutputDir=outputDir
  } = do
      conf <- mkConfigAbs outputDir
      void $ createTestnetEnv
        creationOptions
        -- Do not add hashes to the main config file, so that genesis files
        -- can be modified without having to recompute hashes every time.
        conf{genesisHashesPolicy = WithoutHashes}

runCardanoOptions :: CardanoTestnetCliOptions -> IO ()
runCardanoOptions = \case
  NoUserProvidedEnv NoUserProvidedEnvOptions{noEnvCreationOptions, noEnvOutputDir, noEnvRuntimeOptions} -> do
    -- Create the sandbox, then run cardano-testnet.
    -- It is not necessary to update timestamps here, because
    -- the genesis files will be created with up-to-date stamps already.
    let dirName = fromMaybe "testnet" noEnvOutputDir
    conf <- mkConfigAbs dirName
    runSimpleApp . runResourceT $ do
      logInfo $ "Creating environment: " <> display (tempAbsPath conf)
      createTestnetEnv noEnvCreationOptions conf
      logInfo $ "Starting testnet in environment: " <> display (tempAbsPath conf)
      void $ cardanoTestnet (creationNodes noEnvCreationOptions) noEnvRuntimeOptions conf
      logInfo "Testnet started"
      waitForShutdown
  StartFromEnv StartFromEnvOptions{fromEnvOptions, fromEnvRuntimeOptions} -> do
    -- Run cardano-testnet in the sandbox provided by the user
    let dirName = envPath fromEnvOptions
    unlessM (doesDirectoryExist dirName) $ error $ "The provided path does not exist or is not a directory: " <> dirName
    conf <- mkConfigAbs dirName
    nodes <- readNodeOptionsFromEnv (unTmpAbsPath (tempAbsPath conf))
    runSimpleApp . runResourceT $ do
      logInfo $ "Starting testnet in environment: " <> display (tempAbsPath conf)
      void $ cardanoTestnet nodes fromEnvRuntimeOptions
               conf{updateTimestamps = envUpdateTimestamps fromEnvOptions}
      logInfo "Testnet started"
      waitForShutdown
  where
    waitForShutdown = do
      logInfo "Waiting for shutdown (Ctrl+C)"
      forever (threadDelay 100_000)
