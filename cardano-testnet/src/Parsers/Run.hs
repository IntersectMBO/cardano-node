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

import           Control.Monad (forM_)
import           Data.Default.Class (def)
import qualified Data.Text as Text
import           Options.Applicative
import qualified Options.Applicative as Opt

import           Testnet.Start.Cardano
import           Testnet.Start.Types
import           Testnet.Types (TestnetNode (..))

import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as H

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
          testnetRuntime <- cardanoTestnet testnetOptions conf
          logTestnetInfo conf testnetRuntime
          waitForShutdown
      UserProvidedEnv nodeEnvPath -> do
        -- Run cardano-testnet in the sandbox provided by the user
        -- In that case, 'cardanoOutputDir' is not used
        conf <- mkConfigAbs nodeEnvPath
        runSimpleApp . runResourceT $ do
          testnetRuntime <- cardanoTestnet
            testnetOptions
            conf{updateTimestamps=updateTimestamps'}
          logTestnetInfo conf testnetRuntime
          waitForShutdown
  where
    logTestnetInfo conf testnetRuntime = do
      let nodes = testnetNodes testnetRuntime
      logInfo ""
      logInfo $ "Testnet started in " <> display (tempAbsPath conf)
                <> " with " <> display (length nodes) <> " nodes."
      logInfo ""
      case nodes of
        (firstNode:_) -> do
          logInfo "To interact with the testnet using cardano-cli:"
          logInfo $ "  export CARDANO_NODE_NETWORK_ID=" <> display (testnetMagic testnetRuntime)
          logInfo $ "  export CARDANO_NODE_SOCKET_PATH=" <> display (Text.pack (H.sprocketSystemName (nodeSprocket firstNode)))
          logInfo ""
        [] -> pure ()
      logInfo "Node sockets:"
      forM_ nodes $ \node ->
        logInfo $ "  " <> display (Text.pack (nodeName node)) <> "  " <> display (Text.pack (H.sprocketSystemName (nodeSprocket node)))
      logInfo ""

    waitForShutdown = do
      logInfo "Press Ctrl+C to stop all nodes."
      forever (threadDelay 100_000)