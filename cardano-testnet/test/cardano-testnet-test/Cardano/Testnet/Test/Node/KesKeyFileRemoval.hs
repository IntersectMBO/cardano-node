{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Node.KesKeyFileRemoval
  ( hprop_sighup_removed_kes_key_disables_forging
  ) where

import           Cardano.Api (BlockNo (..))

import           Cardano.Testnet

import           Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Default.Class (def)
import           Data.List (isInfixOf)
import           System.Directory (removeFile)
import           System.FilePath ((</>))

import           Testnet.Components.Query (getBlockNumber, getEpochStateView, waitForBlocks)
import           Testnet.Defaults (defaultSpoKesSKeyFp)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Signal (reloadNodeConfiguration)
import           Testnet.Start.Cardano (liftToIntegration)
import           Testnet.Start.Types (PraosCredentialsSource (..))

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | Removing the KES signing key file and sending @SIGHUP@ disables block
-- forging: the node reports @DisabledBlockForging@ and produces no further
-- blocks. This is the operator flow for taking a block producer out of
-- rotation, and it must keep working now that the key file is read by
-- consensus rather than by the node.
--
-- Execute this test with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/KES key file removal/"'@
hprop_sighup_removed_kes_key_disables_forging :: Property
hprop_sighup_removed_kes_key_disables_forging =
  integrationRetryWorkspace 2 "kes-key-file-removal" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
    conf@Conf { tempAbsPath = TmpAbsolutePath work } <- mkConf tempAbsBasePath'

    -- Create the environment first, so that the node configuration can be
    -- adjusted before the nodes start: 'BlockForgingUpdate' is traced at
    -- severity Info, below the default cut-off of the Startup namespace.
    let creationOptions = def
    liftToIntegration $ createTestnetEnv creationOptions conf
    let configurationFile' = work </> "configuration.yaml"
    config <- H.evalIO (Aeson.eitherDecodeFileStrict' configurationFile') >>= H.evalEither
    configObj <- case config of
      Aeson.Object o -> pure o
      _ -> H.failure
    let traceOptions = Aeson.object [ "Startup" Aeson..= Aeson.object [ "severity" Aeson..= ("Info" :: String) ] ]
        config' = KeyMap.insert "TraceOptions" traceOptions configObj
    H.lbsWriteFile configurationFile' $ Aeson.encodePretty $ Aeson.Object config'

    -- The SPO reads its KES key from a file. Forging is expected to stop, so
    -- the chain-stall watchdog must not treat that as a failure.
    let runtimeOptions = def
          { runtimeKESSource = UseKesKeyFile
          , runtimeEnableChainStallWatchdog = False
          }
    runtime@TestnetRuntime { configurationFile } <-
      liftToIntegration $ cardanoTestnet (creationNodes creationOptions) runtimeOptions conf

    spo <- case spoNodes runtime of
      [spo] -> pure spo
      spos -> do
        H.note_ $ "Expected exactly one SPO node, found: " <> show (nodeName <$> spos)
        H.failure
    let spoLog = nodeStdout spo
    H.note_ spoLog

    -- Let the SPO forge: with a single SPO every block on the chain is its.
    epochStateView <- getEpochStateView configurationFile (nodeSocketPath spo)
    BlockNo forgedBefore <- waitForBlocks epochStateView 2
    H.note_ $ "Blocks forged before removing the key file: " <> show forgedBefore

    -- Remove the key file and ask the node to reload its credentials.
    let kesKeyFile = work </> defaultSpoKesSKeyFp 1
    H.note_ kesKeyFile
    H.evalIO $ removeFile kesKeyFile
    H.evalIO $ reloadNodeConfiguration (nodeProcessHandle spo)

    -- The node reports that forging is disabled, and no reload error.
    H.byDurationM 1 60 "Expected the node to report DisabledBlockForging" $ do
      logLines <- lines <$> H.readFile spoLog
      H.assert $ any ("DisabledBlockForging" `isInfixOf`) logLines
    logLines <- lines <$> H.readFile spoLog
    H.assert . not $ any ("BlockForgingUpdateError" `isInfixOf`) logLines

    -- No further blocks: with slots of 0.1 s and an active slot coefficient
    -- of 0.05, a forging SPO would produce about five blocks in 10 s.
    H.threadDelay 2_000_000
    BlockNo afterDisable <- getBlockNumber epochStateView
    H.threadDelay 10_000_000
    BlockNo later <- getBlockNumber epochStateView
    H.note_ $ "Block number after disabling forging: " <> show afterDisable <> ", 10 s later: " <> show later
    later === afterDisable
