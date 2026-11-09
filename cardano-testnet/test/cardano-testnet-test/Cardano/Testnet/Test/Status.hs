{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Status
  ( hprop_status
  , hprop_status_no_testnet
  ) where

import           Cardano.Api (AnyCardanoEra (..), CardanoEra (..), EpochNo (..), SlotNo (..))

import           Cardano.Testnet (TestnetNode (..), TestnetRuntime (..), createAndRunTestnet,
                   defaultManifestFile, isTestnetNodeSpo, mkConf)

import           Prelude

import           Control.Monad (forM_, void)
import           Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (key)
import           Data.Default.Class (def)
import           Data.Either (isLeft)
import           Data.Foldable (toList)
import           Data.List (find)
import           Data.Maybe (isJust, isNothing)
import           Lens.Micro ((&), (.~))
import           System.FilePath ((</>))
import qualified System.Process as Process

import           Testnet.Defaults (defaultGenesisFilepath)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Signal (hardKillProcess)
import           Testnet.Status.Run (checkStatus)
import           Testnet.Status.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

import           RIO (tryAny)

-- | Integration test: a fresh testnet is RUNNING, with a best tip, the era and
-- epoch of the chain, and the mempool and log files of every node. The era is
-- the one of the node, not the one in the manifest. After one relay is killed,
-- it is DEGRADED, and the killed relay is down. After the other nodes are
-- killed too, it is STOPPED, and every node is down.
--
-- Execute with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Status reports network health/"'@
hprop_status :: Property
hprop_status = integrationRetryWorkspace 2 "status" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tmpDir
  TestnetRuntime{testnetNodes} <- createAndRunTestnet def def conf

  running <- H.evalIO $ checkStatus tmpDir 5
  reportStatus running === NetworkRunning
  map probeNodeState (reportNodes running) === map (const NodeOk) (toList testnetNodes)
  H.assertWith (reportBestTip running) isJust
  H.assertWith (map probeBlocksBehind (reportNodes running)) (Just 0 `elem`)
  H.assertWith (map probeBlocksBehind (reportNodes running)) (all isJust)
  H.assertWith (map probeMempool (reportNodes running)) (all isJust)
  chain <- H.evalMaybe $ reportChain running
  chainEra chain === AnyCardanoEra ConwayEra
  -- The testnet has one era, so every epoch has the same length.
  bestTip <- H.evalMaybe $ reportBestTip running
  let epochLength = chainSlotInEpoch chain + chainSlotsToEpochEnd chain
  unEpochNo (chainEpoch chain) * epochLength + chainSlotInEpoch chain === unSlotNo (tipSlot bestTip)
  forM_ (reportNodes running) $ \node -> do
    H.assertFileExists $ tmpDir </> probeStdoutFile node
    H.assertFileExists $ tmpDir </> probeStderrFile node

  -- The era comes from the node: a manifest with another era does not change it.
  let manifestFile = tmpDir </> defaultManifestFile
  manifest <- H.readJsonFileOk manifestFile
  H.lbsWriteFile manifestFile . Aeson.encode $ (manifest :: Value) & key "network" . key "era" .~ String "babbage"
  otherEra <- H.evalIO $ checkStatus tmpDir 5
  fmap chainEra (reportChain otherEra) === Just (AnyCardanoEra ConwayEra)

  relay <- H.evalMaybe $ find (not . isTestnetNodeSpo) testnetNodes
  H.evalIO $ do
    hardKillProcess (nodeProcessHandle relay)
    -- Wait for the process, so that its pid is gone and status can tell "down".
    void . Process.waitForProcess $ nodeProcessHandle relay

  degraded <- H.evalIO $ checkStatus tmpDir 5
  reportStatus degraded === NetworkDegraded
  killed <- H.evalMaybe $ find ((== nodeName relay) . probeNodeName) (reportNodes degraded)
  probeNodeState killed === NodeDown
  H.assertWith (probeError killed) isJust
  probeMempool killed === Nothing
  probeBlocksBehind killed === Nothing

  H.evalIO . forM_ (filter ((/= nodeName relay) . nodeName) $ toList testnetNodes) $ \node -> do
    hardKillProcess (nodeProcessHandle node)
    void . Process.waitForProcess $ nodeProcessHandle node

  stopped <- H.evalIO $ checkStatus tmpDir 5
  reportStatus stopped === NetworkStopped
  map probeNodeState (reportNodes stopped) === map (const NodeDown) (toList testnetNodes)
  reportBestTip stopped === Nothing
  reportChain stopped === Nothing
  H.assertWith (map probeMempool (reportNodes stopped)) (all isNothing)

-- | 'checkStatus' without a running testnet: a directory without a manifest
-- is NO-TESTNET, with a hint about why, and a broken manifest is an error.
--
-- Execute with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Status without a testnet/"'@
hprop_status_no_testnet :: Property
hprop_status_no_testnet = H.propertyOnce . H.workspace "status-no-testnet" $ \dir -> do
  let missingDir = dir </> "missing"
  missing <- H.evalIO $ checkStatus missingDir 1
  reportStatus missing === NoTestnet
  reportProblemExplanation missing === Just DirMissing

  notTestnet <- H.evalIO $ checkStatus dir 1
  reportStatus notTestnet === NoTestnet
  reportProblemExplanation notTestnet === Just NotATestnetDir

  -- The genesis files are written before the manifest, while the testnet starts.
  H.writeFile (dir </> defaultGenesisFilepath ShelleyEra) "{}"
  starting <- H.evalIO $ checkStatus dir 1
  reportStatus starting === NoTestnet
  reportProblemExplanation starting === Just NotReady

  H.writeFile (dir </> defaultManifestFile) "{"
  broken <- H.evalIO . tryAny $ checkStatus dir 1
  H.assertWith broken isLeft
