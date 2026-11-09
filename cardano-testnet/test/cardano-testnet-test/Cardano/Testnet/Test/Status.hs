{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Testnet.Test.Status
  ( hprop_status
  , hprop_status_no_testnet
  ) where

import           Cardano.Api (AnyCardanoEra (..), BlockHeader, BlockNo (..), CardanoEra (..),
                   ChainTip (..), EpochNo (..), Hash, SlotNo (..), deserialiseFromRawBytesHex,
                   getLocalChainTip)

import           Cardano.Testnet (TestnetNode (..), TestnetRuntime (..), createAndRunTestnet,
                   defaultManifestFile, isTestnetNodeSpo, mkConf)

import           Prelude

import           Control.Monad (forM_, void)
import           Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Integral, key)
import           Data.Default.Class (def)
import           Data.Either (isLeft)
import           Data.Foldable (toList)
import           Data.List (find)
import           Data.Maybe (isJust, isNothing)
import           Lens.Micro ((&), (.~), (^?))
import           System.FilePath ((</>))
import qualified System.Process as Process

import           Testnet.Defaults (defaultGenesisFilepath)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Signal (hardKillProcess)
import           Testnet.Status.Run (askOnChain, checkStatus)
import           Testnet.Status.Types
import           Testnet.Types (nodeConnectionInfo)

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

import           RIO (tryAny)

-- | Integration test: a fresh testnet is RUNNING, with a best tip, the era and
-- epoch of the chain, and the mempool and log files of every node. The era is
-- the one of the node, not the one in the manifest. After one relay is killed,
-- it is DEGRADED, and the killed relay is down. After the other nodes are
-- killed too, it is STOPPED, and every node is down. A node tells a block of
-- its chain, also an older one, from a block that is not on its chain.
--
-- Execute with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Status reports network health/"'@
hprop_status :: Property
hprop_status = integrationRetryWorkspace 2 "status" $ \tmpDir -> H.runWithDefaultWatchdog_ $ do
  conf <- mkConf tmpDir
  runtime@TestnetRuntime{testnetNodes} <- createAndRunTestnet def def conf

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
  -- Short forks come and go, so only check that the fork check could tell.
  H.assertWith (map probeOnLongestChain (reportNodes running)) (all isJust)

  -- The answers that the fork check relies on: a node finds a block of its
  -- chain that is deeper than the security parameter, so it is in the node's
  -- immutable database, and it does not find a made-up block.
  shelleyGenesis <- H.readJsonFileOk $ tmpDir </> defaultGenesisFilepath ShelleyEra
  securityParam <- H.evalMaybe $ (shelleyGenesis :: Value) ^? key "securityParam" . _Integral
  connectInfo <- nodeConnectionInfo runtime 0
  oldTip <- H.evalIO $ getLocalChainTip connectInfo
  (oldSlot, oldHash, oldBlockNo) <- case oldTip of
    ChainTip slot hash (BlockNo blockNo) -> pure (slot, hash, blockNo)
    ChainTipAtGenesis -> H.failure
  H.byDurationM 1 120 "the block is deeper than the security parameter" $ do
    tip <- H.evalIO $ getLocalChainTip connectInfo
    H.assertWith tip $ \case
      ChainTip _ _ (BlockNo blockNo) -> blockNo > oldBlockNo + securityParam + 1
      ChainTipAtGenesis -> False
  -- The old block also stands in for the reference node's own tip, which is
  -- asked about first: it stays on the chain.
  let old = TipInfo oldSlot (BlockNo oldBlockNo) oldHash 0
      madeUp = old{tipHash = madeUpHash}
  onChain <- H.evalIO $ askOnChain 5 connectInfo old [Just old, Just madeUp, Nothing]
  onChain === [Just True, Just False, Nothing]

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
  probeOnLongestChain killed === Nothing

  H.evalIO . forM_ (filter ((/= nodeName relay) . nodeName) $ toList testnetNodes) $ \node -> do
    hardKillProcess (nodeProcessHandle node)
    void . Process.waitForProcess $ nodeProcessHandle node

  stopped <- H.evalIO $ checkStatus tmpDir 5
  reportStatus stopped === NetworkStopped
  map probeNodeState (reportNodes stopped) === map (const NodeDown) (toList testnetNodes)
  reportBestTip stopped === Nothing
  reportChain stopped === Nothing
  H.assertWith (map probeMempool (reportNodes stopped)) (all isNothing)
  H.assertWith (map probeOnLongestChain (reportNodes stopped)) (all isNothing)
  where
    madeUpHash :: Hash BlockHeader
    madeUpHash =
      either (\e -> error $ "madeUpHash: bad fixture hash: " <> show e) id $
        deserialiseFromRawBytesHex "0000000000000000000000000000000000000000000000000000000000000000"

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
