{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.POM
  ( tests
  ) where


import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Cardano.Node.Configuration.LedgerDB
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Types
import           Cardano.Tracing.Config (PartialTraceOptions (..), defaultPartialTraceConfiguration,
                   partialTraceSelectionToEither)
import           Ouroboros.Consensus.Node (NodeDatabasePaths (..))
import qualified Ouroboros.Consensus.Node as Consensus (NetworkP2PMode (..))
import           Ouroboros.Consensus.Node.Genesis (disableGenesisConfig)
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots (NumOfDiskSnapshots (..),
                   SnapshotInterval (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Diffusion.Configuration (ConsensusMode (..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..),
                   DiffusionMode (InitiatorAndResponderDiffusionMode))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))

import           Data.Monoid (Last (..))
import           Data.Text (Text)

import           Hedgehog (Property, discover, withTests, (===))
import qualified Hedgehog
import           Hedgehog.Internal.Property (evalEither, failWith)
import Ouroboros.Cardano.Network.Diffusion.Configuration (defaultNumberOfBigLedgerPeers)


-- This is a simple test to check that the POM technique is working as intended.
-- What is entered on the command line via the cli takes precedence and this is
-- tested in the property below.
-- See: https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67

prop_sanityCheck_POM :: Property
prop_sanityCheck_POM =
   withTests 1 . Hedgehog.property $ do
    let combinedPartials = defaultPartialNodeConfiguration
                             <> testPartialYamlConfig
                             <> testPartialCliConfig
        nc = makeNodeConfiguration combinedPartials
    expectedConfig <- evalEither eExpectedConfig
    case nc of
      Left err -> failWith Nothing $ "Partial Options Monoid sanity check failure: " <> err
      Right config -> config === expectedConfig

testNodeByronProtocolConfiguration :: NodeByronProtocolConfiguration
testNodeByronProtocolConfiguration =
  NodeByronProtocolConfiguration
    { npcByronGenesisFile                   = GenesisFile "dummmy-genesis-file"
    , npcByronGenesisFileHash               = Nothing
    , npcByronReqNetworkMagic               = RequiresNoMagic
    , npcByronPbftSignatureThresh           = Nothing
    , npcByronSupportedProtocolVersionMajor = 0
    , npcByronSupportedProtocolVersionMinor = 0
    , npcByronSupportedProtocolVersionAlt   = 0
    }

testNodeShelleyProtocolConfiguration :: NodeShelleyProtocolConfiguration
testNodeShelleyProtocolConfiguration =
  NodeShelleyProtocolConfiguration
    { npcShelleyGenesisFile     = GenesisFile "dummmy-genesis-file"
    , npcShelleyGenesisFileHash = Nothing
    }

testNodeAlonzoProtocolConfiguration :: NodeAlonzoProtocolConfiguration
testNodeAlonzoProtocolConfiguration =
  NodeAlonzoProtocolConfiguration
    { npcAlonzoGenesisFile      = GenesisFile "dummmy-genesis-file"
    , npcAlonzoGenesisFileHash  = Nothing
    }

testNodeConwayProtocolConfiguration :: NodeConwayProtocolConfiguration
testNodeConwayProtocolConfiguration =
  NodeConwayProtocolConfiguration
    { npcConwayGenesisFile      = GenesisFile "dummmy-genesis-file"
    , npcConwayGenesisFileHash  = Nothing
    }

testNodeHardForkProtocolConfiguration :: NodeHardForkProtocolConfiguration
testNodeHardForkProtocolConfiguration =
  NodeHardForkProtocolConfiguration
    { npcExperimentalHardForksEnabled = True
    , npcTestShelleyHardForkAtEpoch   = Nothing
    , npcTestShelleyHardForkAtVersion = Nothing
    , npcTestAllegraHardForkAtEpoch   = Nothing
    , npcTestAllegraHardForkAtVersion = Nothing
    , npcTestMaryHardForkAtEpoch      = Nothing
    , npcTestMaryHardForkAtVersion    = Nothing
    , npcTestAlonzoHardForkAtEpoch    = Nothing
    , npcTestAlonzoHardForkAtVersion  = Nothing
    , npcTestBabbageHardForkAtEpoch   = Nothing
    , npcTestBabbageHardForkAtVersion = Nothing
    , npcTestConwayHardForkAtEpoch    = Nothing
    , npcTestConwayHardForkAtVersion  = Nothing
    }

testNodeCheckpointsConfiguration :: NodeCheckpointsConfiguration
testNodeCheckpointsConfiguration =
  NodeCheckpointsConfiguration
    { npcCheckpointsFile     = Nothing
    , npcCheckpointsFileHash = Nothing
    }

testNodeProtocolConfiguration :: NodeProtocolConfiguration
testNodeProtocolConfiguration =
  NodeProtocolConfigurationCardano
    testNodeByronProtocolConfiguration
    testNodeShelleyProtocolConfiguration
    testNodeAlonzoProtocolConfiguration
    testNodeConwayProtocolConfiguration
    testNodeHardForkProtocolConfiguration
    testNodeCheckpointsConfiguration

-- | Example partial configuration theoretically created from a
-- config yaml file.
testPartialYamlConfig :: PartialNodeConfiguration
testPartialYamlConfig =
  PartialNodeConfiguration
    { pncProtocolConfig = Last $ Just testNodeProtocolConfiguration
    , pncSocketConfig = Last . Just $ SocketConfig (Last Nothing) mempty mempty mempty
    , pncShutdownConfig = Last Nothing
    , pncStartAsNonProducingNode = Last $ Just False
    , pncDiffusionMode = Last Nothing
    , pncExperimentalProtocolsEnabled = Last Nothing
    , pncMaxConcurrencyBulkSync = Last Nothing
    , pncMaxConcurrencyDeadline = Last Nothing
    , pncLoggingSwitch = Last $ Just True
    , pncLogMetrics = Last $ Just True
    , pncTraceConfig = Last (Just $ PartialTracingOnLegacy defaultPartialTraceConfiguration)
    , pncTraceForwardSocket = Last Nothing
    , pncConfigFile = mempty
    , pncTopologyFile = mempty
    , pncDatabaseFile = mempty
    , pncProtocolFiles = mempty
    , pncValidateDB = mempty
    , pncMaybeMempoolCapacityOverride = mempty
    , pncProtocolIdleTimeout = mempty
    , pncTimeWaitTimeout = mempty
    , pncChainSyncIdleTimeout = mempty
    , pncAcceptedConnectionsLimit = mempty
    , pncDeadlineTargetOfRootPeers = mempty
    , pncDeadlineTargetOfKnownPeers = mempty
    , pncDeadlineTargetOfEstablishedPeers = mempty
    , pncDeadlineTargetOfActivePeers = mempty
    , pncDeadlineTargetOfKnownBigLedgerPeers = mempty
    , pncDeadlineTargetOfEstablishedBigLedgerPeers = mempty
    , pncDeadlineTargetOfActiveBigLedgerPeers = mempty
    , pncSyncTargetOfActivePeers = mempty
    , pncSyncTargetOfKnownBigLedgerPeers = mempty
    , pncSyncTargetOfEstablishedBigLedgerPeers = mempty
    , pncSyncTargetOfActiveBigLedgerPeers = mempty
    , pncMinBigLedgerPeersForTrustedState = mempty
    , pncEnableP2P = Last (Just DisabledP2PMode)
    , pncPeerSharing = Last (Just PeerSharingDisabled)
    , pncConsensusMode = mempty
    , pncGenesisConfigFlags = mempty
    , pncResponderCoreAffinityPolicy = mempty
    , pncLedgerDbConfig = mempty
    }

-- | Example partial configuration theoretically created
-- from what was parsed on the command line.
testPartialCliConfig :: PartialNodeConfiguration
testPartialCliConfig =
  PartialNodeConfiguration
    { pncSocketConfig = Last . Just $ SocketConfig mempty mempty mempty mempty
    , pncShutdownConfig = Last . Just $ ShutdownConfig Nothing (Just . ASlot $ SlotNo 42)
    , pncStartAsNonProducingNode = Last $ Just False
    , pncConfigFile   = mempty
    , pncTopologyFile = mempty
    , pncDatabaseFile = mempty
    , pncDiffusionMode = mempty
    , pncExperimentalProtocolsEnabled = Last $ Just True
    , pncProtocolFiles = Last . Just $ ProtocolFilepaths Nothing Nothing Nothing Nothing Nothing Nothing
    , pncValidateDB = Last $ Just True
    , pncProtocolConfig = mempty
    , pncMaxConcurrencyBulkSync = mempty
    , pncMaxConcurrencyDeadline = mempty
    , pncLoggingSwitch = mempty
    , pncLogMetrics = mempty
    , pncTraceConfig = Last (Just $ PartialTracingOnLegacy defaultPartialTraceConfiguration)
    , pncTraceForwardSocket = mempty
    , pncMaybeMempoolCapacityOverride = mempty
    , pncProtocolIdleTimeout = mempty
    , pncTimeWaitTimeout = mempty
    , pncChainSyncIdleTimeout = mempty
    , pncAcceptedConnectionsLimit = mempty
    , pncDeadlineTargetOfRootPeers = mempty
    , pncDeadlineTargetOfKnownPeers = mempty
    , pncDeadlineTargetOfEstablishedPeers = mempty
    , pncDeadlineTargetOfActivePeers = mempty
    , pncDeadlineTargetOfKnownBigLedgerPeers = mempty
    , pncDeadlineTargetOfEstablishedBigLedgerPeers = mempty
    , pncDeadlineTargetOfActiveBigLedgerPeers = mempty
    , pncSyncTargetOfActivePeers = mempty
    , pncSyncTargetOfKnownBigLedgerPeers = mempty
    , pncSyncTargetOfEstablishedBigLedgerPeers = mempty
    , pncSyncTargetOfActiveBigLedgerPeers = mempty
    , pncMinBigLedgerPeersForTrustedState = Last (Just defaultNumberOfBigLedgerPeers)
    , pncEnableP2P = Last (Just DisabledP2PMode)
    , pncPeerSharing = Last (Just PeerSharingDisabled)
    , pncConsensusMode = Last (Just PraosMode)
    , pncGenesisConfigFlags = mempty
    , pncResponderCoreAffinityPolicy = mempty
    , pncLedgerDbConfig = mempty
    }

-- | Expected final NodeConfiguration
eExpectedConfig :: Either Text NodeConfiguration
eExpectedConfig = do
  traceOptions <- partialTraceSelectionToEither
                    (return $ PartialTracingOnLegacy defaultPartialTraceConfiguration)
  return $ NodeConfiguration
    { ncSocketConfig = SocketConfig mempty mempty mempty mempty
    , ncShutdownConfig = ShutdownConfig Nothing (Just . ASlot $ SlotNo 42)
    , ncStartAsNonProducingNode = False
    , ncConfigFile = ConfigYamlFilePath "configuration/cardano/mainnet-config.json"
    , ncTopologyFile = TopologyFile "configuration/cardano/mainnet-topology.json"
    , ncDatabaseFile = OnePathForAllDbs "mainnet/db/"
    , ncProtocolFiles = ProtocolFilepaths Nothing Nothing Nothing Nothing Nothing Nothing
    , ncValidateDB = True
    , ncProtocolConfig = testNodeProtocolConfiguration
    , ncDiffusionMode = InitiatorAndResponderDiffusionMode
    , ncExperimentalProtocolsEnabled = True
    , ncMaxConcurrencyBulkSync = Nothing
    , ncMaxConcurrencyDeadline = Nothing
    , ncLoggingSwitch = True
    , ncLogMetrics = True
    , ncTraceConfig = traceOptions
    , ncTraceForwardSocket = Nothing
    , ncMaybeMempoolCapacityOverride = Nothing
    , ncProtocolIdleTimeout = 5
    , ncTimeWaitTimeout = 60
    , ncChainSyncIdleTimeout = NoTimeoutOverride
    , ncAcceptedConnectionsLimit =
        AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit = 512
          , acceptedConnectionsSoftLimit = 384
          , acceptedConnectionsDelay     = 5
          }
    , ncDeadlineTargetOfRootPeers = 60
    , ncDeadlineTargetOfKnownPeers = 85
    , ncDeadlineTargetOfEstablishedPeers = 40
    , ncDeadlineTargetOfActivePeers = 15
    , ncDeadlineTargetOfKnownBigLedgerPeers = 15
    , ncDeadlineTargetOfEstablishedBigLedgerPeers = 10
    , ncDeadlineTargetOfActiveBigLedgerPeers = 5
    , ncSyncTargetOfActivePeers = 0
    , ncSyncTargetOfKnownBigLedgerPeers = 100
    , ncSyncTargetOfEstablishedBigLedgerPeers = 50
    , ncSyncTargetOfActiveBigLedgerPeers = 30
    , ncMinBigLedgerPeersForTrustedState = defaultNumberOfBigLedgerPeers
    , ncEnableP2P = SomeNetworkP2PMode Consensus.DisabledP2PMode
    , ncPeerSharing = PeerSharingDisabled
    , ncConsensusMode = PraosMode
    , ncGenesisConfig = disableGenesisConfig
    , ncResponderCoreAffinityPolicy = NoResponderCoreAffinity
    , ncLedgerDbConfig = LedgerDbConfiguration DefaultNumOfDiskSnapshots DefaultSnapshotInterval DefaultQueryBatchSize V2InMemory noDeprecatedOptions
    }

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
