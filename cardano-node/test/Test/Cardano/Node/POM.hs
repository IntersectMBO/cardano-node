{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Node.POM
  ( tests
  ) where


import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Cardano.Network.ConsensusMode (ConsensusMode (..))
import           Cardano.Network.Diffusion.Configuration (defaultNumberOfBigLedgerPeers)
import           Cardano.Network.NodeToNode (AcceptedConnectionsLimit (..),
                   DiffusionMode (InitiatorAndResponderDiffusionMode))
import           Cardano.Node.Configuration.LedgerDB
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Types
import           Cardano.Rpc.Server.Config (makeRpcConfig)
import           Cardano.Tracing.Config (PartialTraceOptions (..), defaultPartialTraceConfiguration,
                   partialTraceSelectionToEither)
import           Ouroboros.Consensus.Node (NodeDatabasePaths (..))
import           Ouroboros.Consensus.Node.Genesis (disableGenesisConfig)
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots (NumOfDiskSnapshots (..),
                   SnapshotInterval (..))
import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.TxSubmission.Inbound.V2.Types

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import           Data.Bifunctor (first)
import           Data.Monoid (Last (..))
import           Data.String
import           Data.Text (Text)

import           Hedgehog (Property, discover, withTests, (===))
import qualified Hedgehog
import           Hedgehog.Internal.Property (evalEither, failWith)


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

prop_parseExperimentalHardForksWithoutDijkstraGenesis :: Property
prop_parseExperimentalHardForksWithoutDijkstraGenesis =
  withTests 1 . Hedgehog.property $ do
    partialConfig <- evalEither $ AesonTypes.parseEither Aeson.parseJSON experimentalHardForkConfigWithoutDijkstra
    protocolConfig <- evalEither $ extractProtocolConfig partialConfig
    protocolConfig === testNodeProtocolConfiguration

prop_parseExperimentalHardForksWithDijkstraGenesis :: Property
prop_parseExperimentalHardForksWithDijkstraGenesis =
  withTests 1 . Hedgehog.property $ do
    partialConfig <- evalEither $ AesonTypes.parseEither Aeson.parseJSON experimentalHardForkConfigWithDijkstra
    protocolConfig <- evalEither $ extractProtocolConfig partialConfig
    protocolConfig === expectedProtocolConfigWithDijkstra

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
    { npcExperimentalHardForksEnabled  = True
    , npcTestShelleyHardForkAtEpoch    = Nothing
    , npcTestShelleyHardForkAtVersion  = Nothing
    , npcTestAllegraHardForkAtEpoch    = Nothing
    , npcTestAllegraHardForkAtVersion  = Nothing
    , npcTestMaryHardForkAtEpoch       = Nothing
    , npcTestMaryHardForkAtVersion     = Nothing
    , npcTestAlonzoHardForkAtEpoch     = Nothing
    , npcTestAlonzoHardForkAtVersion   = Nothing
    , npcTestBabbageHardForkAtEpoch    = Nothing
    , npcTestBabbageHardForkAtVersion  = Nothing
    , npcTestConwayHardForkAtEpoch     = Nothing
    , npcTestConwayHardForkAtVersion   = Nothing
    , npcTestDijkstraHardForkAtEpoch   = Nothing
    , npcTestDijkstraHardForkAtVersion = Nothing
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
    Nothing -- Dijkstra configuration
    testNodeHardForkProtocolConfiguration
    testNodeCheckpointsConfiguration

testNodeDijkstraProtocolConfiguration :: NodeDijkstraProtocolConfiguration
testNodeDijkstraProtocolConfiguration =
  NodeDijkstraProtocolConfiguration
    { npcDijkstraGenesisFile = GenesisFile "dummy-dijkstra-genesis-file"
    , npcDijkstraGenesisFileHash = Nothing
    }

expectedProtocolConfigWithDijkstra :: NodeProtocolConfiguration
expectedProtocolConfigWithDijkstra =
  NodeProtocolConfigurationCardano
    testNodeByronProtocolConfiguration
    testNodeShelleyProtocolConfiguration
    testNodeAlonzoProtocolConfiguration
    testNodeConwayProtocolConfiguration
    (Just testNodeDijkstraProtocolConfiguration)
    testNodeHardForkProtocolConfiguration
    testNodeCheckpointsConfiguration

experimentalHardForkConfigWithoutDijkstra :: Aeson.Value
experimentalHardForkConfigWithoutDijkstra =
  Aeson.object
    [ "ByronGenesisFile" Aeson..= ("dummmy-genesis-file" :: FilePath)
    , "ShelleyGenesisFile" Aeson..= ("dummmy-genesis-file" :: FilePath)
    , "AlonzoGenesisFile" Aeson..= ("dummmy-genesis-file" :: FilePath)
    , "ConwayGenesisFile" Aeson..= ("dummmy-genesis-file" :: FilePath)
    , "RequiresNetworkMagic" Aeson..= RequiresNoMagic
    , "LastKnownBlockVersion-Major" Aeson..= (0 :: Int)
    , "LastKnownBlockVersion-Minor" Aeson..= (0 :: Int)
    , "LastKnownBlockVersion-Alt" Aeson..= (0 :: Int)
    , "ExperimentalHardForksEnabled" Aeson..= True
    ]

experimentalHardForkConfigWithDijkstra :: Aeson.Value
experimentalHardForkConfigWithDijkstra =
  Aeson.object
    [ "ByronGenesisFile" Aeson..= ("dummmy-genesis-file" :: FilePath)
    , "ShelleyGenesisFile" Aeson..= ("dummmy-genesis-file" :: FilePath)
    , "AlonzoGenesisFile" Aeson..= ("dummmy-genesis-file" :: FilePath)
    , "ConwayGenesisFile" Aeson..= ("dummmy-genesis-file" :: FilePath)
    , "DijkstraGenesisFile" Aeson..= ("dummy-dijkstra-genesis-file" :: FilePath)
    , "RequiresNetworkMagic" Aeson..= RequiresNoMagic
    , "LastKnownBlockVersion-Major" Aeson..= (0 :: Int)
    , "LastKnownBlockVersion-Minor" Aeson..= (0 :: Int)
    , "LastKnownBlockVersion-Alt" Aeson..= (0 :: Int)
    , "ExperimentalHardForksEnabled" Aeson..= True
    ]

extractProtocolConfig :: PartialNodeConfiguration -> Either Text NodeProtocolConfiguration
extractProtocolConfig partialConfig =
  case pncProtocolConfig partialConfig of
    Last (Just protocolConfig) -> Right protocolConfig
    Last Nothing -> Left "Missing protocol configuration in parsed partial node configuration"

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
    , pncMempoolTimeoutSoft = mempty
    , pncMempoolTimeoutHard = mempty
    , pncMempoolTimeoutCapacity = mempty
    , pncAcceptedConnectionsLimit = mempty
    , pncDeadlineTargetOfRootPeers = mempty
    , pncDeadlineTargetOfKnownPeers = mempty
    , pncDeadlineTargetOfEstablishedPeers = mempty
    , pncDeadlineTargetOfActivePeers = mempty
    , pncDeadlineTargetOfKnownBigLedgerPeers = mempty
    , pncDeadlineTargetOfEstablishedBigLedgerPeers = mempty
    , pncDeadlineTargetOfActiveBigLedgerPeers = mempty
    , pncSyncTargetOfRootPeers = mempty
    , pncSyncTargetOfKnownPeers = mempty
    , pncSyncTargetOfEstablishedPeers = mempty
    , pncSyncTargetOfActivePeers = mempty
    , pncSyncTargetOfKnownBigLedgerPeers = mempty
    , pncSyncTargetOfEstablishedBigLedgerPeers = mempty
    , pncSyncTargetOfActiveBigLedgerPeers = mempty
    , pncMinBigLedgerPeersForTrustedState = mempty
    , pncPeerSharing = Last (Just PeerSharingDisabled)
    , pncConsensusMode = mempty
    , pncGenesisConfigFlags = mempty
    , pncResponderCoreAffinityPolicy = mempty
    , pncLedgerDbConfig = mempty
    , pncEgressPollInterval = mempty
    , pncRpcConfig = mempty
    , pncTxSubmissionLogicVersion = mempty
    , pncTxSubmissionInitDelay = mempty
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
    , pncMempoolTimeoutSoft = mempty
    , pncMempoolTimeoutHard = mempty
    , pncMempoolTimeoutCapacity = mempty
    , pncAcceptedConnectionsLimit = mempty
    , pncDeadlineTargetOfRootPeers = mempty
    , pncDeadlineTargetOfKnownPeers = mempty
    , pncDeadlineTargetOfEstablishedPeers = mempty
    , pncDeadlineTargetOfActivePeers = mempty
    , pncDeadlineTargetOfKnownBigLedgerPeers = mempty
    , pncDeadlineTargetOfEstablishedBigLedgerPeers = mempty
    , pncDeadlineTargetOfActiveBigLedgerPeers = mempty
    , pncSyncTargetOfRootPeers = mempty
    , pncSyncTargetOfKnownPeers = mempty
    , pncSyncTargetOfEstablishedPeers = mempty
    , pncSyncTargetOfActivePeers = mempty
    , pncSyncTargetOfKnownBigLedgerPeers = mempty
    , pncSyncTargetOfEstablishedBigLedgerPeers = mempty
    , pncSyncTargetOfActiveBigLedgerPeers = mempty
    , pncMinBigLedgerPeersForTrustedState = Last (Just defaultNumberOfBigLedgerPeers)
    , pncPeerSharing = Last (Just PeerSharingDisabled)
    , pncConsensusMode = Last (Just PraosMode)
    , pncGenesisConfigFlags = mempty
    , pncResponderCoreAffinityPolicy = mempty
    , pncLedgerDbConfig = mempty
    , pncEgressPollInterval = mempty
    , pncRpcConfig = mempty
    , pncTxSubmissionLogicVersion = mempty
    , pncTxSubmissionInitDelay = mempty
    }

-- | Expected final NodeConfiguration
eExpectedConfig :: Either Text NodeConfiguration
eExpectedConfig = do
  traceOptions <- partialTraceSelectionToEither
                    (return $ PartialTracingOnLegacy defaultPartialTraceConfiguration)
  ncRpcConfig <- first fromString $ makeRpcConfig mempty
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
    , ncEgressPollInterval = 0
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
    , ncMempoolTimeoutSoft = 1.0
    , ncMempoolTimeoutHard = 1.5
    , ncMempoolTimeoutCapacity = 5.0
    , ncAcceptedConnectionsLimit =
        AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit = 512
          , acceptedConnectionsSoftLimit = 384
          , acceptedConnectionsDelay     = 5
          }
    , ncDeadlineTargetOfRootPeers = 60
    , ncDeadlineTargetOfKnownPeers = 150
    , ncDeadlineTargetOfEstablishedPeers = 30
    , ncDeadlineTargetOfActivePeers = 20
    , ncDeadlineTargetOfKnownBigLedgerPeers = 15
    , ncDeadlineTargetOfEstablishedBigLedgerPeers = 10
    , ncDeadlineTargetOfActiveBigLedgerPeers = 5
    , ncSyncTargetOfRootPeers = 0
    , ncSyncTargetOfKnownPeers = 150
    , ncSyncTargetOfEstablishedPeers = 10
    , ncSyncTargetOfActivePeers = 5
    , ncSyncTargetOfKnownBigLedgerPeers = 100
    , ncSyncTargetOfEstablishedBigLedgerPeers = 40
    , ncSyncTargetOfActiveBigLedgerPeers = 30
    , ncMinBigLedgerPeersForTrustedState = defaultNumberOfBigLedgerPeers
    , ncPeerSharing = PeerSharingDisabled
    , ncConsensusMode = PraosMode
    , ncGenesisConfig = disableGenesisConfig
    , ncResponderCoreAffinityPolicy = NoResponderCoreAffinity
    , ncLedgerDbConfig = LedgerDbConfiguration DefaultNumOfDiskSnapshots DefaultSnapshotInterval DefaultQueryBatchSize V2InMemory noDeprecatedOptions
    , ncRpcConfig
    , ncTxSubmissionLogicVersion = TxSubmissionLogicV1
    , ncTxSubmissionInitDelay = defaultTxSubmissionInitDelay
    }

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
