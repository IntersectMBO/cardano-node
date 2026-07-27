{-# LANGUAGE ScopedTypeVariables #-}

-- | Adapter from @cardano-config@'s resolved configuration to the node's own
-- 'NodeConfiguration' (the POM one).
--
-- The node currently runs both parsers and will eventually drop the legacy POM
-- parser, at which point @cardano-config@ must produce the 'NodeConfiguration'
-- that starts consensus and networking. This adapter is that eventual
-- replacement: it maps @cardano-config@'s resolved values onto a
-- 'PartialNodeConfiguration' and runs the node's own 'makeNodeConfiguration', so
-- fields cardano-config supplies come from cardano-config and the rest fall back
-- to the node defaults. Fields not yet mapped are listed in 'adapterGaps' — that
-- gap list is exactly what must be closed before POM can be dropped, and any
-- gap also shows up concretely as a divergence in
-- 'Cardano.Node.Configuration.CardanoConfigCompare.compareConfigurations'.
module Cardano.Node.Configuration.CardanoConfigAdapter
  ( cardanoConfigToNodeConfiguration
  , cardanoConfigToPartialNodeConfiguration
  , nodeProtocolConfigurationFromCardanoConfig
  , adapterGaps
  ) where

import           Cardano.Api (File (..))
import qualified Cardano.Configuration as Cfg
import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import           Cardano.Ledger.BaseTypes.NonZero (nonZero)
import           Cardano.Network.ConsensusMode (ConsensusMode (..))
import           Cardano.Network.NodeToNode (DiffusionMode (..))
import           Cardano.Network.PeerSelection (NumberOfBigLedgerPeers (..))
import           Cardano.Node.Configuration.LedgerDB (LedgerDbConfiguration (..),
                   LedgerDbSelectorFlag (..), noDeprecatedOptions)
import           Cardano.Node.Configuration.POM (NodeConfiguration,
                   PartialNodeConfiguration (..), ResponderCoreAffinityPolicy (..),
                   defaultPartialNodeConfiguration, makeNodeConfiguration)
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import           Cardano.Node.Handlers.Shutdown (ShutdownConfig (..),
                   ShutdownOn (..))
import           Cardano.Node.Types (CheckpointsFile (..), CheckpointsHash (..),
                   ConfigYamlFilePath (..), GenesisFile (..),
                   GenesisHash (..), KESSource (..), MaxConcurrencyBulkSync (..),
                   MaxConcurrencyDeadline (..),
                   NodeAlonzoProtocolConfiguration (..),
                   NodeByronProtocolConfiguration (..),
                   NodeCheckpointsConfiguration (..),
                   NodeConwayProtocolConfiguration (..),
                   NodeDijkstraProtocolConfiguration (..),
                   NodeHardForkProtocolConfiguration (..),
                   NodeProtocolConfiguration (..),
                   NodeShelleyProtocolConfiguration (..), ProtocolFilepaths (..),
                   TopologyFile (..))
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import           Cardano.Rpc.Server.Config (RpcConfigF (..))
import           Data.Functor.Identity (runIdentity)
import           Data.Monoid (Last (..))
import           Data.Time.Clock (secondsToDiffTime)
import           Ouroboros.Consensus.Node (NodeDatabasePaths (..))
import           Ouroboros.Consensus.Node.Genesis (GenesisConfigFlags (..),
                   defaultGenesisConfigFlags)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import           Ouroboros.Consensus.Mempool (MempoolCapacityBytesOverride (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Args (QueryBatchSize (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
                   (NumOfDiskSnapshots (..), SnapshotDelayRange (..),
                   SnapshotFrequency (..), SnapshotFrequencyArgs (..),
                   SnapshotPolicyArgs (..), defaultSnapshotPolicyArgs,
                   mithrilSnapshotPolicyArgs)
import           Ouroboros.Consensus.Util.Args (OverrideOrDefault (..))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.TxSubmission.Inbound.V2.Types
                   (TxSubmissionInitDelay (..), TxSubmissionLogicVersion (..))
import           System.FilePath (takeDirectory, (</>))

-- | Build the node's 'NodeConfiguration' from a @cardano-config@-resolved
-- configuration, reusing the node's own 'makeNodeConfiguration'. Fields
-- cardano-config does not yet supply keep the node defaults (see 'adapterGaps').
cardanoConfigToNodeConfiguration :: Cfg.NodeConfiguration -> Either String NodeConfiguration
cardanoConfigToNodeConfiguration =
  makeNodeConfiguration . cardanoConfigToPartialNodeConfiguration

-- | Map the @cardano-config@-resolved values onto a 'PartialNodeConfiguration',
-- overriding the node defaults for every field cardano-config supplies.
cardanoConfigToPartialNodeConfiguration :: Cfg.NodeConfiguration -> PartialNodeConfiguration
cardanoConfigToPartialNodeConfiguration cfg =
    defaultPartialNodeConfiguration
      { pncConfigFile = Last (Just (ConfigYamlFilePath (Cfg.configFilePath cfg)))
      , pncTopologyFile = Last (Just (TopologyFile (Cfg.topologyFile cfg)))
      , pncValidateDB = Last (Just (Cfg.validateDatabase cfg))
      , pncStartAsNonProducingNode = Last (Just (runIdentity (Cfg.startAsNonProducingNode protoCfg)))
      , pncProtocolConfig = Last (Just (nodeProtocolConfigurationFromCardanoConfig cfg))
      , pncProtocolFiles = Last (Just (credentialsToProtocolFilepaths (Cfg.credentials cfg)))
      , pncExperimentalProtocolsEnabled = Last (Just (runIdentity (Cfg.experimentalProtocolsEnabled netCfg)))
      , pncMempoolTimeoutSoft = Last (Just (runIdentity (Cfg.mempoolTimeoutSoft mempCfg)))
      , pncMempoolTimeoutHard = Last (Just (runIdentity (Cfg.mempoolTimeoutHard mempCfg)))
      , pncMempoolTimeoutCapacity = Last (Just (runIdentity (Cfg.mempoolTimeoutCapacity mempCfg)))
      , pncMinBigLedgerPeersForTrustedState =
          Last (Just (NumberOfBigLedgerPeers (runIdentity (Cfg.minBigLedgerPeersForTrustedState netCfg))))
      , -- Peer-selection targets: deadline targets are optional (StrictMaybe),
        -- sync targets are always resolved (Identity). Map them all.
        pncDeadlineTargetOfRootPeers =
          Last (strictMaybeToMaybe (Cfg.deadlineTargetOfRootPeers netCfg))
      , pncDeadlineTargetOfKnownPeers =
          Last (strictMaybeToMaybe (Cfg.deadlineTargetOfKnownPeers netCfg))
      , pncDeadlineTargetOfEstablishedPeers =
          Last (strictMaybeToMaybe (Cfg.deadlineTargetOfEstablishedPeers netCfg))
      , pncDeadlineTargetOfActivePeers =
          Last (strictMaybeToMaybe (Cfg.deadlineTargetOfActivePeers netCfg))
      , pncDeadlineTargetOfKnownBigLedgerPeers =
          Last (strictMaybeToMaybe (Cfg.deadlineTargetOfKnownBigLedgerPeers netCfg))
      , pncDeadlineTargetOfEstablishedBigLedgerPeers =
          Last (strictMaybeToMaybe (Cfg.deadlineTargetOfEstablishedBigLedgerPeers netCfg))
      , pncDeadlineTargetOfActiveBigLedgerPeers =
          Last (strictMaybeToMaybe (Cfg.deadlineTargetOfActiveBigLedgerPeers netCfg))
      , pncSyncTargetOfRootPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfRootPeers netCfg)))
      , pncSyncTargetOfKnownPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfKnownPeers netCfg)))
      , pncSyncTargetOfEstablishedPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfEstablishedPeers netCfg)))
      , pncSyncTargetOfActivePeers =
          Last (Just (runIdentity (Cfg.syncTargetOfActivePeers netCfg)))
      , pncSyncTargetOfKnownBigLedgerPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfKnownBigLedgerPeers netCfg)))
      , pncSyncTargetOfEstablishedBigLedgerPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfEstablishedBigLedgerPeers netCfg)))
      , pncSyncTargetOfActiveBigLedgerPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfActiveBigLedgerPeers netCfg)))
      , pncDatabaseFile = Last (Just (fromCfgDbPaths (runIdentity (Cfg.databasePath storeCfg))))
      , pncDiffusionMode = Last (Just (fromCfgDiffusionMode (runIdentity (Cfg.diffusionMode netCfg))))
      , pncMaxConcurrencyBulkSync =
          Last (Just (MaxConcurrencyBulkSync (runIdentity (Cfg.maxConcurrencyBulkSync netCfg))))
      , pncMaxConcurrencyDeadline =
          Last (Just (MaxConcurrencyDeadline (runIdentity (Cfg.maxConcurrencyDeadline netCfg))))
      , pncTxSubmissionInitDelay =
          Last (Just (TxSubmissionInitDelay (runIdentity (Cfg.txSubmissionInitDelay netCfg))))
      , pncAcceptedConnectionsLimit =
          Last (Just (fromCfgAcceptedConnLimit (runIdentity (Cfg.acceptedConnectionsLimit netCfg))))
      , pncConsensusMode = Last (Just (fromCfgConsensusMode consensusModeVal))
      , pncPeerSharing =
          Last (fmap toPeerSharing (strictMaybeToMaybe (Cfg.peerSharing netCfg)))
      , pncMaybeMempoolCapacityOverride =
          Last (fmap (MempoolCapacityBytesOverride . ByteSize32 . fromIntegral)
                     (strictMaybeToMaybe (Cfg.mempoolCapacityOverride mempCfg)))
      , pncShutdownConfig =
          Last (Just (ShutdownConfig
                        (strictMaybeToMaybe (Cfg.shutdownIPC cfg))
                        (fmap toNodeShutdownOn (strictMaybeToMaybe (Cfg.shutdownOnTarget cfg)))))
      , pncResponderCoreAffinityPolicy =
          Last (Just (fromCfgAffinity (runIdentity (Cfg.responderCoreAffinityPolicy netCfg))))
      , pncTxSubmissionLogicVersion =
          Last (Just (fromCfgTxSubmissionLogic (runIdentity (Cfg.txSubmissionLogicVersion netCfg))))
      , -- The Genesis tuning flags only feed 'ncGenesisConfig' when the node runs
        -- in Genesis mode (see 'makeNodeConfiguration'); in Praos mode the node
        -- ignores them, so mirror POM and keep the defaults there.
        pncGenesisConfigFlags =
          Last (Just (case consensusModeVal of
                        Cfg.GenesisMode flags -> fromCfgGenesisFlags flags
                        Cfg.PraosMode -> defaultGenesisConfigFlags))
      , -- Only the local (IPC) socket path lives in the configuration file; the
        -- node-to-node IPv4/IPv6/port bindings are CLI-only in the node, so they
        -- stay empty here exactly as POM leaves them when parsing config alone.
        pncSocketConfig =
          Last (Just (SocketConfig mempty mempty mempty
                        (Last (fmap File (strictMaybeToMaybe (Cfg.socketPath lcc))))))
      , -- 'nodeSocketPath' (third field) is filled in by 'makeNodeConfiguration'
        -- from the resolved socket config, so leave it empty here.
        pncRpcConfig =
          RpcConfig
            (Last (Just (runIdentity (Cfg.enableGrpc lcc))))
            (Last (fmap File (strictMaybeToMaybe (Cfg.grpcSocketPath lcc))))
            mempty
      , -- Backend selector, query batch size and snapshot policy are all mapped
        -- from cardano-config. 'DeprecatedOptions' has no cardano-config
        -- counterpart (they are the legacy top-level SnapshotInterval /
        -- NumOfDiskSnapshots keys), so it keeps the node's empty default.
        pncLedgerDbConfig =
          Last (Just (LedgerDbConfiguration
                        (fromCfgSnapshotPolicy (strictMaybeToMaybe (Cfg.snapshots ledgerDbCfg)))
                        (maybe DefaultQueryBatchSize RequestedQueryBatchSize
                               (strictMaybeToMaybe (Cfg.queryBatchSize ledgerDbCfg)))
                        (maybe V2InMemory fromCfgBackend
                               (strictMaybeToMaybe (Cfg.backendSelector ledgerDbCfg)))
                        noDeprecatedOptions))
      }
  where
    protoCfg = Cfg.protocolConfiguration cfg
    netCfg = Cfg.networkConfiguration cfg
    mempCfg = Cfg.mempoolConfiguration cfg
    storeCfg = Cfg.storageConfiguration cfg
    lcc = Cfg.localConnectionsConfig cfg
    ledgerDbCfg = runIdentity (Cfg.ledgerDbConfiguration storeCfg)
    consensusModeVal = runIdentity (Cfg.getConsensusConfiguration (Cfg.consensusConfiguration cfg))

    fromCfgDbPaths :: Cfg.NodeDatabasePaths -> NodeDatabasePaths
    fromCfgDbPaths (Cfg.SingleDB p) = OnePathForAllDbs p
    fromCfgDbPaths (Cfg.SplitDB imm vol) = MultipleDbPaths imm vol

    fromCfgDiffusionMode :: Cfg.DiffusionMode -> DiffusionMode
    fromCfgDiffusionMode Cfg.InitiatorOnly = InitiatorOnlyDiffusionMode
    fromCfgDiffusionMode Cfg.InitiatorAndResponder = InitiatorAndResponderDiffusionMode

    fromCfgAcceptedConnLimit :: Cfg.AcceptedConnectionsLimit -> AcceptedConnectionsLimit
    fromCfgAcceptedConnLimit c =
      AcceptedConnectionsLimit
        { acceptedConnectionsHardLimit = Cfg.hardLimit c
        , acceptedConnectionsSoftLimit = Cfg.softLimit c
        , acceptedConnectionsDelay = Cfg.delayOnSoftLimit c
        }

    fromCfgConsensusMode :: Cfg.ConsensusMode -> ConsensusMode
    fromCfgConsensusMode Cfg.PraosMode = PraosMode
    fromCfgConsensusMode (Cfg.GenesisMode _) = GenesisMode

    toPeerSharing :: Bool -> PeerSharing
    toPeerSharing True = PeerSharingEnabled
    toPeerSharing False = PeerSharingDisabled

    toNodeShutdownOn :: Cfg.ShutdownOn -> ShutdownOn
    toNodeShutdownOn (Cfg.ShutdownAtSlot w) = ASlot (SlotNo w)
    toNodeShutdownOn (Cfg.ShutdownAtBlock w) = ABlock (BlockNo w)

    fromCfgAffinity :: Cfg.ResponderCoreAffinityPolicy -> ResponderCoreAffinityPolicy
    fromCfgAffinity Cfg.NoResponderCoreAffinity = NoResponderCoreAffinity
    fromCfgAffinity Cfg.ResponderCoreAffinity = ResponderCoreAffinity

    fromCfgTxSubmissionLogic :: Cfg.TxSubmissionLogicVersion -> TxSubmissionLogicVersion
    fromCfgTxSubmissionLogic Cfg.TxSubmissionLogicV1 = TxSubmissionLogicV1
    fromCfgTxSubmissionLogic Cfg.TxSubmissionLogicV2 = TxSubmissionLogicV2

    -- Map cardano-config's snapshot policy onto the node's 'SnapshotPolicyArgs',
    -- mirroring how POM's LedgerDB parser builds it: a named Mithril policy
    -- selects the predefined 'mithrilSnapshotPolicyArgs', a custom policy is
    -- mapped field-by-field, and absence keeps the node default.
    fromCfgSnapshotPolicy :: Maybe Cfg.SnapshotPolicy -> SnapshotPolicyArgs
    fromCfgSnapshotPolicy Nothing = defaultSnapshotPolicyArgs
    fromCfgSnapshotPolicy (Just Cfg.MithrilSnapshotPolicy) = mithrilSnapshotPolicyArgs
    fromCfgSnapshotPolicy (Just (Cfg.CustomSnapshotPolicy opts)) =
      SnapshotPolicyArgs
        (SnapshotFrequency SnapshotFrequencyArgs
          { sfaInterval =
              maybe UseDefault Override (strictMaybeToMaybe (Cfg.snapshotInterval opts) >>= nonZero)
          , sfaOffset =
              maybe UseDefault (Override . SlotNo) (strictMaybeToMaybe (Cfg.slotOffset opts))
          , sfaRateLimit =
              maybe UseDefault (Override . secondsToDiffTime . fromIntegral)
                    (strictMaybeToMaybe (Cfg.snapshotRateLimit opts))
          , sfaDelaySnapshotRange =
              case (strictMaybeToMaybe (Cfg.minDelay opts), strictMaybeToMaybe (Cfg.maxDelay opts)) of
                (Just mn, Just mx) ->
                  Override (SnapshotDelayRange (secondsToDiffTime (fromIntegral mn))
                                               (secondsToDiffTime (fromIntegral mx)))
                _ -> UseDefault
          })
        (maybe UseDefault (Override . NumOfDiskSnapshots . fromIntegral)
               (strictMaybeToMaybe (Cfg.numOfDiskSnapshots opts)))

    fromCfgBackend :: Cfg.LedgerDbBackendSelector -> LedgerDbSelectorFlag
    fromCfgBackend Cfg.V2InMemory = V2InMemory
    fromCfgBackend (Cfg.V2LSM dbPath exportPath) =
      V2LSM (strictMaybeToMaybe dbPath) (strictMaybeToMaybe exportPath)

    -- cardano-config's 'GenesisConfigFlags' mirrors the node's field-for-field,
    -- except 'gcfCSJJumpSize' is a raw 'Word64' there vs a 'SlotNo' here, and the
    -- optional fields are 'StrictMaybe' vs 'Maybe'.
    fromCfgGenesisFlags :: Cfg.GenesisConfigFlags -> GenesisConfigFlags
    fromCfgGenesisFlags f =
      GenesisConfigFlags
        (Cfg.gcfEnableCSJ f)
        (Cfg.gcfEnableLoEAndGDD f)
        (Cfg.gcfEnableLoP f)
        (strictMaybeToMaybe (Cfg.gcfBlockFetchGracePeriod f))
        (strictMaybeToMaybe (Cfg.gcfBucketCapacity f))
        (strictMaybeToMaybe (Cfg.gcfBucketRate f))
        (fmap SlotNo (strictMaybeToMaybe (Cfg.gcfCSJJumpSize f)))
        (strictMaybeToMaybe (Cfg.gcfGDDRateLimit f))

-- | Map @cardano-config@ 'Cfg.Credentials' (file paths) onto the node's
-- 'ProtocolFilepaths'.
credentialsToProtocolFilepaths :: Cfg.Credentials -> ProtocolFilepaths
credentialsToProtocolFilepaths c =
  ProtocolFilepaths
    { byronCertFile = strictMaybeToMaybe (Cfg.byronDelegationCertificate c)
    , byronKeyFile = strictMaybeToMaybe (Cfg.byronSigningKey c)
    , shelleyKESSource = fmap fromCfgKES (strictMaybeToMaybe (Cfg.shelleyKES c))
    , shelleyVRFFile = strictMaybeToMaybe (Cfg.shelleyVRFKey c)
    , shelleyCertFile = strictMaybeToMaybe (Cfg.shelleyOperationalCertificate c)
    , shelleyBulkCredsFile = strictMaybeToMaybe (Cfg.bulkCredentialsFile c)
    }
 where
  fromCfgKES (Cfg.KESKeyFilePath fp) = KESKeyFilePath fp
  fromCfgKES (Cfg.KESAgentSocketPath fp) = KESAgentSocketPath fp

-- | Build the node's 'NodeProtocolConfiguration' from a @cardano-config@-resolved
-- configuration. Genesis file paths are resolved relative to the configuration
-- file's directory (the way cardano-config resolves them at read time).
nodeProtocolConfigurationFromCardanoConfig ::
  Cfg.NodeConfiguration -> NodeProtocolConfiguration
nodeProtocolConfigurationFromCardanoConfig cfg =
  NodeProtocolConfigurationCardano
    byronConfig
    shelleyConfig
    alonzoConfig
    conwayConfig
    dijkstraConfig
    hardforkConfig
    checkpointsConfig
 where
  protoCfg = Cfg.protocolConfiguration cfg
  testCfg = Cfg.testingConfiguration cfg
  configDir = takeDirectory (Cfg.configFilePath cfg)

  genFile :: Cfg.Hashed FilePath -> GenesisFile
  genFile h = GenesisFile (configDir </> Cfg.hashed h)

  genHash :: Cfg.Hashed FilePath -> Maybe GenesisHash
  genHash h = Just (GenesisHash (Cfg.hash h))

  byronGen = Cfg.byronGenesis protoCfg
  byronConfig =
    NodeByronProtocolConfiguration
      { npcByronGenesisFile = genFile (Cfg.byronGenesisFile byronGen)
      , npcByronGenesisFileHash = genHash (Cfg.byronGenesisFile byronGen)
      , npcByronReqNetworkMagic =
          maybe RequiresNoMagic fromCfgReqNetworkMagic
            (strictMaybeToMaybe (Cfg.byronReqNetworkMagic byronGen))
      , npcByronPbftSignatureThresh = Nothing
      , -- cardano-config does not model the Byron software (block) version. The
        -- Byron era is genesis-only for synthesis (the test configuration
        -- hard-forks to a Shelley-based era at epoch 0), so a fixed default is
        -- used. This surfaces as a divergence against POM (see 'adapterGaps').
        npcByronSupportedProtocolVersionMajor = 1
      , npcByronSupportedProtocolVersionMinor = 0
      , npcByronSupportedProtocolVersionAlt = 0
      }

  shelleyConfig =
    NodeShelleyProtocolConfiguration
      (genFile (Cfg.shelleyGenesis protoCfg))
      (genHash (Cfg.shelleyGenesis protoCfg))
  alonzoConfig =
    NodeAlonzoProtocolConfiguration
      (genFile (Cfg.alonzoGenesis protoCfg))
      (genHash (Cfg.alonzoGenesis protoCfg))
  conwayConfig =
    NodeConwayProtocolConfiguration
      (genFile (Cfg.conwayGenesis protoCfg))
      (genHash (Cfg.conwayGenesis protoCfg))
  dijkstraConfig =
    fmap
      (\h -> NodeDijkstraProtocolConfiguration (genFile h) (genHash h))
      (strictMaybeToMaybe (Cfg.experimentalGenesis testCfg))

  hardforkConfig =
    NodeHardForkProtocolConfiguration
      { npcExperimentalHardForksEnabled = runIdentity (Cfg.experimentalHardForksEnabled testCfg)
      , npcTestShelleyHardForkAtEpoch = epochOf (Cfg.testShelleyHardForkAtEpoch testCfg)
      , npcTestShelleyHardForkAtVersion = strictMaybeToMaybe (Cfg.testShelleyHardForkAtVersion testCfg)
      , npcTestAllegraHardForkAtEpoch = epochOf (Cfg.testAllegraHardForkAtEpoch testCfg)
      , npcTestAllegraHardForkAtVersion = strictMaybeToMaybe (Cfg.testAllegraHardForkAtVersion testCfg)
      , npcTestMaryHardForkAtEpoch = epochOf (Cfg.testMaryHardForkAtEpoch testCfg)
      , npcTestMaryHardForkAtVersion = strictMaybeToMaybe (Cfg.testMaryHardForkAtVersion testCfg)
      , npcTestAlonzoHardForkAtEpoch = epochOf (Cfg.testAlonzoHardForkAtEpoch testCfg)
      , npcTestAlonzoHardForkAtVersion = strictMaybeToMaybe (Cfg.testAlonzoHardForkAtVersion testCfg)
      , npcTestBabbageHardForkAtEpoch = epochOf (Cfg.testBabbageHardForkAtEpoch testCfg)
      , npcTestBabbageHardForkAtVersion = strictMaybeToMaybe (Cfg.testBabbageHardForkAtVersion testCfg)
      , npcTestConwayHardForkAtEpoch = epochOf (Cfg.testConwayHardForkAtEpoch testCfg)
      , npcTestConwayHardForkAtVersion = strictMaybeToMaybe (Cfg.testConwayHardForkAtVersion testCfg)
      , npcTestDijkstraHardForkAtEpoch = epochOf (Cfg.testDijkstraHardForkAtEpoch testCfg)
      , npcTestDijkstraHardForkAtVersion = strictMaybeToMaybe (Cfg.testDijkstraHardForkAtVersion testCfg)
      }

  -- Optional checkpoints file (and hash), path resolved relative to the config
  -- directory like the genesis files above.
  checkpointsConfig =
    case strictMaybeToMaybe (Cfg.checkpointsFile protoCfg) of
      Nothing -> NodeCheckpointsConfiguration Nothing Nothing
      Just mh ->
        NodeCheckpointsConfiguration
          (Just (CheckpointsFile (configDir </> Cfg.maybeHashed mh)))
          (fmap CheckpointsHash (strictMaybeToMaybe (Cfg.maybeHash mh)))

  epochOf = fmap EpochNo . strictMaybeToMaybe

  fromCfgReqNetworkMagic :: Cfg.RequiresNetworkMagic -> RequiresNetworkMagic
  fromCfgReqNetworkMagic Cfg.RequiresNoMagic = RequiresNoMagic
  fromCfgReqNetworkMagic Cfg.RequiresMagic = RequiresMagic

-- | Node 'NodeConfiguration' fields the adapter does not yet populate from
-- @cardano-config@ (they keep the node defaults, so they show up as divergences
-- against POM). Closing these is the remaining work before POM can be dropped.
adapterGaps :: [String]
adapterGaps =
  [ "ncProtocolConfig: Byron supported-protocol-version — genuinely not modelled by"
      <> " cardano-config; hard-coded default (Byron era is genesis-only for synthesis)"
  , "ncTraceForwardSocket — CLI-only in both the node and cardano-config (POM leaves it"
      <> " empty when parsing the config file, filling it only from the command line;"
      <> " cardano-config's tracerSocket is likewise a CLI argument). It is absent from"
      <> " the resolved configuration file, so there is nothing to map and it stays at"
      <> " the node default (empty), exactly as POM does when parsing config alone."
  ]
