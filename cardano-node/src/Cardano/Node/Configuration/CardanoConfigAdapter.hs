{-# LANGUAGE ScopedTypeVariables #-}

-- | Adapter from @cardano-config@'s resolved configuration to the node's own
-- 'NodeConfiguration' (the POM one).
--
-- It maps @cardano-config@'s resolved values onto a 'PartialNodeConfiguration'
-- and runs the node's own 'makeNodeConfiguration', so fields cardano-config
-- supplies come from cardano-config and the rest fall back to the node defaults.
--
-- This is what the node runs on for a @cardano-config@ envelope configuration —
-- the only parser that can read one — and what the legacy configuration's
-- cross-check is compared against; see
-- 'Cardano.Node.Configuration.CardanoConfigResolve'.
--
-- Fields not mapped are listed in 'adapterGaps' — that gap list is exactly what
-- must be closed before the POM parser can be dropped, and every gap also shows
-- up concretely as a divergence in
-- 'Cardano.Node.Configuration.CardanoConfigCompare.compareConfigurations'.
module Cardano.Node.Configuration.CardanoConfigAdapter
  ( cardanoConfigToNodeConfiguration
  , cardanoConfigToPartialNodeConfiguration
  , nodeProtocolConfigurationFromCardanoConfig
  , adapterGaps
  ) where

import           Cardano.Api (File (..))
import qualified Cardano.Configuration as Cfg
import qualified Cardano.Configuration.CliArgs as CliArgs
import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import           Cardano.Ledger.BaseTypes.NonZero (nonZero)
import           Cardano.Network.ConsensusMode (ConsensusMode (..))
import           Cardano.Network.PeerSelection (NumberOfBigLedgerPeers (..))
import           Cardano.Logging.Types (ForwarderMode (..), HowToConnect (..))
import           Cardano.Node.Configuration.LedgerDB (LedgerDbConfiguration (..),
                   LedgerDbSelectorFlag (..), noDeprecatedOptions)
import           Cardano.Node.Configuration.NodeAddress (NodeHostIPv4Address (..),
                   NodeHostIPv6Address (..))
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
import           Cardano.Rpc.Server.Config (RpcConfigF (..), RpcEndpoint (..),
                   RpcTlsFiles (..))
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
                   SnapshotInterval (..), SnapshotPolicyArgs (..),
                   defaultSnapshotPolicyArgs)
import           Ouroboros.Network.TxSubmission.Inbound.V2.Types
                   (TxSubmissionInitDelay (..))
import           System.FilePath (takeDirectory, (</>))

-- | Build the node's 'NodeConfiguration' from a @cardano-config@-resolved
-- configuration, reusing the node's own 'makeNodeConfiguration'. Fields
-- cardano-config does not yet supply keep the node defaults (see 'adapterGaps').
cardanoConfigToNodeConfiguration :: Cfg.NodeConfiguration -> Either String NodeConfiguration
cardanoConfigToNodeConfiguration =
  makeNodeConfiguration . cardanoConfigToPartialNodeConfiguration

-- | Map the @cardano-config@-resolved values onto a 'PartialNodeConfiguration',
-- overriding the node defaults for every field cardano-config supplies.
--
-- Several of the types are now shared rather than mirrored: @cardano-config@
-- carries @ouroboros-network@'s own 'Cfg.DiffusionMode', 'Cfg.PeerSharing',
-- 'Cfg.AcceptedConnectionsLimit' and 'Cfg.TxSubmissionLogicVersion', which are
-- exactly the types the node's 'PartialNodeConfiguration' holds, so those fields
-- pass through with no conversion.
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
      , -- The deadline targets have no always-applied default (the two shipped
        -- role configurations set them, but a configuration may state none), so
        -- they stay optional on both sides and an unset one keeps the node
        -- default.
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
      , pncSyncTargetOfRootPeers = Last (Just (runIdentity (Cfg.syncTargetOfRootPeers netCfg)))
      , pncSyncTargetOfKnownPeers = Last (Just (runIdentity (Cfg.syncTargetOfKnownPeers netCfg)))
      , pncSyncTargetOfEstablishedPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfEstablishedPeers netCfg)))
      , pncSyncTargetOfActivePeers = Last (Just (runIdentity (Cfg.syncTargetOfActivePeers netCfg)))
      , pncSyncTargetOfKnownBigLedgerPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfKnownBigLedgerPeers netCfg)))
      , pncSyncTargetOfEstablishedBigLedgerPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfEstablishedBigLedgerPeers netCfg)))
      , pncSyncTargetOfActiveBigLedgerPeers =
          Last (Just (runIdentity (Cfg.syncTargetOfActiveBigLedgerPeers netCfg)))
      , pncDatabaseFile = Last (Just (fromCfgDbPaths (runIdentity (Cfg.databasePath storeCfg))))
      , pncDiffusionMode = Last (Just (runIdentity (Cfg.diffusionMode netCfg)))
      , pncMaxConcurrencyBulkSync =
          Last (Just (MaxConcurrencyBulkSync (runIdentity (Cfg.maxConcurrencyBulkSync netCfg))))
      , pncMaxConcurrencyDeadline =
          Last (Just (MaxConcurrencyDeadline (runIdentity (Cfg.maxConcurrencyDeadline netCfg))))
      , pncProtocolIdleTimeout = Last (Just (runIdentity (Cfg.protocolIdleTimeout netCfg)))
      , pncTimeWaitTimeout = Last (Just (runIdentity (Cfg.timeWaitTimeout netCfg)))
      , pncEgressPollInterval = Last (Just (runIdentity (Cfg.egressPollInterval netCfg)))
      , pncChainSyncIdleTimeout = Last (Just (runIdentity (Cfg.chainSyncIdleTimeout netCfg)))
      , pncTxSubmissionInitDelay =
          Last (Just (TxSubmissionInitDelay (runIdentity (Cfg.txSubmissionInitDelay netCfg))))
      , pncAcceptedConnectionsLimit = Last (Just (Cfg.acceptedConnectionsLimitOf netCfg))
      , pncConsensusMode = Last (Just (fromCfgConsensusMode consensusModeVal))
      , pncPeerSharing = Last (strictMaybeToMaybe (Cfg.peerSharing netCfg))
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
          Last (Just (runIdentity (Cfg.txSubmissionLogicVersion netCfg)))
      , -- The Genesis tuning flags only feed 'ncGenesisConfig' when the node runs
        -- in Genesis mode (see 'makeNodeConfiguration'); in Praos mode the node
        -- ignores them, so mirror POM and keep the defaults there.
        pncGenesisConfigFlags =
          Last (Just (case consensusModeVal of
                        Cfg.GenesisMode flags -> fromCfgGenesisFlags flags
                        Cfg.PraosMode -> defaultGenesisConfigFlags))
      , -- The local (IPC) socket path comes from the configuration file or the
        -- command line; the node-to-node IPv4/IPv6/port bindings are CLI-only in
        -- both parsers, and cardano-config carries them on the resolved
        -- configuration, so they are mapped here too.
        pncSocketConfig =
          Last (Just (SocketConfig
                        (Last (fmap NodeHostIPv4Address (strictMaybeToMaybe (Cfg.hostAddr cfg))))
                        (Last (fmap NodeHostIPv6Address (strictMaybeToMaybe (Cfg.hostIPv6Addr cfg))))
                        (Last (strictMaybeToMaybe (Cfg.port cfg)))
                        (Last (fmap File (strictMaybeToMaybe (Cfg.socketPath lcc))))))
      , pncTraceForwardSocket =
          Last (fmap fromCfgTracerConnection (strictMaybeToMaybe (Cfg.tracerSocket cfg)))
      , -- 'nodeSocketPath' is filled in by 'makeNodeConfiguration' from the
        -- resolved socket config, so leave it empty here.
        pncRpcConfig =
          RpcConfig
            { isEnabled = Last (Just (runIdentity (Cfg.enableGrpc lcc)))
            , rpcEndpoint =
                Last (fmap fromCfgGrpcEndpoint (strictMaybeToMaybe (Cfg.grpcEndpoint lcc)))
            , nodeSocketPath = mempty
            }
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

    fromCfgConsensusMode :: Cfg.ConsensusMode -> ConsensusMode
    fromCfgConsensusMode Cfg.PraosMode = PraosMode
    fromCfgConsensusMode (Cfg.GenesisMode _) = GenesisMode

    toNodeShutdownOn :: Cfg.ShutdownOn -> ShutdownOn
    toNodeShutdownOn (Cfg.ShutdownAtSlot w) = ASlot (SlotNo w)
    toNodeShutdownOn (Cfg.ShutdownAtBlock w) = ABlock (BlockNo w)

    fromCfgAffinity :: Cfg.ResponderCoreAffinityPolicy -> ResponderCoreAffinityPolicy
    fromCfgAffinity Cfg.NoResponderCoreAffinity = NoResponderCoreAffinity
    fromCfgAffinity Cfg.ResponderCoreAffinity = ResponderCoreAffinity

    -- cardano-config's gRPC endpoint and the node's 'RpcEndpoint' offer the same
    -- three choices, so the mapping is one constructor to one constructor. Both
    -- default the listen address to 127.0.0.1, and cardano-config has already
    -- applied that default, so the endpoint arrives complete.
    fromCfgGrpcEndpoint :: Cfg.GrpcEndpoint -> RpcEndpoint
    fromCfgGrpcEndpoint (Cfg.GrpcEndpointUnixSocket fp) = RpcEndpointUnixSocket (File fp)
    fromCfgGrpcEndpoint (Cfg.GrpcEndpointHttp ip portNo) = RpcEndpointHttp ip portNo
    fromCfgGrpcEndpoint (Cfg.GrpcEndpointHttps ip portNo tls) =
      RpcEndpointHttps ip portNo (fromCfgGrpcTlsFiles tls)

    fromCfgGrpcTlsFiles :: Cfg.GrpcTlsFiles -> RpcTlsFiles
    fromCfgGrpcTlsFiles tls =
      RpcTlsFiles
        { certificateFile = File (Cfg.certificateFile tls)
        , privateKeyFile = File (Cfg.privateKeyFile tls)
        , chainCertificateFiles = map File (Cfg.chainCertificateFiles tls)
        }

    -- cardano-config records the socket mode as the literal @"Accept"@ /
    -- @"Connect"@ its CLI parser produces; anything else cannot occur.
    fromCfgTracerConnection :: Cfg.TracerConnection -> (HowToConnect, ForwarderMode)
    fromCfgTracerConnection (Cfg.TracerConnection mode method) =
      ( case method of
          CliArgs.TracerConnectViaPipe fp -> LocalPipe fp
          CliArgs.TracerConnectViaRemote host portNo ->
            RemoteSocket host (fromIntegral portNo)
      , case mode of
          "Accept" -> Responder
          _ -> Initiator
      )

    -- Map cardano-config's snapshot policy onto the node's 'SnapshotPolicyArgs'.
    -- Resolution has already turned a named Mithril policy into the concrete
    -- options it stands for and filled in every option a partial policy left out
    -- ('Cfg.resolveSnapshotPolicy'), so the policy reaching here states all six
    -- values; 'Cfg.resolveSnapshotPolicy' is applied again to be independent of
    -- that, and absence keeps the node default.
    fromCfgSnapshotPolicy :: Maybe Cfg.SnapshotPolicy -> SnapshotPolicyArgs
    fromCfgSnapshotPolicy Nothing = defaultSnapshotPolicyArgs
    fromCfgSnapshotPolicy (Just policy) =
      SnapshotPolicyArgs
        (SnapshotFrequency SnapshotFrequencyArgs
          { sfaInterval =
              maybe (sfaInterval defaultFrequencyArgs) RequestedSnapshotInterval
                    (strictMaybeToMaybe (Cfg.snapshotInterval opts) >>= nonZero)
          , sfaOffset =
              maybe (sfaOffset defaultFrequencyArgs) SlotNo
                    (strictMaybeToMaybe (Cfg.slotOffset opts))
          , sfaRateLimit =
              maybe (sfaRateLimit defaultFrequencyArgs) (secondsToDiffTime . fromIntegral)
                    (strictMaybeToMaybe (Cfg.snapshotRateLimit opts))
          , sfaDelaySnapshotRange =
              case (strictMaybeToMaybe (Cfg.minDelay opts), strictMaybeToMaybe (Cfg.maxDelay opts)) of
                (Just mn, Just mx) ->
                  SnapshotDelayRange (secondsToDiffTime (fromIntegral mn))
                                     (secondsToDiffTime (fromIntegral mx))
                _ -> sfaDelaySnapshotRange defaultFrequencyArgs
          })
        (maybe (spaNum defaultSnapshotPolicyArgs) (NumOfDiskSnapshots . fromIntegral)
               (strictMaybeToMaybe (Cfg.numOfDiskSnapshots opts)))
     where
      opts = Cfg.resolveSnapshotPolicy policy

    -- A snapshot option the configuration leaves unset keeps the node's own
    -- default for that field, exactly as POM's LedgerDB parser does.
    defaultFrequencyArgs = case spaFrequency defaultSnapshotPolicyArgs of
      SnapshotFrequency sfa -> sfa
      DisableSnapshots ->
        error "defaultSnapshotPolicyArgs unexpectedly disables snapshots"

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
-- configuration. Genesis and checkpoints file paths are resolved relative to the
-- configuration file's directory (the way cardano-config resolves them at read
-- time, and the way POM's 'AdjustFilePaths' does).
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
      , -- cardano-config does not model the Byron PBFT signature threshold or
        -- the Byron software (block) version: @PBftSignatureThreshold@ and the
        -- @LastKnownBlockVersion-*@ trio are among the keys @migrate@ drops, as
        -- they now come from consensus defaults rather than configuration. Fixed
        -- defaults are used here (see 'adapterGaps'). A different
        -- @PBftSignatureThreshold@ surfaces as a divergence against POM; the
        -- version trio does not, as the comparison leaves it out (see
        -- 'Cardano.Node.Configuration.CardanoConfigCompare.compareProtocol').
        npcByronPbftSignatureThresh = Nothing
      , npcByronSupportedProtocolVersionMajor = 1
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

  -- The checkpoints file is optional, and so is its hash once the file is given
  -- ('Cfg.MaybeHashed'), which is exactly the node's own shape.
  checkpointsConfig =
    case strictMaybeToMaybe (Cfg.checkpointsFile protoCfg) of
      Nothing -> NodeCheckpointsConfiguration Nothing Nothing
      Just h ->
        NodeCheckpointsConfiguration
          (Just (CheckpointsFile (configDir </> Cfg.maybeHashed h)))
          (fmap CheckpointsHash (strictMaybeToMaybe (Cfg.maybeHash h)))

  epochOf = fmap EpochNo . strictMaybeToMaybe

  fromCfgReqNetworkMagic :: Cfg.RequiresNetworkMagic -> RequiresNetworkMagic
  fromCfgReqNetworkMagic Cfg.RequiresNoMagic = RequiresNoMagic
  fromCfgReqNetworkMagic Cfg.RequiresMagic = RequiresMagic

-- | Node 'NodeConfiguration' fields the adapter does not yet populate from
-- @cardano-config@ (they keep the node defaults, so they show up as divergences
-- against POM). Closing these is the remaining work before POM can be dropped.
adapterGaps :: [String]
adapterGaps =
  [ "ncProtocolConfig: Byron PBFT signature threshold and supported-protocol-version"
      <> " — the PBftSignatureThreshold and LastKnownBlockVersion-* keys are deliberately"
      <> " not modelled by cardano-config (they now come from consensus defaults), so"
      <> " Nothing and a fixed 1/0/0 are used here"
  ]
