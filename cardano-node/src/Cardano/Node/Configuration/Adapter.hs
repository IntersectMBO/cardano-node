{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Adapter from the @cardano-config@ package's resolved configuration
-- ('CC.NodeConfiguration') to the node's existing 'PartialNodeConfiguration'.
--
-- The intent is that the node's entrypoint becomes:
--
-- @
--   file <- CC.parseConfigurationFiles (CC.configFilePath cli)
--   let ccnc = CC.resolveConfiguration cli file
--       cfgDir = takeDirectory (CC.configFilePath cli)
--   makeNodeConfiguration (defaultPartialNodeConfiguration <> fromCardanoConfig cfgDir ccnc)
-- @
--
-- We deliberately target 'PartialNodeConfiguration' (not the fully-resolved
-- 'NodeConfiguration') so that all of the node's defaulting and validation
-- logic in 'makeNodeConfiguration' is reused verbatim. Whenever
-- @cardano-config@ has no value for a field we leave it as 'mempty' / @Last
-- Nothing@ so the node default applies.
--
-- NOTE on file paths: genesis and checkpoint files are always sourced from the
-- configuration file and, as in the legacy parser, are resolved relative to the
-- configuration file's directory (the @configDir@ argument). Credential,
-- database and socket paths come (mostly) from the CLI and are left as given,
-- i.e. relative to the current working directory — again matching the legacy
-- 'parseNodeConfigurationFP' / 'AdjustFilePaths' behaviour.
module Cardano.Node.Configuration.Adapter
  ( fromCardanoConfig
  , nodeConfigurationFromCli
  , nodeConfigurationFromFile
  , cliArgsFromConfigFile
  ) where

import qualified Cardano.Configuration as CC
import qualified Cardano.Configuration.CliArgs as CCCli
import qualified Cardano.Configuration.File.Consensus as CCCon
import qualified Cardano.Configuration.File.Network as CCNet
import qualified Cardano.Configuration.File.Storage as CCSto

import           Cardano.Api (File (..))

import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Network.ConsensusMode (ConsensusMode (..))
import           Cardano.Network.PeerSelection (NumberOfBigLedgerPeers (..))
import           Cardano.Node.Configuration.LedgerDB (LedgerDbConfiguration (..),
                   LedgerDbSelectorFlag (..), noDeprecatedOptions)
import           Cardano.Node.Configuration.NodeAddress (NodeHostIPv4Address (..),
                   NodeHostIPv6Address (..))
import           Cardano.Node.Configuration.POM (NodeConfiguration,
                   PartialNodeConfiguration (..), ResponderCoreAffinityPolicy (..),
                   defaultPartialNodeConfiguration, makeNodeConfiguration)
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import           Cardano.Node.Handlers.Shutdown (ShutdownConfig (..), ShutdownOn (..))
import           Cardano.Node.Types
import           Cardano.Rpc.Server.Config (RpcConfigF (..))

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Cardano.Logging.Types as Net

import           Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import           Ouroboros.Consensus.Mempool (MempoolCapacityBytesOverride (..))
import           Ouroboros.Consensus.Node (NodeDatabasePaths (..))
import qualified Ouroboros.Consensus.Node.Genesis as Genesis
import           Ouroboros.Consensus.Storage.LedgerDB.Args (QueryBatchSize (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots (NumOfDiskSnapshots (..),
                   SnapshotFrequency (..), SnapshotFrequencyArgs (..),
                   SnapshotPolicyArgs (..), defaultSnapshotPolicyArgs)
import           Ouroboros.Consensus.Util.Args (OverrideOrDefault (..))
-- These three are re-exported (unqualified) from
-- @Ouroboros.Network.Diffusion.Configuration@, exactly as
-- 'Cardano.Node.Configuration.POM' imports them.
import           Ouroboros.Network.Diffusion.Configuration (AcceptedConnectionsLimit (..),
                   DiffusionMode (..), PeerSharing (..))
import           Ouroboros.Network.TxSubmission.Inbound.V2.Types (TxSubmissionInitDelay (..),
                   TxSubmissionLogicVersion (..))

import           Cardano.Ledger.BaseTypes.NonZero (nonZero)

import           Control.Exception (displayException)
import           Data.Bifunctor (first)
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Last (..))
import           Data.Time.Clock (secondsToDiffTime)
import           GHC.Stack (HasCallStack)
import           System.FilePath (takeDirectory, (</>))

-- | Parse the configuration file(s) referenced by the CLI arguments, resolve
-- them through @cardano-config@, and build the node's 'NodeConfiguration'
-- (applying the node's own defaults and validation via 'makeNodeConfiguration').
--
-- Returns 'Left' with a human-readable message on a resolution or validation
-- error; an I/O error while reading the configuration file is thrown.
nodeConfigurationFromCli :: HasCallStack => CC.CliArgs -> IO (Either String NodeConfiguration)
nodeConfigurationFromCli cli = do
  file <- CC.parseConfigurationFiles (CCCli.configFilePath cli)
  pure $ do
    ccnc <- first displayException (CC.resolveConfiguration cli file)
    let cfgDir = takeDirectory (CCCli.configFilePath cli)
    makeNodeConfiguration (defaultPartialNodeConfiguration <> fromCardanoConfig cfgDir ccnc)

-- | Build the node's 'NodeConfiguration' from just a configuration file path,
-- applying no CLI overrides. For tools (e.g. the chairman, tx-generator) that
-- only need the configuration file's contents.
nodeConfigurationFromFile :: HasCallStack => FilePath -> IO (Either String NodeConfiguration)
nodeConfigurationFromFile = nodeConfigurationFromCli . cliArgsFromConfigFile

-- | The default topology path used by @cardano-config@'s CLI parser, applied
-- when a tool constructs 'CC.CliArgs' without parsing a @--topology@ flag.
defaultTopologyFile :: FilePath
defaultTopologyFile = "configuration/cardano/mainnet-topology.json"

-- | Build a 'CC.CliArgs' that supplies only the configuration file path,
-- leaving every other option unset. For tools that only need the configuration
-- file's contents, with no CLI overrides.
cliArgsFromConfigFile :: FilePath -> CC.CliArgs
cliArgsFromConfigFile fp =
  CCCli.CliArgs
    { CCCli.configFilePath = fp
    , CCCli.topologyFile = defaultTopologyFile
    , CCCli.databasePathCLI = Nothing
    , CCCli.validateDatabase = False
    , CCCli.socketPath = Nothing
    , CCCli.credentials = CCCli.Credentials Nothing Nothing Nothing Nothing Nothing Nothing
    , CCCli.startAsNonProducingNode = Nothing
    , CCCli.hostAddr = Nothing
    , CCCli.hostIPv6Addr = Nothing
    , CCCli.port = Nothing
    , CCCli.tracerSocket = Nothing
    , CCCli.shutdownIPC = Nothing
    , CCCli.shutdownOnTarget = Nothing
    , CCCli.enableRpcCLI = Nothing
    , CCCli.rpcSocketPathCLI = Nothing
    }

-- | Map a resolved @cardano-config@ configuration onto a
-- 'PartialNodeConfiguration'. Combine with
-- @defaultPartialNodeConfiguration <> _@ and feed to @makeNodeConfiguration@.
fromCardanoConfig :: FilePath -> CC.NodeConfiguration -> PartialNodeConfiguration
fromCardanoConfig configDir ccnc =
    PartialNodeConfiguration
      { pncConfigFile   = Last . Just . ConfigYamlFilePath $ CC.configFilePath ccnc
      , pncTopologyFile = Last . Just . TopologyFile $ CC.topologyFile ccnc
      , pncDatabaseFile = Last . Just . fromDbPaths . runIdentity $ CC.databasePath storage
      , pncValidateDB   = Last . Just $ CC.validateDatabase ccnc
      , pncProtocolFiles = Last . Just $ fromCredentials (CC.credentials ccnc)
      , pncShutdownConfig = Last . Just $ ShutdownConfig (CC.shutdownIPC ccnc) (fromShutdownOn <$> CC.shutdownOnTarget ccnc)
      , pncSocketConfig = Last . Just $ socketConfig
      , pncStartAsNonProducingNode = Last . Just . runIdentity $ CC.startAsNonProducingNode protocol

        -- Protocol-specific parameters
      , pncProtocolConfig = Last . Just $ protocolConfig

        -- Modes
      , pncDiffusionMode = Last . Just . fromDiffusionMode . runIdentity $ CCNet.diffusionMode net
      , pncExperimentalProtocolsEnabled = Last . Just . runIdentity $ CCNet.experimentalProtocolsEnabled net

        -- BlockFetch
      , pncMaxConcurrencyBulkSync = Last . Just . MaxConcurrencyBulkSync . runIdentity $ CCNet.maxConcurrencyBulkSync net
      , pncMaxConcurrencyDeadline = Last . Just . MaxConcurrencyDeadline . runIdentity $ CCNet.maxConcurrencyDeadline net

        -- Tracing forwarder socket (CLI only)
      , pncTraceForwardSocket = Last $ fromTracerConnection <$> CC.tracerSocket ccnc

        -- Mempool
      , pncMaybeMempoolCapacityOverride =
          Last $ (MempoolCapacityBytesOverride . ByteSize32 . fromIntegral)
                   <$> CC.mempoolCapacityOverride mempool
      , pncMempoolTimeoutSoft     = Last $ CC.mempoolTimeoutSoft mempool
      , pncMempoolTimeoutHard     = Last $ CC.mempoolTimeoutHard mempool
      , pncMempoolTimeoutCapacity = Last $ CC.mempoolTimeoutCapacity mempool

        -- LedgerDB
      , pncLedgerDbConfig = Last . Just . fromLedgerDb . runIdentity $ CC.ledgerDbConfiguration storage

        -- Network timeouts
      , pncProtocolIdleTimeout  = Last . Just . runIdentity $ CCNet.protocolIdleTimeout net
      , pncTimeWaitTimeout      = Last . Just . runIdentity $ CCNet.timeWaitTimeout net
      , pncEgressPollInterval   = Last . Just . runIdentity $ CCNet.egressPollInterval net
      , pncChainSyncIdleTimeout = Last . Just . runIdentity $ CCNet.chainSyncIdleTimeout net

        -- AcceptedConnectionsLimit
      , pncAcceptedConnectionsLimit = Last . Just . fromAcceptedConnectionsLimit . runIdentity $ CCNet.acceptedConnectionsLimit net

        -- P2P governor targets (deadline)
      , pncDeadlineTargetOfRootPeers                 = Last $ CCNet.deadlineTargetOfRootPeers net
      , pncDeadlineTargetOfKnownPeers                = Last $ CCNet.deadlineTargetOfKnownPeers net
      , pncDeadlineTargetOfEstablishedPeers          = Last $ CCNet.deadlineTargetOfEstablishedPeers net
      , pncDeadlineTargetOfActivePeers               = Last $ CCNet.deadlineTargetOfActivePeers net
      , pncDeadlineTargetOfKnownBigLedgerPeers       = Last $ CCNet.deadlineTargetOfKnownBigLedgerPeers net
      , pncDeadlineTargetOfEstablishedBigLedgerPeers = Last $ CCNet.deadlineTargetOfEstablishedBigLedgerPeers net
      , pncDeadlineTargetOfActiveBigLedgerPeers      = Last $ CCNet.deadlineTargetOfActiveBigLedgerPeers net

        -- P2P governor targets (sync)
      , pncSyncTargetOfRootPeers                 = Last . Just . runIdentity $ CCNet.syncTargetOfRootPeers net
      , pncSyncTargetOfKnownPeers                = Last . Just . runIdentity $ CCNet.syncTargetOfKnownPeers net
      , pncSyncTargetOfEstablishedPeers          = Last . Just . runIdentity $ CCNet.syncTargetOfEstablishedPeers net
      , pncSyncTargetOfActivePeers               = Last . Just . runIdentity $ CCNet.syncTargetOfActivePeers net
      , pncSyncTargetOfKnownBigLedgerPeers       = Last . Just . runIdentity $ CCNet.syncTargetOfKnownBigLedgerPeers net
      , pncSyncTargetOfEstablishedBigLedgerPeers = Last . Just . runIdentity $ CCNet.syncTargetOfEstablishedBigLedgerPeers net
      , pncSyncTargetOfActiveBigLedgerPeers      = Last . Just . runIdentity $ CCNet.syncTargetOfActiveBigLedgerPeers net

      , pncMinBigLedgerPeersForTrustedState =
          Last . Just . NumberOfBigLedgerPeers . runIdentity $ CCNet.minBigLedgerPeersForTrustedState net

        -- Consensus mode / Genesis
      , pncConsensusMode = Last . Just $ consensusMode
      , pncGenesisConfigFlags = Last genesisConfigFlags

        -- Peer sharing
      , pncPeerSharing = Last $ fromPeerSharing <$> CCNet.peerSharing net

      , pncResponderCoreAffinityPolicy =
          Last . Just . fromResponderCoreAffinity . runIdentity $ CCNet.responderCoreAffinityPolicy net

      , pncTxSubmissionLogicVersion = Last $ fromTxSubmissionLogicVersion (runIdentity (CCNet.txSubmissionLogicVersion net))
      , pncTxSubmissionInitDelay    = Last . Just . TxSubmissionInitDelay . runIdentity $ CCNet.txSubmissionInitDelay net

        -- gRPC
      , pncRpcConfig = rpcConfig
      }
  where
    storage   = CC.storageConfiguration ccnc
    consensus = CC.consensusConfiguration ccnc
    protocol  = CC.protocolConfiguration ccnc
    net       = CC.networkConfiguration ccnc
    testing   = CC.testingConfiguration ccnc
    mempool   = CC.mempoolConfiguration ccnc
    lcc       = CC.localConnectionsConfig ccnc

    socketConfig =
      SocketConfig
        (Last $ NodeHostIPv4Address <$> CC.hostAddr ccnc)
        (Last $ NodeHostIPv6Address <$> CC.hostIPv6Addr ccnc)
        (Last $ CC.port ccnc)
        (Last $ File <$> CCNet.socketPath lcc)

    rpcConfig =
      RpcConfig
        (Last . Just . runIdentity $ CCNet.enableRpc lcc)
        (Last $ File <$> CCNet.rpcSocketPath lcc)
        mempty

    -- The resolved consensus mode also drives whether the (separate) genesis
    -- config flags field is populated. Mirrors the node's split: ConsensusMode
    -- (Praos|Genesis) lives in one field, the low-level flags in another.
    (consensusMode, genesisConfigFlags) =
      case runIdentity (CC.getConsensusConfiguration consensus) of
        CCCon.PraosMode          -> (PraosMode,   Nothing)
        CCCon.GenesisMode flags  -> (GenesisMode, Just (fromGenesisConfigFlags flags))

    -- Genesis files always come from the configuration file and are resolved
    -- relative to its directory (matching the legacy parser).
    genesisFile = toGenesisFile configDir

    protocolConfig =
      NodeProtocolConfigurationCardano
        (fromByron configDir (CC.byronGenesis protocol))
        (NodeShelleyProtocolConfiguration (genesisFile (CC.shelleyGenesis protocol)) (toGenesisHash (CC.shelleyGenesis protocol)))
        (NodeAlonzoProtocolConfiguration  (genesisFile (CC.alonzoGenesis protocol))  (toGenesisHash (CC.alonzoGenesis protocol)))
        (NodeConwayProtocolConfiguration  (genesisFile (CC.conwayGenesis protocol))  (toGenesisHash (CC.conwayGenesis protocol)))
        dijkstra
        hardFork
        checkpoints

    -- Dijkstra is only wired in when the experimental eras are enabled, and its
    -- genesis file lives under the Testing component in cardano-config.
    dijkstra
      | runIdentity (CC.experimentalHardForksEnabled testing)
      , Just h <- CC.experimentalGenesis testing
      = Just $ NodeDijkstraProtocolConfiguration (genesisFile h) (toGenesisHash h)
      | otherwise = Nothing

    hardFork =
      NodeHardForkProtocolConfiguration
        { npcExperimentalHardForksEnabled = runIdentity (CC.experimentalHardForksEnabled testing)
        , npcTestShelleyHardForkAtEpoch    = EpochNo <$> CC.testShelleyHardForkAtEpoch testing
        , npcTestShelleyHardForkAtVersion  = CC.testShelleyHardForkAtVersion testing
        , npcTestAllegraHardForkAtEpoch    = EpochNo <$> CC.testAllegraHardForkAtEpoch testing
        , npcTestAllegraHardForkAtVersion  = CC.testAllegraHardForkAtVersion testing
        , npcTestMaryHardForkAtEpoch       = EpochNo <$> CC.testMaryHardForkAtEpoch testing
        , npcTestMaryHardForkAtVersion     = CC.testMaryHardForkAtVersion testing
        , npcTestAlonzoHardForkAtEpoch     = EpochNo <$> CC.testAlonzoHardForkAtEpoch testing
        , npcTestAlonzoHardForkAtVersion   = CC.testAlonzoHardForkAtVersion testing
        , npcTestBabbageHardForkAtEpoch    = EpochNo <$> CC.testBabbageHardForkAtEpoch testing
        , npcTestBabbageHardForkAtVersion  = CC.testBabbageHardForkAtVersion testing
        , npcTestConwayHardForkAtEpoch     = EpochNo <$> CC.testConwayHardForkAtEpoch testing
        , npcTestConwayHardForkAtVersion   = CC.testConwayHardForkAtVersion testing
        , npcTestDijkstraHardForkAtEpoch   = EpochNo <$> CC.testDijkstraHardForkAtEpoch testing
        , npcTestDijkstraHardForkAtVersion = CC.testDijkstraHardForkAtVersion testing
        }

    checkpoints =
      NodeCheckpointsConfiguration
        (CheckpointsFile . (configDir </>) . CC.hashed <$> CC.checkpointsFile protocol)
        (toHash =<< CC.checkpointsFile protocol)
      where
        toHash h = CheckpointsHash <$> CC.hash h

--------------------------------------------------------------------------------
-- Component conversions
--------------------------------------------------------------------------------

-- Field order is (immutable, volatile) for both: cardano-config's
-- @SplitDB ImmutablePath VolatilePath@ and the node's
-- @MultipleDbPaths immutable volatile@ (see Cardano.Node.Run).
fromDbPaths :: CC.NodeDatabasePaths -> NodeDatabasePaths
fromDbPaths = \case
  CC.SingleDB fp    -> OnePathForAllDbs fp
  CC.SplitDB im vol -> MultipleDbPaths im vol

fromCredentials :: CC.Credentials -> ProtocolFilepaths
fromCredentials creds =
  ProtocolFilepaths
    { byronCertFile        = CCCli.byronDelegationCertificate creds
    , byronKeyFile         = CCCli.byronSigningKey creds
    , shelleyKESSource     = fromKESSource <$> CCCli.shelleyKES creds
    , shelleyVRFFile       = CCCli.shelleyVRFKey creds
    , shelleyCertFile      = CCCli.shelleyOperationalCertificate creds
    , shelleyBulkCredsFile = CCCli.bulkCredentialsFile creds
    }

fromKESSource :: CCCli.KESSource -> KESSource
fromKESSource = \case
  CCCli.KESKeyFilePath fp    -> KESKeyFilePath fp
  CCCli.KESAgentSocketPath s -> KESAgentSocketPath s

fromShutdownOn :: CCCli.ShutdownOn -> ShutdownOn
fromShutdownOn = \case
  CCCli.ShutdownAtSlot  w -> ASlot  (SlotNo w)
  CCCli.ShutdownAtBlock w -> ABlock (BlockNo w)

fromDiffusionMode :: CCNet.DiffusionMode -> DiffusionMode
fromDiffusionMode = \case
  CCNet.InitiatorOnly         -> InitiatorOnlyDiffusionMode
  CCNet.InitiatorAndResponder -> InitiatorAndResponderDiffusionMode

fromPeerSharing :: Bool -> PeerSharing
fromPeerSharing True  = PeerSharingEnabled
fromPeerSharing False = PeerSharingDisabled

fromResponderCoreAffinity :: String -> ResponderCoreAffinityPolicy
fromResponderCoreAffinity = \case
  "ResponderCoreAffinity" -> ResponderCoreAffinity
  _                       -> NoResponderCoreAffinity

-- Spellings match the node's orphan @FromJSON TxSubmissionLogicVersion@
-- (Ouroboros.Network.OrphanInstances). Falls back to the node default
-- (Last Nothing) on an unrecognised value.
fromTxSubmissionLogicVersion :: String -> Maybe TxSubmissionLogicVersion
fromTxSubmissionLogicVersion = \case
  "TxSubmissionLogicV1" -> Just TxSubmissionLogicV1
  "TxSubmissionLogicV2" -> Just TxSubmissionLogicV2
  _                     -> Nothing

-- cardano-config: AcceptedConnectionsLimit <hard> <soft> <delay>.
-- ouroboros: AcceptedConnectionsLimit { hard, soft, delay }.
fromAcceptedConnectionsLimit :: CCNet.AcceptedConnectionsLimit -> AcceptedConnectionsLimit
fromAcceptedConnectionsLimit (CCNet.AcceptedConnectionsLimit hard soft delay) =
  AcceptedConnectionsLimit hard soft delay

fromTracerConnection :: CCCli.TracerConnection -> (Net.HowToConnect, Net.ForwarderMode)
fromTracerConnection (CCCli.TracerConnection tag method) =
    (howToConnect, forwarderMode tag)
  where
    howToConnect = case method of
      CCCli.TracerConnectViaPipe fp        -> Net.LocalPipe fp
      CCCli.TracerConnectViaRemote host pn -> Net.RemoteSocket host (fromIntegral pn)
    -- "Accept" => we accept an incoming connection (Responder);
    -- "Connect" => we connect out (Initiator). The tag comes from
    -- cardano-config's TracerConnection (Cardano.Configuration.CliArgs).
    forwarderMode "Connect" = Net.Initiator
    forwarderMode _         = Net.Responder

fromGenesisConfigFlags :: CCCon.GenesisConfigFlags -> Genesis.GenesisConfigFlags
fromGenesisConfigFlags f =
  Genesis.GenesisConfigFlags
    { Genesis.gcfEnableCSJ             = CCCon.gcfEnableCSJ f
    , Genesis.gcfEnableLoEAndGDD       = CCCon.gcfEnableLoEAndGDD f
    , Genesis.gcfEnableLoP             = CCCon.gcfEnableLoP f
    , Genesis.gcfBlockFetchGracePeriod = CCCon.gcfBlockFetchGracePeriod f
    , Genesis.gcfBucketCapacity        = CCCon.gcfBucketCapacity f
    , Genesis.gcfBucketRate            = CCCon.gcfBucketRate f
    , Genesis.gcfCSJJumpSize           = SlotNo <$> CCCon.gcfCSJJumpSize f
    , Genesis.gcfGDDRateLimit          = CCCon.gcfGDDRateLimit f
    }

fromByron :: FilePath -> CC.ByronGenesisConfiguration -> NodeByronProtocolConfiguration
fromByron configDir b =
  NodeByronProtocolConfiguration
    { npcByronGenesisFile     = toGenesisFile configDir (CC.byronGenesisFile b)
    , npcByronGenesisFileHash = toGenesisHash (CC.byronGenesisFile b)
    , npcByronReqNetworkMagic = fromReqNetworkMagic (CC.byronReqNetworkMagic b)
    , npcByronPbftSignatureThresh = CC.byronPbftSignatureThresh b
    , npcByronSupportedProtocolVersionMajor = CC.byronSupportedProtocolVersionMajor b
    , npcByronSupportedProtocolVersionMinor = CC.byronSupportedProtocolVersionMinor b
    , npcByronSupportedProtocolVersionAlt   = fromMaybe 0 (CC.byronSupportedProtocolVersionAlt b)
    }

-- cardano-config uses its own 'RequiresNetworkMagic' enum (absent => default
-- 'RequiresNoMagic'); the node uses the cardano-crypto enum.
fromReqNetworkMagic :: Maybe CC.RequiresNetworkMagic -> RequiresNetworkMagic
fromReqNetworkMagic = \case
  Just CC.RequiresMagic   -> RequiresMagic
  Just CC.RequiresNoMagic -> RequiresNoMagic
  Nothing                 -> RequiresNoMagic

-- | Resolve a genesis file path relative to the configuration file's directory.
-- @System.FilePath.(</>)@ leaves absolute paths untouched.
toGenesisFile :: FilePath -> CC.Hashed FilePath -> GenesisFile
toGenesisFile configDir = GenesisFile . (configDir </>) . CC.hashed

toGenesisHash :: CC.Hashed FilePath -> Maybe GenesisHash
toGenesisHash = fmap GenesisHash . CC.hash

-- TODO(adapter): cardano-config's Mithril snapshot policy and the LSM export
-- path have no representation in the node's LedgerDbConfiguration; the Mithril
-- policy falls back to the default and the export path is dropped. The node also
-- does not consume MinDelay/MaxDelay (sfaDelaySnapshotRange is always UseDefault
-- here, mirroring the legacy parser). An absent backend selector defaults to
-- 'V2InMemory'.
fromLedgerDb :: CC.LedgerDbConfiguration -> LedgerDbConfiguration
fromLedgerDb ldb =
    LedgerDbConfiguration
      snapshotPolicyArgs
      queryBatchSize
      selector
      noDeprecatedOptions
  where
    snapshotPolicyArgs =
      case CC.snapshots ldb of
        Nothing -> defaultSnapshotPolicyArgs
        Just CCSto.MithrilSnapshotPolicy -> defaultSnapshotPolicyArgs   -- TODO(adapter)
        Just (CCSto.CustomSnapshotPolicy opts) ->
          SnapshotPolicyArgs
            (SnapshotFrequency SnapshotFrequencyArgs
              { sfaInterval = maybe UseDefault Override (CCSto.snapshotInterval opts >>= nonZero)
              , sfaOffset   = maybe UseDefault (Override . SlotNo) (CCSto.slotOffset opts)
              , sfaRateLimit = maybe UseDefault (Override . secondsToDiffTime . fromIntegral) (CCSto.snapshotRateLimit opts)
              , sfaDelaySnapshotRange = UseDefault
              })
            (maybe UseDefault (Override . NumOfDiskSnapshots . fromIntegral) (CCSto.numOfDiskSnapshots opts))

    queryBatchSize = maybe DefaultQueryBatchSize RequestedQueryBatchSize (CC.queryBatchSize ldb)

    selector = case CC.backendSelector ldb of
      Nothing                          -> V2InMemory
      Just CCSto.V2InMemory            -> V2InMemory
      Just (CCSto.V2LSM dbPath _export) -> V2LSM dbPath
