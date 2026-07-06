{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

{- HLINT ignore "Functor law" -}

module Cardano.Node.Configuration.POM
  ( NodeConfiguration (..)
  , ResponderCoreAffinityPolicy (..)
  , PartialNodeConfiguration(..)
  , TimeoutOverride (..)
  , defaultPartialNodeConfiguration
  , lastOption
  , makeNodeConfiguration
  , pncProtocol
  , ncProtocol
  , getForkPolicy
  )
where

import           Cardano.Logging.Types
import           Cardano.Network.ConsensusMode (ConsensusMode (..), defaultConsensusMode)
import qualified Cardano.Network.Diffusion.Configuration as Cardano
import           Cardano.Network.PeerSelection (NumberOfBigLedgerPeers (..))
import           Cardano.Node.Configuration.LedgerDB
import           Cardano.Node.Configuration.Socket (SocketConfig (..))
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.Types
import           Cardano.Rpc.Server.Config (PartialRpcConfig, RpcConfig, RpcConfigF (..),
                   makeRpcConfig)
import           Ouroboros.Consensus.Mempool (MempoolCapacityBytesOverride (..))
import           Ouroboros.Consensus.Node (NodeDatabasePaths (..))
import           Ouroboros.Consensus.Node.Genesis (GenesisConfig, GenesisConfigFlags,
                   defaultGenesisConfigFlags, mkGenesisConfig)
import           Ouroboros.Consensus.Storage.LedgerDB.Args (QueryBatchSize (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots (defaultSnapshotPolicyArgs)
import           Ouroboros.Network.Diffusion.Configuration as Configuration
import qualified Ouroboros.Network.Diffusion.Configuration as Ouroboros
import qualified Ouroboros.Network.Mux as Mux
import           Ouroboros.Network.OrphanInstances ()
import qualified Ouroboros.Network.PeerSelection.Governor as PeerSelection
import           Ouroboros.Network.TxSubmission.Inbound.V2.Types (TxSubmissionInitDelay (..),
                   TxSubmissionLogicVersion (..), defaultTxSubmissionInitDelay)

import           Control.Concurrent (getNumCapabilities)
import           Control.Monad (unless)
import           Data.Aeson
import           Data.Hashable (Hashable)
import           Data.Maybe
import           Data.Monoid (Last (..))
import           Data.Text (Text)
import           Data.Time.Clock (DiffTime)
import           GHC.Generics (Generic)
import           Options.Applicative
import           System.Random (randomIO)

import           Generic.Data (gmappend)
import           Generic.Data.Orphans ()

-- | Isomorphic to a `Maybe DiffTime`, but expresses what `Nothing` means, in
-- this case that we want to /NOT/ override the default timeout.
data TimeoutOverride = NoTimeoutOverride | TimeoutOverride DiffTime
  deriving (Eq, Show)

data NodeConfiguration
  = NodeConfiguration
      {  ncSocketConfig    :: !SocketConfig
           -- | Filepath of the configuration yaml file. This file determines
          -- all the configuration settings required for the cardano node
          -- (logging, tracing, protocol, slot length etc)
       , ncConfigFile      :: !ConfigYamlFilePath
       , ncTopologyFile    :: !TopologyFile
       , ncDatabaseFile    :: !NodeDatabasePaths
       , ncProtocolFiles   :: !ProtocolFilepaths
       , ncValidateDB      :: !Bool
       , ncShutdownConfig  :: !ShutdownConfig

       , ncStartAsNonProducingNode :: !Bool

        -- Protocol-specific parameters:
       , ncProtocolConfig :: !NodeProtocolConfiguration

         -- Modes
       , ncDiffusionMode :: !DiffusionMode

         -- | During the development and integration of new network protocols
         -- (node-to-node and node-to-client) we wish to be able to test them
         -- but not have everybody use them by default on the mainnet. Avoiding
         -- enabling them by default makes it practical to include such
         -- not-yet-ready protocol versions into released versions of the node
         -- without the danger that node operators on the mainnet will start
         -- using them prematurely, before the testing is complete.
         --
         -- The flag defaults to 'False'
         --
         -- This flag should be set to 'True' when testing the new protocol
         -- versions.
       , ncExperimentalProtocolsEnabled :: !Bool

         -- BlockFetch configuration
       , ncMaxConcurrencyBulkSync :: !(Maybe MaxConcurrencyBulkSync)
       , ncMaxConcurrencyDeadline :: !(Maybe MaxConcurrencyDeadline)

       , ncTraceForwardSocket :: !(Maybe (HowToConnect, ForwarderMode))

       , ncMaybeMempoolCapacityOverride :: !(Maybe MempoolCapacityBytesOverride)

         -- LedgerDB configuration
       , ncLedgerDbConfig     :: !LedgerDbConfiguration

         -- | Protocol idleness timeout, see
         -- 'Ouroboros.Network.Diffusion.daProtocolIdleTimeout'.
         --
       , ncProtocolIdleTimeout   :: DiffTime
         -- | Wait time timeout, see
         -- 'Ouroboros.Network.Diffusion.daTimeWaitTimeout'.
         --
       , ncTimeWaitTimeout       :: DiffTime

       , ncEgressPollInterval    :: DiffTime

         -- | Timeout override for ChainSync, see
         -- 'Ouroboros.Network.Protocol.ChainSync.Codec.ChainSyncTimeout'
       , ncChainSyncIdleTimeout :: TimeoutOverride

         -- Mempool timeout configurations:
         -- These configuration control a lightweight "defensive programming"
         -- feature in the Mempool.
         -- See documentation in @Ouroboros.Consensus.Mempool.API@ for more info

         -- | If the mempool takes longer than this to validate a tx, then it
         -- discards the tx instead of adding it.
       , ncMempoolTimeoutSoft :: DiffTime

         -- | If the mempool takes longer than this to validate a tx, then it
         -- disconnects from the peer.
         --
         -- WARNING: if this is less than 'mempoolTimeoutSoft', then
         -- 'mempoolTimeoutSoft' is irrelevant. If it's equal or just barely larger,
         -- then the soft/hard distinction will likely be unreliable.
       , ncMempoolTimeoutHard :: DiffTime

          -- | If the mempool takes longer than this cumulatively to
          -- validate when each entered the mempool, then the mempool is at
          -- capacity, ie it's full, ie no tx can be added.
       , ncMempoolTimeoutCapacity :: DiffTime

         -- | Node AcceptedConnectionsLimit
       , ncAcceptedConnectionsLimit :: !AcceptedConnectionsLimit

         -- P2P governor targets
       , ncDeadlineTargetOfRootPeers        :: !Int
       , ncDeadlineTargetOfKnownPeers       :: !Int
       , ncDeadlineTargetOfEstablishedPeers :: !Int
       , ncDeadlineTargetOfActivePeers      :: !Int
       , ncDeadlineTargetOfKnownBigLedgerPeers       :: !Int
       , ncDeadlineTargetOfEstablishedBigLedgerPeers :: !Int
       , ncDeadlineTargetOfActiveBigLedgerPeers      :: !Int
       , ncSyncTargetOfRootPeers        :: !Int
       , ncSyncTargetOfKnownPeers       :: !Int
       , ncSyncTargetOfEstablishedPeers :: !Int
       , ncSyncTargetOfActivePeers      :: !Int
       , ncSyncTargetOfKnownBigLedgerPeers       :: !Int
       , ncSyncTargetOfEstablishedBigLedgerPeers :: !Int
       , ncSyncTargetOfActiveBigLedgerPeers      :: !Int

         -- Used to determine which set of peer targets to use
         -- by the diffusion layer when syncing
       , ncConsensusMode :: !ConsensusMode

         -- Minimum number of active big ledger peers we must be connected to
         -- in Genesis mode
       , ncMinBigLedgerPeersForTrustedState :: NumberOfBigLedgerPeers

         -- Enable Peer Sharing
       , ncPeerSharing :: PeerSharing

         -- Ouroboros Genesis
       , ncGenesisConfig :: GenesisConfig

       , ncResponderCoreAffinityPolicy :: ResponderCoreAffinityPolicy

         -- gRPC
       , ncRpcConfig :: RpcConfig

       , ncTxSubmissionLogicVersion :: TxSubmissionLogicVersion
       , ncTxSubmissionInitDelay :: TxSubmissionInitDelay
       } deriving (Eq, Show)

-- | We expose the `Ouroboros.Network.Mux.ForkPolicy` as a `NodeConfiguration` field.
--
-- * `NoResponderCoreAffinity` corresponds to `Ouroboros.Network.Mux.noBindForkPolicy`
-- * `ResponderCoreAffinity` corresponds to `Ouroboros.Network.Mux.responderForkPolicy`
--   with a `randomIO` generated salt and `getNumCapabilities`.
--
data ResponderCoreAffinityPolicy = NoResponderCoreAffinity | ResponderCoreAffinity deriving (Eq, Show, Generic, FromJSON)

-- | Convert `NCForkPolicy` to a `Ouroboros.Network.Mux.ForkPolicy`
getForkPolicy :: Hashable peerAddr => ResponderCoreAffinityPolicy -> IO (Mux.ForkPolicy peerAddr)
getForkPolicy = \case
  NoResponderCoreAffinity -> pure Mux.noBindForkPolicy
  ResponderCoreAffinity -> Mux.responderForkPolicy <$> randomIO <*> getNumCapabilities

data PartialNodeConfiguration
  = PartialNodeConfiguration
      {  pncSocketConfig    :: !(Last SocketConfig)
         -- | Filepath of the configuration yaml file. This file determines
         -- all the configuration settings required for the cardano node
         -- (logging, tracing, protocol, slot length etc)
       , pncConfigFile      :: !(Last ConfigYamlFilePath)
       , pncTopologyFile    :: !(Last TopologyFile)
       , pncDatabaseFile    :: !(Last NodeDatabasePaths)
         -- | pncProtocolFiles can only be supplied with command line arguments.
       , pncProtocolFiles   :: !(Last ProtocolFilepaths)
       , pncValidateDB      :: !(Last Bool)
       , pncShutdownConfig  :: !(Last ShutdownConfig)

       , pncStartAsNonProducingNode :: !(Last Bool)

         -- Protocol-specific parameters:
       , pncProtocolConfig :: !(Last NodeProtocolConfiguration)

         -- Node parameters, not protocol-specific:
       , pncDiffusionMode      :: !(Last DiffusionMode)

       , pncExperimentalProtocolsEnabled :: !(Last Bool)

         -- BlockFetch configuration
       , pncMaxConcurrencyBulkSync :: !(Last MaxConcurrencyBulkSync)
       , pncMaxConcurrencyDeadline :: !(Last MaxConcurrencyDeadline)

       , pncTraceForwardSocket :: !(Last (HowToConnect, ForwarderMode))

         -- Configuration for testing purposes
       , pncMaybeMempoolCapacityOverride :: !(Last MempoolCapacityBytesOverride)

         -- LedgerDB configuration
       , pncLedgerDbConfig :: !(Last LedgerDbConfiguration)

         -- Network timeouts
       , pncProtocolIdleTimeout   :: !(Last DiffTime)
       , pncTimeWaitTimeout       :: !(Last DiffTime)
       , pncEgressPollInterval    :: !(Last DiffTime)

       , pncChainSyncIdleTimeout      :: !(Last DiffTime)

       -- Mempool timeout configurations:
       , pncMempoolTimeoutSoft :: !(Last DiffTime)
       , pncMempoolTimeoutHard :: !(Last DiffTime)
       , pncMempoolTimeoutCapacity :: !(Last DiffTime)

         -- AcceptedConnectionsLimit
       , pncAcceptedConnectionsLimit :: !(Last AcceptedConnectionsLimit)

         -- P2P governor targets
       , pncDeadlineTargetOfRootPeers        :: !(Last Int)
       , pncDeadlineTargetOfKnownPeers       :: !(Last Int)
       , pncDeadlineTargetOfEstablishedPeers :: !(Last Int)
       , pncDeadlineTargetOfActivePeers      :: !(Last Int)
       , pncDeadlineTargetOfKnownBigLedgerPeers       :: !(Last Int)
       , pncDeadlineTargetOfEstablishedBigLedgerPeers :: !(Last Int)
       , pncDeadlineTargetOfActiveBigLedgerPeers      :: !(Last Int)
       , pncSyncTargetOfRootPeers                 :: !(Last Int)
       , pncSyncTargetOfKnownPeers                :: !(Last Int)
       , pncSyncTargetOfEstablishedPeers          :: !(Last Int)
       , pncSyncTargetOfActivePeers               :: !(Last Int)
       , pncSyncTargetOfKnownBigLedgerPeers       :: !(Last Int)
       , pncSyncTargetOfEstablishedBigLedgerPeers :: !(Last Int)
       , pncSyncTargetOfActiveBigLedgerPeers      :: !(Last Int)

         -- Minimum number of active big ledger peers we must be connected to
         -- in Genesis mode
       , pncMinBigLedgerPeersForTrustedState :: !(Last NumberOfBigLedgerPeers)

         -- Consensus mode for diffusion layer
       , pncConsensusMode :: !(Last ConsensusMode)

         -- Peer Sharing
       , pncPeerSharing :: !(Last PeerSharing)

         -- Ouroboros Genesis
       , pncGenesisConfigFlags :: !(Last GenesisConfigFlags)

       , pncResponderCoreAffinityPolicy :: !(Last ResponderCoreAffinityPolicy)

       , pncTxSubmissionLogicVersion :: !(Last TxSubmissionLogicVersion)
       , pncTxSubmissionInitDelay :: !(Last TxSubmissionInitDelay)

         -- gRPC
       , pncRpcConfig :: !PartialRpcConfig
       } deriving (Eq, Generic, Show)

instance Semigroup PartialNodeConfiguration where
  (<>) = gmappend

-- | Default configuration is mainnet
defaultPartialNodeConfiguration :: PartialNodeConfiguration
defaultPartialNodeConfiguration =
  PartialNodeConfiguration
    { pncConfigFile = Last . Just $ ConfigYamlFilePath "configuration/cardano/mainnet-config.json"
    , pncDatabaseFile = Last . Just $ OnePathForAllDbs "mainnet/db/"
    , pncSocketConfig = Last . Just $ SocketConfig mempty mempty mempty mempty
    , pncDiffusionMode = Last $ Just InitiatorAndResponderDiffusionMode
    , pncExperimentalProtocolsEnabled = Last $ Just False
    , pncTopologyFile = Last . Just $ TopologyFile "configuration/cardano/mainnet-topology.json"
    , pncProtocolFiles = Last . Just $ ProtocolFilepaths Nothing Nothing Nothing Nothing Nothing Nothing
    , pncValidateDB = Last $ Just False
    , pncShutdownConfig = Last . Just $ ShutdownConfig Nothing Nothing
    , pncStartAsNonProducingNode = Last $ Just False
    , pncProtocolConfig = mempty
    , pncMaxConcurrencyBulkSync = mempty
    , pncMaxConcurrencyDeadline = mempty
    , pncTraceForwardSocket = mempty
    , pncMaybeMempoolCapacityOverride = mempty
    , pncLedgerDbConfig =
        Last $ Just $
          LedgerDbConfiguration
            defaultSnapshotPolicyArgs
            DefaultQueryBatchSize
            V2InMemory
            noDeprecatedOptions
    , pncProtocolIdleTimeout      = Last (Just Ouroboros.defaultProtocolIdleTimeout)
      -- https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-Diffusion-Configuration.html#v:defaultProtocolIdleTimeout
    , pncTimeWaitTimeout          = Last (Just Ouroboros.defaultTimeWaitTimeout)
      -- https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-Diffusion-Configuration.html#v:defaultTimeWaitTimeout
    , pncEgressPollInterval       = Last (Just Ouroboros.defaultEgressPollInterval)
      -- https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-Diffusion-Configuration.html#v:defaultEgressPollInterval
    , pncAcceptedConnectionsLimit = Last (Just Ouroboros.defaultAcceptedConnectionsLimit)
      -- https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-Diffusion-Configuration.html#v:defaultAcceptedConnectionsLimit
    , pncChainSyncIdleTimeout     = mempty
    , pncMempoolTimeoutSoft       = mempty
    , pncMempoolTimeoutHard       = mempty
    , pncMempoolTimeoutCapacity   = mempty

    -- these targets are set properly in makeNodeConfiguration below
    , pncDeadlineTargetOfRootPeers                 = mempty
    , pncDeadlineTargetOfKnownPeers                = mempty
    , pncDeadlineTargetOfEstablishedPeers          = mempty
    , pncDeadlineTargetOfActivePeers               = mempty
    , pncDeadlineTargetOfKnownBigLedgerPeers       = mempty
    , pncDeadlineTargetOfEstablishedBigLedgerPeers = mempty
    , pncDeadlineTargetOfActiveBigLedgerPeers      = mempty
      -- https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-Diffusion-Configuration.html#v:defaultDeadlineTargets

    , pncSyncTargetOfRootPeers                     = Last (Just $ targetNumberOfRootPeers                 Cardano.defaultSyncTargets)
    , pncSyncTargetOfKnownPeers                    = Last (Just $ targetNumberOfKnownPeers                Cardano.defaultSyncTargets)
    , pncSyncTargetOfEstablishedPeers              = Last (Just $ targetNumberOfEstablishedPeers          Cardano.defaultSyncTargets)
    , pncSyncTargetOfActivePeers                   = Last (Just $ targetNumberOfActivePeers               Cardano.defaultSyncTargets)
    , pncSyncTargetOfKnownBigLedgerPeers           = Last (Just $ targetNumberOfKnownBigLedgerPeers       Cardano.defaultSyncTargets)
    , pncSyncTargetOfEstablishedBigLedgerPeers     = Last (Just $ targetNumberOfEstablishedBigLedgerPeers Cardano.defaultSyncTargets)
    , pncSyncTargetOfActiveBigLedgerPeers          = Last (Just $ targetNumberOfActiveBigLedgerPeers      Cardano.defaultSyncTargets)
      -- https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/cardano-diffusion/Cardano-Network-Diffusion-Configuration.html#v:defaultSyncTargets

    , pncMinBigLedgerPeersForTrustedState = Last (Just Cardano.defaultNumberOfBigLedgerPeers)
      -- https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/cardano-diffusion/Cardano-Network-Diffusion-Configuration.html#v:defaultNumberOfBigLedgerPeers
    , pncConsensusMode = Last (Just defaultConsensusMode)
      -- https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-Diffusion-Configuration.html#v:defaultConsensusMode
    , pncPeerSharing   = mempty
      -- the default is defined in `makeNodeConfiguration`
    , pncGenesisConfigFlags = Last (Just defaultGenesisConfigFlags)
      -- https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-diffusion/Ouroboros-Consensus-Node-Genesis.html#v:defaultGenesisConfigFlags
    , pncResponderCoreAffinityPolicy = Last $ Just NoResponderCoreAffinity
    , pncRpcConfig = mempty

    , pncTxSubmissionLogicVersion = Last $ Just TxSubmissionLogicV1
    , pncTxSubmissionInitDelay = Last $ Just defaultTxSubmissionInitDelay
    }

lastOption :: Parser a -> Parser (Last a)
lastOption = fmap Last . optional

lastToEither :: String -> Last a -> Either String a
lastToEither msg = maybe (Left msg) Right . getLast

makeNodeConfiguration :: PartialNodeConfiguration -> Either String NodeConfiguration
makeNodeConfiguration pnc = do
  configFile <- lastToEither "Missing YAML config file" $ pncConfigFile pnc
  topologyFile <- lastToEither "Missing TopologyFile" $ pncTopologyFile pnc
  databaseFile <- lastToEither "Missing DatabaseFile" $ pncDatabaseFile pnc
  validateDB <- lastToEither "Missing ValidateDB" $ pncValidateDB pnc
  startAsNonProducingNode <- lastToEither "Missing StartAsNonProducingNode" $ pncStartAsNonProducingNode pnc
  protocolConfig <- lastToEither "Missing ProtocolConfig" $ pncProtocolConfig pnc
  protocolFiles <- lastToEither "Missing ProtocolFiles" $ pncProtocolFiles pnc
  diffusionMode <- lastToEither "Missing DiffusionMode" $ pncDiffusionMode pnc
  shutdownConfig <- lastToEither "Missing ShutdownConfig" $ pncShutdownConfig pnc
  socketConfig <- lastToEither "Missing SocketConfig" $ pncSocketConfig pnc

  let PeerSelectionTargets {
        targetNumberOfRootPeers, targetNumberOfKnownPeers,
        targetNumberOfEstablishedPeers, targetNumberOfActivePeers,
        targetNumberOfKnownBigLedgerPeers, targetNumberOfEstablishedBigLedgerPeers,
        targetNumberOfActiveBigLedgerPeers
        } = Ouroboros.defaultDeadlineTargets $ if hasProtocolFile protocolFiles
              then BlockProducer else Relay
      (<>!) defaults override = fromJust . getLast $ pure defaults <> override

      ncDeadlineTargetOfRootPeers =
        targetNumberOfRootPeers <>! pncDeadlineTargetOfRootPeers pnc
      ncDeadlineTargetOfKnownPeers =
        targetNumberOfKnownPeers <>! pncDeadlineTargetOfKnownPeers pnc
      ncDeadlineTargetOfEstablishedPeers =
        targetNumberOfEstablishedPeers <>! pncDeadlineTargetOfEstablishedPeers pnc
      ncDeadlineTargetOfActivePeers =
        targetNumberOfActivePeers <>! pncDeadlineTargetOfActivePeers pnc
      ncDeadlineTargetOfKnownBigLedgerPeers =
        targetNumberOfKnownBigLedgerPeers <>! pncDeadlineTargetOfKnownBigLedgerPeers pnc
      ncDeadlineTargetOfEstablishedBigLedgerPeers =
        targetNumberOfEstablishedBigLedgerPeers <>! pncDeadlineTargetOfEstablishedBigLedgerPeers pnc
      ncDeadlineTargetOfActiveBigLedgerPeers =
        targetNumberOfActiveBigLedgerPeers <>! pncDeadlineTargetOfActiveBigLedgerPeers pnc

  ncSyncTargetOfRootPeers <-
    lastToEither "Missing SyncTargetNumberOfRootPeers"
    $ pncSyncTargetOfRootPeers pnc
  ncSyncTargetOfKnownPeers <-
    lastToEither "Missing SyncTargetNumberOfKnownPeers"
    $ pncSyncTargetOfKnownPeers pnc
  ncSyncTargetOfEstablishedPeers <-
    lastToEither "Missing SyncTargetNumberOfEstablishedPeers"
    $ pncSyncTargetOfEstablishedPeers pnc
  ncSyncTargetOfActivePeers <-
    lastToEither "Missing SyncTargetNumberOfActivePeers"
    $ pncSyncTargetOfActivePeers pnc
  ncSyncTargetOfKnownBigLedgerPeers <-
    lastToEither "Missing SyncTargetNumberOfKnownBigLedgerPeers"
    $ pncSyncTargetOfKnownBigLedgerPeers pnc
  ncSyncTargetOfEstablishedBigLedgerPeers <-
    lastToEither "Missing SyncTargetNumberOfEstablishedBigLedgerPeers"
    $ pncSyncTargetOfEstablishedBigLedgerPeers pnc
  ncSyncTargetOfActiveBigLedgerPeers <-
    lastToEither "Missing SyncTargetNumberOfActiveBigLedgerPeers"
    $ pncSyncTargetOfActiveBigLedgerPeers pnc
  ncMinBigLedgerPeersForTrustedState <-
    lastToEither "Missing MinBigLedgerPeersForTrustedState"
    $ pncMinBigLedgerPeersForTrustedState pnc
  ncConsensusMode <-
    lastToEither "Missing ConsensusMode"
    $ pncConsensusMode pnc
  ncLedgerDbConfig <-
    lastToEither "Missing LedgerDb config"
    $ pncLedgerDbConfig pnc
  ncProtocolIdleTimeout <-
    lastToEither "Missing ProtocolIdleTimeout"
    $ pncProtocolIdleTimeout pnc
  ncTimeWaitTimeout <-
    lastToEither "Missing TimeWaitTimeout"
    $ pncTimeWaitTimeout pnc
  ncEgressPollInterval <-
    lastToEither "Missing EgressPollInterval"
    $ pncEgressPollInterval pnc
  ncAcceptedConnectionsLimit <-
    lastToEither "Missing AcceptedConnectionsLimit" $
      pncAcceptedConnectionsLimit pnc
  ncChainSyncIdleTimeout <-
    Right
    $ maybe NoTimeoutOverride TimeoutOverride
    $ getLast
    $ pncChainSyncIdleTimeout pnc

  let mempoolTimeouts = ( getLast (pncMempoolTimeoutSoft pnc)
                        , getLast (pncMempoolTimeoutHard pnc)
                        , getLast (pncMempoolTimeoutCapacity pnc)
                        )
  (ncMempoolTimeoutSoft, ncMempoolTimeoutHard, ncMempoolTimeoutCapacity) <-
    case mempoolTimeouts of
      (Just s, Just h, Just c) -> pure (s, h, c)
      (Nothing, Nothing, Nothing) -> pure (1, 1.5, 5)
      _ -> Left "Mempool timeouts must be either all set or all unset"

  let ncPeerSharing =
        case pncPeerSharing pnc of
          Last Nothing ->
            if hasProtocolFile protocolFiles
            then PeerSharingDisabled
            else PeerSharingEnabled
          Last (Just peerSharing) -> peerSharing

  mGenesisConfigFlags <- case ncConsensusMode of
    PraosMode -> pure Nothing
    GenesisMode ->
        fmap Just
      $ lastToEither "Missing GenesisConfigFlags"
      $ pncGenesisConfigFlags pnc
  let ncGenesisConfig = mkGenesisConfig mGenesisConfigFlags

  ncResponderCoreAffinityPolicy <- lastToEither "Missing ResponderCoreAffinityPolicy" $ pncResponderCoreAffinityPolicy pnc

  ncTxSubmissionLogicVersion <- lastToEither "Missing TxSubmissionLogicVersion" $ pncTxSubmissionLogicVersion pnc
  ncTxSubmissionInitDelay <- lastToEither "Missing TxSubmissionInitDelay" $ pncTxSubmissionInitDelay pnc

  let deadlineTargets =
        PeerSelectionTargets {
          targetNumberOfRootPeers = ncDeadlineTargetOfRootPeers,
          targetNumberOfKnownPeers = ncDeadlineTargetOfKnownPeers,
          targetNumberOfEstablishedPeers = ncDeadlineTargetOfEstablishedPeers,
          targetNumberOfActivePeers = ncDeadlineTargetOfActivePeers,
          targetNumberOfKnownBigLedgerPeers = ncDeadlineTargetOfKnownBigLedgerPeers,
          targetNumberOfEstablishedBigLedgerPeers = ncDeadlineTargetOfEstablishedBigLedgerPeers,
          targetNumberOfActiveBigLedgerPeers = ncDeadlineTargetOfActiveBigLedgerPeers }
      syncTargets = PeerSelectionTargets {
        targetNumberOfRootPeers  = ncSyncTargetOfRootPeers,
        targetNumberOfKnownPeers = ncSyncTargetOfKnownPeers,
        targetNumberOfEstablishedPeers = ncSyncTargetOfEstablishedPeers,
        targetNumberOfActivePeers = ncSyncTargetOfActivePeers,
        targetNumberOfKnownBigLedgerPeers = ncSyncTargetOfKnownBigLedgerPeers,
        targetNumberOfEstablishedBigLedgerPeers = ncSyncTargetOfEstablishedBigLedgerPeers,
        targetNumberOfActiveBigLedgerPeers = ncSyncTargetOfActiveBigLedgerPeers  }

  unless (PeerSelection.sanePeerSelectionTargets deadlineTargets
          && PeerSelection.sanePeerSelectionTargets syncTargets) $
    Left $ "Invalid peer selection targets. Ensure that targets satisfy the "
           <> "inequalities of known >= established >= active >= 0"
           <> "for both deadline and sync target groups. The deadline group target names start with "
           <> "TargetNumber... while the sync group starts with SyncTarget... "

  -- TODO: This is not mandatory
  experimentalProtocols <-
    lastToEither "Missing ExperimentalProtocolsEnabled" $
      pncExperimentalProtocolsEnabled pnc

  ncRpcConfig <- makeRpcConfig $ (pncRpcConfig pnc){nodeSocketPath=ncSocketPath socketConfig}

  return $ NodeConfiguration
             { ncConfigFile = configFile
             , ncTopologyFile = topologyFile
             , ncDatabaseFile = databaseFile
             , ncProtocolFiles = protocolFiles
             , ncValidateDB = validateDB
             , ncShutdownConfig = shutdownConfig
             , ncStartAsNonProducingNode = startAsNonProducingNode
             , ncProtocolConfig = protocolConfig
             , ncSocketConfig = socketConfig
             , ncDiffusionMode = diffusionMode
             , ncExperimentalProtocolsEnabled = experimentalProtocols
             , ncMaxConcurrencyBulkSync = getLast $ pncMaxConcurrencyBulkSync pnc
             , ncMaxConcurrencyDeadline = getLast $ pncMaxConcurrencyDeadline pnc
             , ncTraceForwardSocket = getLast $ pncTraceForwardSocket pnc
             , ncMaybeMempoolCapacityOverride = getLast $ pncMaybeMempoolCapacityOverride pnc
             , ncLedgerDbConfig
             , ncProtocolIdleTimeout
             , ncTimeWaitTimeout
             , ncChainSyncIdleTimeout
             , ncMempoolTimeoutSoft
             , ncMempoolTimeoutHard
             , ncMempoolTimeoutCapacity
             , ncEgressPollInterval
             , ncAcceptedConnectionsLimit
             , ncDeadlineTargetOfRootPeers
             , ncDeadlineTargetOfKnownPeers
             , ncDeadlineTargetOfEstablishedPeers
             , ncDeadlineTargetOfActivePeers
             , ncDeadlineTargetOfKnownBigLedgerPeers
             , ncDeadlineTargetOfEstablishedBigLedgerPeers
             , ncDeadlineTargetOfActiveBigLedgerPeers
             , ncSyncTargetOfRootPeers
             , ncSyncTargetOfKnownPeers
             , ncSyncTargetOfEstablishedPeers
             , ncSyncTargetOfActivePeers
             , ncSyncTargetOfKnownBigLedgerPeers
             , ncSyncTargetOfEstablishedBigLedgerPeers
             , ncSyncTargetOfActiveBigLedgerPeers
             , ncMinBigLedgerPeersForTrustedState
             , ncPeerSharing
             , ncConsensusMode
             , ncGenesisConfig
             , ncResponderCoreAffinityPolicy
             , ncRpcConfig
             , ncTxSubmissionLogicVersion
             , ncTxSubmissionInitDelay
             }

ncProtocol :: NodeConfiguration -> Protocol
ncProtocol nc =
  case ncProtocolConfig nc of
    -- NodeProtocolConfigurationByron{}   -> ByronProtocol -- jky delete me
    -- NodeProtocolConfigurationShelley{} -> ShelleyProtocol -- jky delete me
    NodeProtocolConfigurationCardano{} -> CardanoProtocol

pncProtocol :: PartialNodeConfiguration -> Either Text Protocol
pncProtocol pnc =
  case pncProtocolConfig pnc of
    Last Nothing -> Left "Node protocol configuration not found"
    Last (Just NodeProtocolConfigurationCardano{}) -> Right CardanoProtocol
