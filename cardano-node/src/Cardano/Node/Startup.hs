{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Node.Startup where

import qualified Cardano.Api as Api

import           Cardano.Git.Rev (gitRev)
import           Cardano.Ledger.Shelley.Genesis (sgSystemStart)
import           Cardano.Logging
import           Cardano.Node.Configuration.POM (NodeConfiguration (..), ncProtocol)
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Protocol (ProtocolInstantiationError)
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Types (PeerSnapshotFile)
import           Cardano.Slotting.Slot (SlotNo, WithOrigin)
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as WCT
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork (shelleyLedgerConfig)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.Ledger.Query (getSystemStart)
import           Ouroboros.Consensus.Node (pInfoConfig)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion,
                   BlockNodeToNodeVersion)
import           Ouroboros.Consensus.Shelley.Ledger.Ledger (shelleyLedgerGenesis)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (LocalAddress (..), LocalSocket,
                   NodeToClientVersion)
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), NodeToNodeVersion, PeerAdvertise)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (UseLedgerPeers)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency, LocalRootConfig, WarmValency)
import           Ouroboros.Network.Subscription.Dns (DnsSubscriptionTarget (..))
import           Ouroboros.Network.Subscription.Ip (IPSubscriptionTarget (..))

import           Prelude

import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Map.Strict (Map)
import           Data.Monoid (Last (..))
import           Data.Text (Text, pack)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Data.Version (showVersion)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Network.HostName (getHostName)
import qualified Network.Socket as Socket

import           Paths_cardano_node (version)

data StartupTrace blk =
  -- | Log startup information.
  --
    StartupInfo
      [SocketOrSocketInfo Socket.SockAddr Socket.SockAddr]
      -- ^ node-to-node addresses
      (Maybe (SocketOrSocketInfo LocalSocket LocalAddress))
      -- ^ node-to-client socket path
      (Map NodeToNodeVersion (BlockNodeToNodeVersion blk))
      -- ^ supported node-to-node versions
      (Map NodeToClientVersion (BlockNodeToClientVersion blk))
      -- ^ supported node-to-client versions

  -- | Log peer-to-peer diffusion mode
  | StartupP2PInfo DiffusionMode

  | StartupTime UTCTime

  | StartupNetworkMagic NetworkMagic

  | StartupSocketConfigError SocketConfigError

  | StartupDBValidation

  -- | Log that the block forging is being updated
  | BlockForgingUpdate EnabledBlockForging

  -- | Protocol instantiation error when updating block forging
  | BlockForgingUpdateError ProtocolInstantiationError

  -- | Mismatched block type
  | BlockForgingBlockTypeMismatch
       Api.SomeBlockType -- ^ expected
       Api.SomeBlockType -- ^ provided

  -- | Log that the network configuration is being updated.
  --
  | NetworkConfigUpdate

  -- | Re-configuration of network config is not supported.
  --
  | NetworkConfigUpdateUnsupported

  -- | Log network configuration update error.
  --
  | NetworkConfigUpdateError Text

  -- | Log peer-to-peer network configuration, either on startup or when its
  -- updated.
  --
  | NetworkConfig [(HotValency, WarmValency, Map RelayAccessPoint LocalRootConfig)]
                  (Map RelayAccessPoint PeerAdvertise)
                  UseLedgerPeers
                  (Maybe PeerSnapshotFile)

  -- | Warn when 'DisabledP2P' is set.
  | NonP2PWarning

  -- | Warn when 'ExperimentalProtocolsEnabled' is set and affects
  -- node-to-node protocol.
  --
  | WarningDevelopmentNodeToNodeVersions [NodeToNodeVersion]

  -- | Warn when 'ExperimentalProtocolsEnabled' is set and affects
  -- node-to-client protocol.
  --
  | WarningDevelopmentNodeToClientVersions [NodeToClientVersion]

  | BICommon BasicInfoCommon
  | BIShelley BasicInfoShelleyBased
  | BIByron BasicInfoByron
  | BINetwork BasicInfoNetwork
  | LedgerPeerSnapshotLoaded (WithOrigin SlotNo)

data EnabledBlockForging = EnabledBlockForging
                         | DisabledBlockForging
                         | NotEffective
                         -- ^ one needs to send `SIGHUP` after consensus
                         -- initialised itself (especially after replying all
                         -- blocks).
                         deriving (Eq, Show)

data BasicInfoCommon = BasicInfoCommon {
    biConfigPath    :: FilePath
  , biNetworkMagic  :: NetworkMagic
  , biProtocol      :: Text
  , biVersion       :: Text
  , biCommit        :: Text
  , biNodeStartTime :: UTCTime
  }

data BasicInfoShelleyBased = BasicInfoShelleyBased {
    bisEra               :: Text
  , bisSystemStartTime   :: UTCTime
  , bisSlotLength        :: NominalDiffTime
  , bisEpochLength       :: Word64
  , bisSlotsPerKESPeriod :: Word64
  }

data BasicInfoByron = BasicInfoByron {
    bibSystemStartTime :: UTCTime
  , bibSlotLength      :: NominalDiffTime
  , bibEpochLength     :: Word64
  }

data BasicInfoNetwork = BasicInfoNetwork {
    niAddresses     :: [SocketOrSocketInfo Socket.SockAddr Socket.SockAddr]
  , niDiffusionMode :: DiffusionMode
  , niDnsProducers  :: [DnsSubscriptionTarget]
  , niIpProducers   :: IPSubscriptionTarget
  }

-- | This information is taken from 'BasicInfoShelleyBased'. It is required for
--   'cardano-tracer' service (particularly, for RTView).
data NodeStartupInfo = NodeStartupInfo {
    suiEra               :: Text
  , suiSlotLength        :: NominalDiffTime
  , suiEpochLength       :: Word64
  , suiSlotsPerKESPeriod :: Word64
  } deriving (Eq, Generic, ToJSON, FromJSON, Show)

deriving instance (NFData NodeStartupInfo)

instance MetaTrace NodeStartupInfo where
  namespaceFor NodeStartupInfo {}  =
    Namespace [] ["NodeStartupInfo"]
  severityFor  (Namespace _ ["NodeStartupInfo"]) _ =
    Just Info
  severityFor _ns _ =
    Nothing
  documentFor  (Namespace _ ["NodeStartupInfo"]) = Just
    "Startup information about this node, required for RTView\
        \\n\
        \\n _suiEra_: Name of the current era. \
        \\n _suiSlotLength_: Slot length, in seconds. \
        \\n _suiEpochLength_: Epoch length, in slots. \
        \\n _suiSlotsPerKESPeriod_: KES period length, in slots."
  documentFor _ns =
     Nothing
  allNamespaces = [ Namespace [] ["NodeStartupInfo"]]
