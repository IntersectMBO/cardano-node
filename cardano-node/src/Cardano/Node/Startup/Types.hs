{-# LANGUAGE DerivingStrategies #-}

module Cardano.Node.Startup.Types where

import qualified Cardano.Api as Api

import           Cardano.Network.Diffusion (CardanoLocalRootConfig)
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Protocol (ProtocolInstantiationError)
import           Cardano.Node.Types (PeerSnapshotFile)
import           Cardano.Slotting.Slot (SlotNo, WithOrigin)
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion,
                   BlockNodeToNodeVersion)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (NodeToClientVersion)
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), NodeToNodeVersion, PeerAdvertise)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (UseLedgerPeers)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency, WarmValency)

import           Prelude

import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Data.Word (Word64)
import qualified Network.Socket as Socket

data StartupTrace blk =
  -- | Log startup information.
  --
    StartupInfo
      [Socket.SockAddr]
      -- ^ node-to-node addresses
      (Maybe LocalSocketOrSocketInfo)
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

  -- | Log network configuration update warning.
  --
  | NetworkConfigUpdateWarning Text

  -- | Log network configuration update info.
  --
  | NetworkConfigUpdateInfo Text

  -- | Log peer-to-peer network configuration, either on startup or when its
  -- updated.
  --
  | NetworkConfig [(HotValency, WarmValency, Map RelayAccessPoint CardanoLocalRootConfig)]
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
  | LedgerPeerSnapshotLoaded (Either (UseLedgerPeers, WithOrigin SlotNo) (WithOrigin SlotNo))
  | MovedTopLevelOption String

data EnabledBlockForging
  = EnabledBlockForging
  | DisabledBlockForging
  | NotEffective
    -- ^ one needs to send `SIGHUP` after consensus
    -- initialised itself (especially after replying all
    -- blocks).
  deriving stock
    (Eq, Show)

data BasicInfoCommon = BasicInfoCommon {
    biConfigPath    :: FilePath
  , biNetworkMagic  :: NetworkMagic
  , biProtocol      :: Text
  , biVersion       :: Text
  , biCommit        :: Text
  , biNodeStartTime :: UTCTime
  }

-- Fields of this type are made strict to be sure no path from GC roots to genesis is retained
data BasicInfoShelleyBased = BasicInfoShelleyBased {
    bisEra               :: !Text
  , bisSystemStartTime   :: !UTCTime
  , bisSlotLength        :: !NominalDiffTime
  , bisEpochLength       :: !Word64
  , bisSlotsPerKESPeriod :: !Word64
  }

data BasicInfoByron = BasicInfoByron {
    bibSystemStartTime :: UTCTime
  , bibSlotLength      :: NominalDiffTime
  , bibEpochLength     :: Word64
  }

data BasicInfoNetwork = BasicInfoNetwork {
    niAddresses     :: [SocketOrSocketInfo]
  , niDiffusionMode :: DiffusionMode
  }