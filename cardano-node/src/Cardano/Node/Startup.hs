{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Node.Startup
  ( module Cardano.Node.Startup
  , module Cardano.Logging.Types.NodeInfo
  , module Cardano.Logging.Types.NodeStartupInfo
  ) where

import qualified Cardano.Api as Api

import           Cardano.Git.Rev (gitRev)
import           Cardano.Ledger.Shelley.Genesis (sgSystemStart)
import           Cardano.Logging
import           Cardano.Logging.Types.NodeInfo (NodeInfo (..))
import           Cardano.Logging.Types.NodeStartupInfo (NodeStartupInfo (..))
import           Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
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
import           Ouroboros.Network.NodeToClient (NodeToClientVersion)
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), NodeToNodeVersion, PeerAdvertise)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type (UseLedgerPeers)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency, LocalRootConfig, WarmValency)
import           Ouroboros.Network.Subscription.Dns (DnsSubscriptionTarget (..))
import           Ouroboros.Network.Subscription.Ip (IPSubscriptionTarget (..))

import           Prelude

import           Data.Map.Strict (Map)
import           Data.Monoid (Last (..))
import           Data.Text (Text, pack)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Data.Version (showVersion)
import           Data.Word (Word64)
import           Network.HostName (getHostName)
import qualified Network.Socket as Socket

import           Paths_cardano_node (version)

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

  -- | Log peer-to-peer network configuration, either on startup or when its
  -- updated.
  --
  | NetworkConfig [(HotValency, WarmValency, Map RelayAccessPoint (LocalRootConfig PeerTrustable))]
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
    niAddresses     :: [SocketOrSocketInfo]
  , niDiffusionMode :: DiffusionMode
  , niDnsProducers  :: [DnsSubscriptionTarget]
  , niIpProducers   :: IPSubscriptionTarget
  }

-- | Prepare basic info about the node. This info will be sent to 'cardano-tracer'.
prepareNodeInfo
  :: NodeConfiguration
  -> SomeConsensusProtocol
  -> TraceConfig
  -> UTCTime
  -> IO NodeInfo
prepareNodeInfo nc (SomeConsensusProtocol whichP pForInfo) tc nodeStartTime = do
  nodeName <- prepareNodeName
  return $ NodeInfo
    { niName            = nodeName
    , niProtocol        = pack . show . ncProtocol $ nc
    , niVersion         = pack . showVersion $ version
    , niCommit          = $(gitRev)
    , niStartTime       = nodeStartTime
    , niSystemStartTime = systemStartTime
    }
 where
  cfg = pInfoConfig $ fst $ Api.protocolInfo @IO pForInfo

  systemStartTime :: UTCTime
  systemStartTime =
    case whichP of
      Api.ByronBlockType ->
        getSystemStartByron
      Api.ShelleyBlockType ->
        let DegenLedgerConfig cfgShelley = configLedger cfg
        in getSystemStartShelley cfgShelley
      Api.CardanoBlockType ->
        let CardanoLedgerConfig _ cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage cfgConway = configLedger cfg
        in minimum [ getSystemStartByron
                   , getSystemStartShelley cfgShelley
                   , getSystemStartShelley cfgAllegra
                   , getSystemStartShelley cfgMary
                   , getSystemStartShelley cfgAlonzo
                   , getSystemStartShelley cfgBabbage
                   , getSystemStartShelley cfgConway
                   ]

  getSystemStartByron = WCT.getSystemStart . getSystemStart . configBlock $ cfg
  getSystemStartShelley = sgSystemStart . shelleyLedgerGenesis . shelleyLedgerConfig

  prepareNodeName =
    case tcNodeName tc of
      Just aName -> return aName
      Nothing -> do
        -- The user didn't specify node's name in the configuration.
        -- In this case we should form node's name as "host_port",
        -- where 'host' is the machine's host name and 'port' is taken
        -- from the '--port' CLI-parameter.

        let suffix :: String
            suffix
              | SocketConfig{ncNodePortNumber = Last (Just port)} <- ncSocketConfig nc
              = "_" <> show port
              | otherwise
              = ""

        hostName <- getHostName
        return (pack (hostName <> suffix))
