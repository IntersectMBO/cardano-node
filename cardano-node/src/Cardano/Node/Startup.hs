{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Node.Startup where

import           Prelude

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Map (Map)
import           Data.Text (Text, pack)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Data.Version (showVersion)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Network.HostName (getHostName)
import qualified Network.Socket as Socket

import           Cardano.Ledger.Shelley.Genesis (sgSystemStart)

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
import           Ouroboros.Network.NodeToNode (DiffusionMode (..), NodeToNodeVersion)
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise)
import           Ouroboros.Network.Subscription.Dns (DnsSubscriptionTarget (..))
import           Ouroboros.Network.Subscription.Ip (IPSubscriptionTarget (..))

import           Cardano.Api.Protocol.Types (BlockType (..), protocolInfo)
import           Cardano.Logging
import           Cardano.Node.Configuration.Socket
import           Cardano.Node.Protocol.Types (Protocol (..), SomeConsensusProtocol (..))

import           Cardano.Git.Rev (gitRev)
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
  | NetworkConfig [(Int, Map RelayAccessPoint PeerAdvertise)]
                  [RelayAccessPoint]
                  UseLedgerAfter

  -- | Warn when 'EnableP2P' is set.
  | P2PWarning

  -- | Warn that peer-to-peer requires
  -- 'TestEnableDevelopmentNetworkProtocols' to be set.
  --
  | P2PWarningDevelopementNetworkProtocols

  -- | Warn when 'TestEnableDevelopmentNetworkProtocols' is set.
  --
  | WarningDevelopmentNetworkProtocols [NodeToNodeVersion] [NodeToClientVersion]

  | BICommon BasicInfoCommon
  | BIShelley BasicInfoShelleyBased
  | BIByron BasicInfoByron
  | BINetwork BasicInfoNetwork

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

data NodeInfo = NodeInfo
  { niName            :: Text
  , niProtocol        :: Text
  , niVersion         :: Text
  , niCommit          :: Text
  , niStartTime       :: UTCTime
  , niSystemStartTime :: UTCTime
  } deriving (Eq, Generic, ToJSON, FromJSON, Show)

docNodeInfoTraceEvent :: Documented NodeInfo
docNodeInfoTraceEvent = Documented [
    DocMsg
      ["NodeInfo"]
        []
        "Basic information about this node collected at startup\
        \\n\
        \\n _niName_: Name of the node. \
        \\n _niProtocol_: Protocol which this nodes uses. \
        \\n _niVersion_: Software version which this node is using. \
        \\n _niStartTime_: Start time of this node. \
        \\n _niSystemStartTime_: How long did the start of the node took."
  ]

-- | Prepare basic info about the node. This info will be sent to 'cardano-tracer'.
prepareNodeInfo
  :: Protocol
  -> SomeConsensusProtocol
  -> TraceConfig
  -> UTCTime
  -> IO NodeInfo
prepareNodeInfo ptcl (SomeConsensusProtocol whichP pForInfo) tc nodeStartTime = do
  nodeName <- prepareNodeName
  return $ NodeInfo
    { niName            = nodeName
    , niProtocol        = pack . show $ ptcl
    , niVersion         = pack . showVersion $ version
    , niCommit          = gitRev
    , niStartTime       = nodeStartTime
    , niSystemStartTime = systemStartTime
    }
 where
  cfg = pInfoConfig $ protocolInfo pForInfo

  systemStartTime :: UTCTime
  systemStartTime =
    case whichP of
      ByronBlockType ->
        getSystemStartByron
      ShelleyBlockType ->
        let DegenLedgerConfig cfgShelley = configLedger cfg
        in getSystemStartShelley cfgShelley
      CardanoBlockType ->
        let CardanoLedgerConfig _ cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage = configLedger cfg
        in minimum [ getSystemStartByron
                   , getSystemStartShelley cfgShelley
                   , getSystemStartShelley cfgAllegra
                   , getSystemStartShelley cfgMary
                   , getSystemStartShelley cfgAlonzo
                   , getSystemStartShelley cfgBabbage
                   ]

  getSystemStartByron = WCT.getSystemStart . getSystemStart . configBlock $ cfg
  getSystemStartShelley = sgSystemStart . shelleyLedgerGenesis . shelleyLedgerConfig

  prepareNodeName =
    case tcNodeName tc of
      Just aName -> return aName
      Nothing    -> pack <$> getHostName
