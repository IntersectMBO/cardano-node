-- {-# Options_GHC -w #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Cardano.Logging.Types.DataPoint 
  ( NodePeers (..)
  -- , traceNodePeers
  , NodeInfo (..)
  , PeerT (..)
  -- , prepareNodeInfo
  , ppStatus
  , ppMaxSlotNo
  , ppPeer
  ) where


import           Cardano.Logging.Types --todo
import           Cardano.Logging
import           Control.DeepSeq (NFData)
import qualified Data.List as List
import qualified Data.Set as Set
import           Text.Printf (printf)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Ouroboros.Network.ConnectionId (remoteAddress)
import           Data.Aeson (ToJSON (..), Value (..), toJSON, (.=))
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Data.Text (Text)
import qualified Data.Text as Text
-- import           Ouroboros.Consensus.Block (Header)
import qualified Ouroboros.Network.AnchoredFragment as Net
import           Ouroboros.Network.Block (unSlotNo)
import qualified Ouroboros.Network.Block as Net
import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..), PeerFetchStatus (..))
import           Ouroboros.Network.NodeToNode (RemoteConnectionId)
import           Ouroboros.Network.SizeInBytes (SizeInBytes (..))

-- import           Cardano.Logging hiding (traceWith)
-- -- import           Cardano.Node.Orphans ()
-- -- import           Cardano.Node.Queries
-- import           Ouroboros.Consensus.Block (Header)
-- import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainSyncClientHandle,
--                    csCandidate, cschcMap, viewChainSyncState)
-- -- import           Ouroboros.Consensus.Util.Orphans ()
-- import qualified Ouroboros.Network.AnchoredFragment as Net
-- import           Ouroboros.Network.Block (unSlotNo)
-- import qualified Ouroboros.Network.Block as Net
-- import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
-- import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..),
--                    PeerFetchStatus (..), readFetchClientState)
-- import           Ouroboros.Network.ConnectionId (remoteAddress)
-- import           Ouroboros.Network.NodeToNode (RemoteAddress)

-- import           Control.Concurrent (threadDelay)
-- import           Control.Concurrent.Async
-- -- import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
-- import           Control.Monad (forever)
-- import           "contra-tracer" Control.Tracer
-- import           Data.Aeson (ToJSON (..), Value (..), toJSON, (.=))
-- import           Data.Functor ((<&>))
-- import qualified Data.List as List
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
-- import           Data.Text (Text)
-- import qualified Data.Text as Text
-- import           GHC.Conc (labelThread, myThreadId)

type PeerInfoPP = Text -- The result of 'ppPeer' function.

-- | This type contains an information about current peers of the node.
--   It will be asked by external applications as a DataPoint.
newtype NodePeers = NodePeers [PeerInfoPP]
  deriving stock    Generic
  deriving anyclass (NFData, ToJSON, FromJSON)

instance MetaTrace NodePeers where
  namespaceFor NodePeers {}  =
    Namespace [] ["NodePeers"]
  severityFor (Namespace _ ["NodePeers"]) _ =
    Just Info
  severityFor _ns _ =
    Nothing
  documentFor (Namespace _ ["NodePeers"]) =
    Just ""
  documentFor _ns =
    Nothing
  allNamespaces = [ Namespace [] ["NodePeers"]]


-- --------------------------------------------------------------------------------
-- -- Peers Tracer
-- --------------------------------------------------------------------------------

type PeerT :: Type -> Type
data PeerT blk = PeerT
    RemoteConnectionId
    (Net.AnchoredFragment (Header blk))
    (PeerFetchStatus (Header blk))
    (PeerFetchInFlight (Header blk))

-- traceNodePeers
--   :: Trace IO NodePeers
--   -> [PeerT blk]
--   -> IO ()
-- traceNodePeers tr ev = traceWith tr $ NodePeers (fmap ppPeer ev)


-- | NodeInfo

-- TODO: experimental

data NodeInfo = NodeInfo
  { niName            :: Text
  , niProtocol        :: Text
  , niVersion         :: Text
  , niCommit          :: Text
  , niStartTime       :: UTCTime
  , niSystemStartTime :: UTCTime
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (NFData, ToJSON, FromJSON)

instance MetaTrace NodeInfo where
  namespaceFor NodeInfo {}  =
    Namespace [] ["NodeInfo"]
  severityFor  (Namespace _ ["NodeInfo"]) _ =
    Just Info
  severityFor _ns _ =
    Nothing
  documentFor  (Namespace _ ["NodeInfo"]) = Just
    "Basic information about this node collected at startup\
        \\n\
        \\n _niName_: Name of the node. \
        \\n _niProtocol_: Protocol which this nodes uses. \
        \\n _niVersion_: Software version which this node is using. \
        \\n _niStartTime_: Start time of this node. \
        \\n _niSystemStartTime_: How long did the start of the node took."
  documentFor _ns =
     Nothing
  allNamespaces = [ Namespace [] ["NodeInfo"]]

-- -- | Prepare basic info about the node. This info will be sent to 'cardano-tracer'.
-- prepareNodeInfo
--   :: NodeConfiguration
--   -> SomeConsensusProtocol
--   -> TraceConfig
--   -> UTCTime
--   -> IO NodeInfo
-- prepareNodeInfo = undefined 
-- prepareNodeInfo nc (SomeConsensusProtocol whichP pForInfo) tc nodeStartTime = do
--   nodeName <- prepareNodeName
--   return $ NodeInfo
--     { niName            = nodeName
--     , niProtocol        = pack . show . ncProtocol $ nc
--     , niVersion         = pack . showVersion $ version
--     , niCommit          = $(gitRev)
--     , niStartTime       = nodeStartTime
--     , niSystemStartTime = systemStartTime
--     }
--  where
--   cfg = pInfoConfig $ fst $ Api.protocolInfo @IO pForInfo

--   systemStartTime :: UTCTime
--   systemStartTime =
--     case whichP of
--       Api.ByronBlockType ->
--         getSystemStartByron
--       Api.ShelleyBlockType ->
--         let DegenLedgerConfig cfgShelley = configLedger cfg
--         in getSystemStartShelley cfgShelley
--       Api.CardanoBlockType ->
--         let CardanoLedgerConfig _ cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage cfgConway = configLedger cfg
--         in minimum [ getSystemStartByron
--                    , getSystemStartShelley cfgShelley
--                    , getSystemStartShelley cfgAllegra
--                    , getSystemStartShelley cfgMary
--                    , getSystemStartShelley cfgAlonzo
--                    , getSystemStartShelley cfgBabbage
--                    , getSystemStartShelley cfgConway
--                    ]

--   getSystemStartByron = WCT.getSystemStart . getSystemStart . configBlock $ cfg
--   getSystemStartShelley = sgSystemStart . shelleyLedgerGenesis . shelleyLedgerConfig

--   prepareNodeName =
--     case tcNodeName tc of
--       Just aName -> return aName
--       Nothing -> do
--         -- The user didn't specify node's name in the configuration.
--         -- In this case we should form node's name as "host_port",
--         -- where 'host' is the machine's host name and 'port' is taken
--         -- from the '--port' CLI-parameter.

--         let suffix :: String
--             suffix
--               | SocketConfig{ncNodePortNumber = Last (Just port)} <- ncSocketConfig nc
--               = "_" <> show port
--               | otherwise
--               = ""

--         hostName <- getHostName
--         return (pack (hostName <> suffix))



