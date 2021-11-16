{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Types
  ( AcceptedMetrics
  , AcceptedNodeInfo
  , DataPointAskers
  , Metrics
  , NodeId (..)
  , NodeInfo (..)
  , connIdToNodeId
  , initAcceptedMetrics
  , initAcceptedNodeInfo
  , initDataPointAskers
  , printNodeFullId
  ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import qualified System.Metrics as EKG

import           Ouroboros.Network.Socket (ConnectionId (..))

import           Trace.Forward.Utils.DataPoint (DataPointAsker)

import           System.Metrics.Store.Acceptor (MetricsLocalStore)

-- | Unique identifier of the node, based on 'remoteAddress' from 'ConnectionId',
--   please see 'ouroboros-network'.
newtype NodeId = NodeId Text
  deriving (Eq, Ord, Show)

-- | We have to create EKG.Store and MetricsLocalStore
--   to keep all the metrics accepted from the node.
type Metrics = (EKG.Store, TVar MetricsLocalStore)

type AcceptedMetrics = TVar (Map NodeId Metrics)

-- | We have to store 'DataPointAsker's to be able to ask particular node for some 'DataPoint's.
type DataPointAskers = TVar (Map NodeId DataPointAsker)

-- | TMP!
data NodeInfo = NodeInfo
  { niName            :: !Text
  , niProtocol        :: !Text
  , niVersion         :: !Text
  , niCommit          :: !Text
  , niStartTime       :: !UTCTime
  , niSystemStartTime :: !UTCTime
  }

-- | 'NodeInfo' type is provided by the node (as one of 'DataPoint's)
--   and contains important information about the node.
--
--   Please note that 'NodeInfo' should be asked ASAP after the node is connected,
--   because all the parts of 'cardano-tracer' need it.
--
--   If the node is disconnected, corresponding 'NodeInfo' will be deleted,
--   so 'AcceptedNodeInfo' is used as a "source of truth" about currently
--   connected nodes.
type AcceptedNodeInfo = TVar (Map NodeId NodeInfo)

connIdToNodeId :: Show addr => ConnectionId addr -> NodeId
connIdToNodeId ConnectionId{remoteAddress} = NodeId preparedAddress
 where
  -- We have to remove "wrong" symbols from 'NodeId',
  -- to make it appropriate for the name of the subdirectory.
  preparedAddress =
      T.replace "LocalAddress" "" -- There are only local addresses by design.
    . T.replace " " "-"
    . T.replace "\"" ""
    . T.replace "/" "-"
    . T.pack
    $ show remoteAddress

printNodeFullId :: Text -> NodeId -> Text
printNodeFullId ""       (NodeId p) = T.drop 2 p -- In this case, '--' in the beginning is useless.
printNodeFullId nodeName (NodeId p) = nodeName <> p

initAcceptedMetrics :: IO AcceptedMetrics
initAcceptedMetrics = newTVarIO M.empty

initAcceptedNodeInfo :: IO AcceptedNodeInfo
initAcceptedNodeInfo = newTVarIO M.empty

initDataPointAskers :: IO DataPointAskers
initDataPointAskers = newTVarIO M.empty
