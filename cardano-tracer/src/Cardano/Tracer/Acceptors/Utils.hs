module Cardano.Tracer.Acceptors.Utils
  ( prepareDataPointRequestor
  , prepareMetricsStores
  , removeDisconnectedNode
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified System.Metrics as EKG

import           Ouroboros.Network.Snocket (LocalAddress)
import           Ouroboros.Network.Socket (ConnectionId (..))
import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)
import           Trace.Forward.Utils.DataPoint (DataPointRequestor, initDataPointRequestor)

import           Cardano.Tracer.Types (AcceptedMetrics, ConnectedNodes, DataPointRequestors)
import           Cardano.Tracer.Utils (connIdToNodeId)

prepareDataPointRequestor
  :: ConnectedNodes
  -> DataPointRequestors
  -> ConnectionId LocalAddress
  -> IO DataPointRequestor
prepareDataPointRequestor connectedNodes dpRequestors connId = do
  addConnectedNode connectedNodes connId
  dpRequestor <- initDataPointRequestor
  atomically $
    modifyTVar' dpRequestors $ M.insert (connIdToNodeId connId) dpRequestor
  return dpRequestor

prepareMetricsStores
  :: ConnectedNodes
  -> AcceptedMetrics
  -> ConnectionId LocalAddress
  -> IO (EKG.Store, TVar MetricsLocalStore)
prepareMetricsStores connectedNodes acceptedMetrics connId = do
  addConnectedNode connectedNodes connId
  storesForNewNode <- (,) <$> EKG.newStore
                          <*> newTVarIO emptyMetricsLocalStore
  atomically $
    modifyTVar' acceptedMetrics $ M.insert (connIdToNodeId connId) storesForNewNode
  return storesForNewNode

addConnectedNode
  :: ConnectedNodes
  -> ConnectionId LocalAddress
  -> IO ()
addConnectedNode connectedNodes connId = atomically $
  modifyTVar' connectedNodes $ S.insert (connIdToNodeId connId)

-- | This handler is called when 'runPeer' function throws an exception,
--   which means that there is a problem with network connection.
removeDisconnectedNode
  :: ConnectedNodes
  -> AcceptedMetrics
  -> DataPointRequestors
  -> ConnectionId LocalAddress
  -> IO ()
removeDisconnectedNode connectedNodes acceptedMetrics dpRequestors connId = atomically $ do
  -- Remove all the stuff related to disconnected node.
  modifyTVar' connectedNodes  $ S.delete nodeId
  modifyTVar' acceptedMetrics $ M.delete nodeId
  modifyTVar' dpRequestors    $ M.delete nodeId
 where
  nodeId = connIdToNodeId connId
