module Cardano.Tracer.Acceptors.Utils
  ( prepareDataPointAsker
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
import           Trace.Forward.Utils.DataPoint (DataPointAsker, initDataPointAsker)

import           Cardano.Tracer.Handlers.RTView.State.TraceObjects (SavedTraceObjects)
import           Cardano.Tracer.Types (AcceptedMetrics, ConnectedNodes, DataPointAskers)
import           Cardano.Tracer.Utils (connIdToNodeId)

prepareDataPointAsker
  :: ConnectedNodes
  -> DataPointAskers
  -> ConnectionId LocalAddress
  -> IO DataPointAsker
prepareDataPointAsker connectedNodes dpAskers connId = do
  addConnectedNode connectedNodes connId
  dpAsker <- initDataPointAsker
  atomically $
    modifyTVar' dpAskers $ M.insert (connIdToNodeId connId) dpAsker
  return dpAsker

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
  -> DataPointAskers
  -> SavedTraceObjects
  -> ConnectionId LocalAddress
  -> IO ()
removeDisconnectedNode connectedNodes acceptedMetrics dpAskers savedTO connId = atomically $ do
  -- Remove all the data related to disconnected node.
  modifyTVar' connectedNodes  $ S.delete nodeId
  modifyTVar' acceptedMetrics $ M.delete nodeId
  modifyTVar' dpAskers        $ M.delete nodeId
  modifyTVar' savedTO         $ M.delete nodeId
 where
  nodeId = connIdToNodeId connId
