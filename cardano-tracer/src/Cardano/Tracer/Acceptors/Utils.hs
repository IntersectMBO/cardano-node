{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Acceptors.Utils
  ( notifyAboutNodeDisconnected
  , prepareDataPointRequestor
  , prepareMetricsStores
  , removeDisconnectedNode
  ) where

import           Cardano.Logging (SeverityS (..))
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.Notifications.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils
import           Ouroboros.Network.Snocket (LocalAddress)
import           Ouroboros.Network.Socket (ConnectionId (..))

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import qualified Data.Bimap as BM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import qualified System.Metrics as EKG
import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)

import           Trace.Forward.Utils.DataPoint (DataPointRequestor, initDataPointRequestor)

prepareDataPointRequestor
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO DataPointRequestor
prepareDataPointRequestor TracerEnv{teConnectedNodes, teDPRequestors} connId = do
  addConnectedNode teConnectedNodes connId
  dpRequestor <- initDataPointRequestor
  atomically $
    modifyTVar' teDPRequestors $ M.insert (connIdToNodeId connId) dpRequestor
  return dpRequestor

prepareMetricsStores
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO (EKG.Store, TVar MetricsLocalStore)
prepareMetricsStores TracerEnv{teConnectedNodes, teAcceptedMetrics} connId = do
  addConnectedNode teConnectedNodes connId
  storesForNewNode <- (,) <$> EKG.newStore
                          <*> newTVarIO emptyMetricsLocalStore
  atomically $
    modifyTVar' teAcceptedMetrics $ M.insert (connIdToNodeId connId) storesForNewNode
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
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO ()
removeDisconnectedNode tracerEnv connId =
  -- Remove all the stuff related to disconnected node.
  atomically $ do
    modifyTVar' teConnectedNodes      $ S.delete  nodeId
    modifyTVar' teConnectedNodesNames $ BM.delete nodeId
    modifyTVar' teAcceptedMetrics     $ M.delete  nodeId
    modifyTVar' teDPRequestors        $ M.delete  nodeId
 where
  TracerEnv{teConnectedNodes, teConnectedNodesNames, teAcceptedMetrics, teDPRequestors} = tracerEnv
  nodeId = connIdToNodeId connId

notifyAboutNodeDisconnected
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO ()
notifyAboutNodeDisconnected TracerEnv{teEventsQueues} connId = do
  now <- systemToUTCTime <$> getSystemTime
  addNewEvent teEventsQueues EventNodeDisconnected $ Event nodeId now Warning msg
 where
  nodeId = connIdToNodeId connId
  msg = "Node is disconnected"
