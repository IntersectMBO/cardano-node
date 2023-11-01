{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Acceptors.Utils
  ( notifyAboutNodeDisconnected
  , prepareDataPointRequestor
  , prepareMetricsStores
  , removeDisconnectedNode
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import Data.Bimap qualified as BM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import System.Metrics qualified as EKG

import Ouroboros.Network.Snocket (LocalAddress)
import Ouroboros.Network.Socket (ConnectionId (..))
import System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)
import Trace.Forward.Utils.DataPoint (DataPointRequestor, initDataPointRequestor)

import Cardano.Logging (SeverityS (..))

import Cardano.Tracer.Environment
import Cardano.Tracer.Handlers.RTView.Notifications.Types
import Cardano.Tracer.Handlers.RTView.Notifications.Utils
import Cardano.Tracer.Types
import Cardano.Tracer.Utils

import ListT qualified 
import StmContainers.Map   qualified as STM.Map
import StmContainers.Set   qualified as STM.Set
import StmContainers.Bimap qualified as STM.Bimap

prepareDataPointRequestor
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO DataPointRequestor
prepareDataPointRequestor TracerEnv{teConnectedNodes, teDPRequestors} connId = do
  addConnectedNode teConnectedNodes connId
  dpRequestor <- initDataPointRequestor
  atomically do
    STM.Map.insert dpRequestor (connIdToNodeId connId) teDPRequestors
  return dpRequestor

prepareMetricsStores
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO (EKG.Store, TVar MetricsLocalStore)
prepareMetricsStores TracerEnv{teConnectedNodes, teAcceptedMetrics} connId = do
  addConnectedNode teConnectedNodes connId
  storesForNewNode <- (,) <$> EKG.newStore
                          <*> newTVarIO emptyMetricsLocalStore
  atomically do
    STM.Map.insert storesForNewNode (connIdToNodeId connId) teAcceptedMetrics
  return storesForNewNode

addConnectedNode
  :: ConnectedNodes
  -> ConnectionId LocalAddress
  -> IO ()
addConnectedNode connectedNodes connId = atomically do
  STM.Set.insert (connIdToNodeId connId) connectedNodes

-- | This handler is called when 'runPeer' function throws an exception,
--   which means that there is a problem with network connection.
removeDisconnectedNode
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO ()
removeDisconnectedNode tracerEnv connId =
  atomically do
    -- Remove all the stuff related to disconnected node.
    STM.Set.delete       nodeId teConnectedNodes
    STM.Bimap.deleteLeft nodeId teConnectedNodesNames
    STM.Map.delete       nodeId teAcceptedMetrics
    STM.Map.delete       nodeId teDPRequestors
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
