{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Tracer.Acceptors.Utils
  ( prepareDataPointRequestor
  , prepareMetricsStores
  , removeDisconnectedNode
  , notifyAboutNodeDisconnected
  , store
  ) where

#if RTVIEW
import           Cardano.Logging (SeverityS (..))
#endif
import qualified Cardano.Timeseries.Component as Timeseries
import           Cardano.Timeseries.Domain.Types (MetricIdentifier)
import           Cardano.Tracer.Environment
#if RTVIEW
import           Cardano.Tracer.Handlers.Notifications.Types
import           Cardano.Tracer.Handlers.Notifications.Utils
#endif
import           Cardano.Tracer.Time (getTimeMs)
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils
import           Ouroboros.Network.Socket (ConnectionId (..))

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import qualified Data.Bimap as BM
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
#if RTVIEW
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
#endif
import qualified System.Metrics as EKG
import           System.Metrics.ReqResp
import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore,
                   storeMetrics)

import           Trace.Forward.Utils.DataPoint (DataPointRequestor, initDataPointRequestor)
import qualified Data.Text.Read as Text

prepareDataPointRequestor
  :: Show addr
  => TracerEnv
  -> ConnectionId addr
  -> IO DataPointRequestor
prepareDataPointRequestor TracerEnv{teConnectedNodes, teDPRequestors} connId = do
  addConnectedNode teConnectedNodes connId
  dpRequestor <- initDataPointRequestor
  atomically $
    modifyTVar' teDPRequestors $ M.insert (connIdToNodeId connId) dpRequestor
  return dpRequestor

prepareMetricsStores
  :: Show addr
  => TracerEnv
  -> ConnectionId addr
  -> IO (EKG.Store, TVar MetricsLocalStore)
prepareMetricsStores TracerEnv{teConnectedNodes, teAcceptedMetrics} connId = do
  addConnectedNode teConnectedNodes connId
  st <- EKG.newStore

  EKG.registerCounter "ekg.server_timestamp_ms" getTimeMs st
  storesForNewNode <- (st ,) <$> newTVarIO emptyMetricsLocalStore

  atomically do
    modifyTVar' teAcceptedMetrics do
      M.insert (connIdToNodeId connId) storesForNewNode

  return storesForNewNode

addConnectedNode
  :: Show addr
  => ConnectedNodes
  -> ConnectionId addr
  -> IO ()
addConnectedNode connectedNodes connId = atomically $
  modifyTVar' connectedNodes $ S.insert (connIdToNodeId connId)

-- | This handler is called when 'runPeer' function throws an exception,
--   which means that there is a problem with network connection.
removeDisconnectedNode
  :: Show addr
  => TracerEnv
  -> ConnectionId addr -- LocalAddress
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
  :: Show addr
  => TracerEnvRTView
  -> ConnectionId addr
  -> IO ()
#if RTVIEW
notifyAboutNodeDisconnected TracerEnvRTView{teEventsQueues} connId = do
  now <- systemToUTCTime <$> getSystemTime
  addNewEvent teEventsQueues EventNodeDisconnected $ Event nodeId now Warning msg
 where
  nodeId = connIdToNodeId connId
  msg = "Node is disconnected"
#else
notifyAboutNodeDisconnected _ _ = pure ()
#endif

store :: TracerEnv -> NodeId -> (EKG.Store, TVar MetricsLocalStore) -> Response -> IO ()
store tracerEnv (NodeId nodeId) (ekgStore, localStore) resp@(ResponseMetrics ms) = do
  storeMetrics resp ekgStore localStore
  for_ (teTimeseriesHandle tracerEnv) $ \h -> do
    ts <- getTimeMs
    Timeseries.insert h "node_id" nodeId (fromIntegral ts) (mapMaybe parseMetric ms)

  where
    numeralOnly :: MetricValue -> Maybe Double
    numeralOnly (GaugeValue x) = Just (fromIntegral x)
    numeralOnly (CounterValue x) = Just (fromIntegral x)
    -- If the label is readable as double, accept it
    numeralOnly (LabelValue (Text.double -> Right (x, ""))) = Just x
    numeralOnly _ = Nothing

    parseMetric :: (MetricName, MetricValue) -> Maybe (MetricIdentifier, Double)
    parseMetric (k, numeralOnly -> Just v) = Just (k, v)
    parseMetric _ = Nothing

