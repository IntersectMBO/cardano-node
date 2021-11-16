module Cardano.Tracer.Acceptors.Utils
  ( prepareDataPointAsker
  , prepareMetricsStores
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVarIO)
import           Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified System.Metrics as EKG

import           Ouroboros.Network.Socket (ConnectionId (..))

import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)

import           Trace.Forward.Utils.DataPoint (DataPointAsker, initDataPointAsker)

import           Cardano.Tracer.Types

prepareDataPointAsker
  :: Show addr
  => DataPointAskers
  -> ConnectionId addr
  -> IO DataPointAsker
prepareDataPointAsker dpAskers connId = do
  let nodeId = connIdToNodeId connId
  dpAsker <- initDataPointAsker
  atomically $ modifyTVar' dpAskers $ \askers ->
    if nodeId `M.member` askers
      then M.adjust (const dpAsker) nodeId askers
      else M.insert nodeId dpAsker askers
  return dpAsker

prepareMetricsStores
  :: Show addr
  => AcceptedMetrics
  -> ConnectionId addr
  -> IO (EKG.Store, TVar MetricsLocalStore)
prepareMetricsStores acceptedMetrics connId = do
  let nodeId = connIdToNodeId connId
  prepareAcceptedMetricsForNewNode nodeId
  metrics <- readTVarIO acceptedMetrics
  return $ metrics M.! nodeId
 where
  prepareAcceptedMetricsForNewNode nodeId = do
    metrics <- readTVarIO acceptedMetrics
    unless (nodeId `M.member` metrics) $ do
      storesForNewNode <-
        (,) <$> EKG.newStore
            <*> newTVarIO emptyMetricsLocalStore
      atomically $ modifyTVar' acceptedMetrics $ M.insert nodeId storesForNewNode
