{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Utils
  ( module Cardano.Tracer.Handlers.RTView.Utils
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Types

import           Control.Concurrent.STM.TVar (readTVarIO)
import qualified Data.HashMap.Strict as HM (toList)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import           Data.Text as T (Text, pack)
import           System.Metrics (Store, Value (..), sampleAll)

import           Graphics.UI.Threepenny.Core

type MetricName  = Text
type MetricValue = Text
type MetricsList = [(MetricName, MetricValue)]


forConnected :: TracerEnv -> (NodeId -> IO b) -> IO [b]
forConnected TracerEnv{teConnectedNodes} action =
  mapM action . S.toList =<< readTVarIO teConnectedNodes

forConnected_ :: TracerEnv -> (NodeId -> IO ()) -> IO ()
forConnected_ TracerEnv{teConnectedNodes} action =
  mapM_ action =<< readTVarIO teConnectedNodes

forConnectedUI :: TracerEnv -> (NodeId -> UI b) -> UI [b]
forConnectedUI TracerEnv{teConnectedNodes} action =
  mapM action . S.toList =<< liftIO (readTVarIO teConnectedNodes)

forConnectedUI_ :: TracerEnv -> (NodeId -> UI ()) -> UI ()
forConnectedUI_ TracerEnv{teConnectedNodes} action =
  mapM_ action =<< liftIO (readTVarIO teConnectedNodes)

forAcceptedMetrics_
  :: TracerEnv
  -> ((NodeId, MetricsStores) -> IO ())
  -> IO ()
forAcceptedMetrics_ TracerEnv{teAcceptedMetrics} action =
  mapM_ action . M.toList =<< readTVarIO teAcceptedMetrics

forAcceptedMetricsUI_
  :: TracerEnv
  -> ((NodeId, MetricsStores) -> UI ())
  -> UI ()
forAcceptedMetricsUI_ TracerEnv{teAcceptedMetrics} action =
  mapM_ action . M.toList =<< liftIO (readTVarIO teAcceptedMetrics)

getListOfMetrics :: Store -> IO MetricsList
getListOfMetrics = fmap (mapMaybe metricsWeNeed . HM.toList) . sampleAll
 where
  metricsWeNeed (mName, mValue) =
    case mValue of
      Counter c -> Just (mName, T.pack $ show c)
      Gauge g   -> Just (mName, T.pack $ show g)
      Label l   -> Just (mName, l)
      _         -> Nothing -- 'ekg-forward' doesn't support 'Distribution' yet.
