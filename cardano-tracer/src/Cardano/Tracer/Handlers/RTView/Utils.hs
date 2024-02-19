{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Utils
  ( forAcceptedMetrics_
  , forAcceptedMetricsUI_
  , forConnected
  , forConnected_
  , forConnectedUI
  , forConnectedUI_
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Types

import           Control.Concurrent.STM.TVar (readTVarIO)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Graphics.UI.Threepenny.Core

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
