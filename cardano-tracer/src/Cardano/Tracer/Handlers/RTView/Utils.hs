{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Utils
  ( forAcceptedMetrics_
  , forAcceptedMetricsUI_
  , forConnected
  , forConnected_
  , forConnectedUI
  , forConnectedUI_
  ) where

import Data.Foldable
import Graphics.UI.Threepenny.Core

import Cardano.Tracer.Environment
import Cardano.Tracer.Types

import Control.Concurrent.STM
import StmContainers.Set qualified as STM.Set
import StmContainers.Map qualified as STM.Map
import ListT             qualified
import ListT (ListT)

forGeneral :: MonadIO io => (t -> ListT STM a) -> t -> (a -> io b) -> io [b]
forGeneral f tracerEnv action =
  traverse action =<< liftIO do
    atomically do
      ListT.toList (f tracerEnv)

forGeneral_ :: MonadIO io => (t -> ListT STM a) -> t -> (a -> io b) -> io ()
forGeneral_ f tracerEnv action =
  traverse_ action =<< liftIO do
    atomically do
      ListT.toList (f tracerEnv)

forConnected :: forall b. TracerEnv -> (NodeId -> IO b) -> IO [b]
forConnected = forGeneral (STM.Set.listT . teConnectedNodes)

forConnected_ :: TracerEnv -> (NodeId -> IO ()) -> IO ()
forConnected_ = forGeneral_ (STM.Set.listT . teConnectedNodes)

forConnectedUI :: TracerEnv -> (NodeId -> UI b) -> UI [b]
forConnectedUI = forGeneral (STM.Set.listT . teConnectedNodes)

forConnectedUI_ :: TracerEnv -> (NodeId -> UI ()) -> UI ()
forConnectedUI_ = forGeneral_ (STM.Set.listT . teConnectedNodes)

forAcceptedMetrics_
  :: TracerEnv
  -> (NodeId -> MetricsStores -> IO ())
  -> IO ()
forAcceptedMetrics_ env =
  forGeneral_ (STM.Map.listT . teAcceptedMetrics) env . uncurry

forAcceptedMetricsUI_
  :: TracerEnv
  -> (NodeId -> MetricsStores -> UI ())
  -> UI ()
forAcceptedMetricsUI_ env =
  forGeneral_ (STM.Map.listT . teAcceptedMetrics) env . uncurry
