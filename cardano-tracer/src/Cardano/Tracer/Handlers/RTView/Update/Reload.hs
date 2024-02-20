{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Reload
  ( updateUIAfterReload
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.Update.NodeInfo
import           Cardano.Tracer.Handlers.RTView.Update.Nodes

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.List.NonEmpty (NonEmpty)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

updateUIAfterReload
  :: TracerEnv
  -> DisplayedElements
  -> NonEmpty LoggingParams
  -> Colors
  -> DatasetsIndices
  -> UI.Timer
  -> UI ()
updateUIAfterReload tracerEnv displayedElements loggingConfig colors
                    datasetIndices noNodesProgressTimer = do
  -- Ok, web-page was reload (i.e. it's the first update after DOM-rendering),
  -- so displayed state should be restored immediately.
  connected <- liftIO $ readTVarIO (teConnectedNodes tracerEnv)
  addColumnsForConnected
    tracerEnv
    connected
    loggingConfig
  checkNoNodesState connected noNodesProgressTimer
  askNSetNodeInfo tracerEnv connected displayedElements
  addDatasetsForConnected tracerEnv connected colors datasetIndices
  restoreLastHistoryOnAllCharts tracerEnv datasetIndices
  liftIO $
    updateDisplayedElements displayedElements connected
