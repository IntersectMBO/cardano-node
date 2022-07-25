{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Reload
  ( updateUIAfterReload
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.List.NonEmpty (NonEmpty)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Handlers.RTView.Update.NodeInfo
import           Cardano.Tracer.Handlers.RTView.Update.Nodes

updateUIAfterReload
  :: TracerEnv
  -> DisplayedElements
  -> NonEmpty LoggingParams
  -> Colors
  -> DatasetsIndices
  -> Errors
  -> UI.Timer
  -> UI.Timer
  -> UI ()
updateUIAfterReload tracerEnv displayedElements loggingConfig colors datasetIndices
                    nodesErrors updateErrorsTimer noNodesProgressTimer = do
  -- Ok, web-page was reload (i.e. it's the first update after DOM-rendering),
  -- so displayed state should be restored immediately.
  connected <- liftIO $ readTVarIO (teConnectedNodes tracerEnv)
  addColumnsForConnected
    connected
    loggingConfig
    nodesErrors
    updateErrorsTimer
    displayedElements
  checkNoNodesState connected noNodesProgressTimer
  askNSetNodeInfo tracerEnv connected displayedElements
  addDatasetsForConnected connected colors datasetIndices displayedElements
  liftIO $ do
    restoreHistoryFromBackup tracerEnv connected
    updateDisplayedElements displayedElements connected
