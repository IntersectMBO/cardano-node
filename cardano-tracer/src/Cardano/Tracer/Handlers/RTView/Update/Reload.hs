{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Reload
  ( updateUIAfterReload
  ) where

import           Control.Concurrent.Extra (Lock)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.List.NonEmpty (NonEmpty)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.Update.NodeInfo
import           Cardano.Tracer.Handlers.RTView.Update.Nodes
import           Cardano.Tracer.Types

updateUIAfterReload
  :: UI.Window
  -> ConnectedNodes
  -> DisplayedElements
  -> DataPointRequestors
  -> Lock
  -> NonEmpty LoggingParams
  -> Colors
  -> DatasetsIndices
  -> Errors
  -> UI.Timer
  -> UI.Timer
  -> UI ()
updateUIAfterReload window connectedNodes displayedElements dpRequestors currentDPLock
                    loggingConfig colors datasetIndices nodesErrors updateErrorsTimer
                    noNodesProgressTimer = do
  -- Ok, web-page was reload (i.e. it's the first update after DOM-rendering),
  -- so displayed state should be restored immediately.
  connected <- liftIO $ readTVarIO connectedNodes
  addColumnsForConnected
    window
    connected
    loggingConfig
    nodesErrors
    updateErrorsTimer
    displayedElements
  checkNoNodesState window connected noNodesProgressTimer
  askNSetNodeInfo window dpRequestors currentDPLock connected displayedElements
  addDatasetsForConnected window connected colors datasetIndices displayedElements
  liftIO $ updateDisplayedElements displayedElements connected
