{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.RTView.Update.Reload
  ( updateUIAfterReload
  ) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set qualified as Set
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core

import Cardano.Tracer.Types (NodeId)
import Cardano.Tracer.Utils (fromSTMSet)
import Cardano.Tracer.Configuration
import Cardano.Tracer.Environment
import Cardano.Tracer.Handlers.RTView.State.Displayed
import Cardano.Tracer.Handlers.RTView.UI.Charts
import Cardano.Tracer.Handlers.RTView.UI.Types
import Cardano.Tracer.Handlers.RTView.Update.NodeInfo
import Cardano.Tracer.Handlers.RTView.Update.Nodes

import Control.Concurrent.STM
import ListT   qualified
import StmContainers.Set qualified as STM.Set

updateUIAfterReload
  :: TracerEnv
  -> DisplayedElements
  -> NonEmpty LoggingParams
  -> Colors
  -> DatasetsIndices
  -> UI.Timer
  -> UI ()
updateUIAfterReload tracerEnv@TracerEnv{teConnectedNodes} displayedElements loggingConfig colors
          datasetIndices noNodesProgressTimer = do
  -- Ok, web-page was reload (i.e. it's the first update after DOM-rendering),
  -- so displayed state should be restored immediately.
  connected :: Set NodeId <- liftIO $ atomically do 
    fromSTMSet teConnectedNodes
  addColumnsForConnected
    tracerEnv
    connected
    loggingConfig
  checkNoNodesState connected noNodesProgressTimer
  askNSetNodeInfo tracerEnv connected displayedElements
  addDatasetsForConnected tracerEnv connected colors datasetIndices
  restoreLastHistoryOnAllCharts tracerEnv datasetIndices
  liftIO do
    updateDisplayedElements displayedElements connected
