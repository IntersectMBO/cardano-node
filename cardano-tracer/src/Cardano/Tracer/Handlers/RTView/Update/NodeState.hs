{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.NodeState
  ( askNSetNodeState
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJustM)
import           Data.Text (pack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, liftIO)
import           Text.Printf (printf)

import           Cardano.Node.Tracing.StateRep

import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

-- | There is 'NodeState' datapoint, it contains different information
--   about the current state of the node. For example, its sync progress.
askNSetNodeState
  :: UI.Window
  -> ConnectedNodes
  -> DataPointRequestors
  -> DisplayedElements
  -> UI ()
askNSetNodeState _window connectedNodes dpRequestors displayed = do
  connected <- liftIO $ readTVarIO connectedNodes
  forM_ connected $ \nodeId@(NodeId _anId) ->
    whenJustM (liftIO $ askDataPoint dpRequestors nodeId "NodeState") $ \(ns :: NodeState) ->
      case ns of
        NodeAddBlock (AddedToCurrentChain _ _ syncPct) -> setSyncProgress nodeId syncPct
        _ -> return ()
 where
  setSyncProgress nodeId@(NodeId anId) syncPct = do
    let nodeSyncProgressElId = anId <> "__node-sync-progress"
    if syncPct < 100.0
      then setDisplayedValue nodeId displayed nodeSyncProgressElId $
             pack (printf "%.2f" syncPct) <> "&nbsp;%"
      else setTextAndClasses nodeSyncProgressElId "100&nbsp;%" "rt-view-percent-done"
