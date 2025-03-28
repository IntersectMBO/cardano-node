{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.NodeState
  ( NodeStateWrapper (..)
  , askNSetNodeState
  ) where

-- import           Cardano.Node.Tracing.StateRep
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.Utils
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types

import           Control.Monad.Extra (whenJustM)
import           Data.Text (pack)
import           Text.Printf (printf)

import           Graphics.UI.Threepenny.Core (UI, liftIO)

-- | There is 'NodeState' datapoint, it contains different information
--   about the current state of the node. For example, its sync progress.
askNSetNodeState
  :: TracerEnv
  -> DisplayedElements
  -> UI ()
askNSetNodeState tracerEnv displayed =
  forConnectedUI_ tracerEnv $ \nodeId ->
    whenJustM (liftIO $ askDataPoint teDPRequestors teCurrentDPLock nodeId "NodeState") \(NodeStateWrapper syncPct) ->
      setSyncProgress nodeId syncPct
    -- whenJustM (liftIO $ askDataPoint teDPRequestors teCurrentDPLock nodeId "NodeState") $ \(ns :: NodeState) ->
    --   case ns of
    --     NodeAddBlock (AddedToCurrentChain _ _ syncPct) -> setSyncProgress nodeId syncPct
    --     _ -> return ()
 where
  TracerEnv{teDPRequestors, teCurrentDPLock} = tracerEnv

  setSyncProgress :: NodeId -> Double -> UI ()
  setSyncProgress nodeId@(NodeId anId) syncPct = do
    let nodeSyncProgressElId = anId <> "__node-sync-progress"
    if syncPct < 100.0
      then setDisplayedValue nodeId displayed nodeSyncProgressElId $
             pack (printf "%.2f" syncPct) <> "&nbsp;%"
      else setTextAndClasses nodeSyncProgressElId "100&nbsp;%" "rt-view-percent-done"

-- | This is to avoid creating a dependency on `cardano-node's `NodeState'.
newtype NodeStateWrapper = NodeStateWrapper
  { getNodeStateWrapper :: Double }

instance FromJSON NodeStateWrapper where
  parseJSON :: Value -> Parser NodeStateWrapper
  parseJSON = withObject "NodeState" \obj -> do
    -- Check if this is a NodeAddBlock constructor
    tag <- obj .: "tag"
    guard (tag == "NodeAddBlock")
    -- Verify it's AddedToCurrentChain and extract the double
    contents <- obj .: "contents"
    case contents of
      [_,_,a] -> pure (NodeStateWrapper a)
      _ -> fail "Expected AddedToCurrentChain object"
