{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.NodeState
  ( NodeStateWrapper (..)
  , askNSetNodeState
  ) where

-- import           Cardano.Node.Tracing.StateRep
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.Utils
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types

import           Control.Monad.Extra (whenJustM)
import           Data.Text (Text, pack)
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
--
-- Before: Pattern matching on `NodeState' to access a single Double:
--
-- @
--   NodeAddBlock (AddedToCurrentChain _ _ syncPct) -> setSyncProgress nodeId syncPct
--   _ -> return ()
-- @
--
-- Now: We pattern match on a newtype wrapper for a `Double' and parse
-- it from a JSON object if it matches the serialization of
-- `NodeAddBlock (AddedToCurrentChain _ _ syncPct)'.
--
-- @
--   \(NodeStateWrapper syncPct) -> setSyncProgress nodeId syncPct
-- @
newtype NodeStateWrapper = NodeStateWrapper
  { getNodeStateWrapper :: Double }

instance FromJSON NodeStateWrapper where
  parseJSON :: Value -> Parser NodeStateWrapper
  parseJSON = withObject "NodeState" \obj -> do
    -- Check if this is a NodeAddBlock constructor, verify that it's
    -- AddedToCurrentChain and extract the Double.
    "NodeAddBlock" :: Text <- obj .: "tag"
    [_, _, double] <- obj .: "contents"
    pure (NodeStateWrapper double)
