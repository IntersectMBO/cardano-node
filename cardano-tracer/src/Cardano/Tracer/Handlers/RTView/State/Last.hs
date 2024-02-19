module Cardano.Tracer.Handlers.RTView.State.Last
  ( LastResourcesForNode (..)
  , LastResources
  , addNullResources
  , initLastResources
  , updateLastResources
  ) where

import           Cardano.Tracer.Types (NodeId)

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Word (Word64)

-- | We have to store last received metric to be able to calculate
--   the value based on next received metric. For example, calculate
--   "CPU usage percent" based on received CPU ticks.
--   That value will be stored in corresponding 'HistoricalData' for
--   rendering on the corresponding chart.

data LastResourcesForNode = LastResourcesForNode
  { cpuLastTicks    :: !Int
  , cpuLastNS       :: !Word64
  , cpuGCLastTicks  :: !Int
  , cpuGCLastNS     :: !Word64
  , cpuAppLastTicks :: !Int
  , cpuAppLastNS    :: !Word64
  }

type LastResources = TVar (Map NodeId LastResourcesForNode)

initLastResources :: IO LastResources
initLastResources = newTVarIO M.empty

-- | For the first update only.
addNullResources
  :: LastResources
  -> NodeId
  -> IO ()
addNullResources lastResources nodeId = atomically $
  modifyTVar' lastResources $ M.insert nodeId nulls
 where
  nulls =
    LastResourcesForNode
      { cpuLastTicks    = 0
      , cpuLastNS       = 0
      , cpuGCLastTicks  = 0
      , cpuGCLastNS     = 0
      , cpuAppLastTicks = 0
      , cpuAppLastNS    = 0
      }

updateLastResources
  :: LastResources
  -> NodeId
  -> (LastResourcesForNode -> LastResourcesForNode)
  -> IO ()
updateLastResources lastResources nodeId updateIt = atomically $
  modifyTVar' lastResources $ M.adjust updateIt nodeId
