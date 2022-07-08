{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Leadership
  ( updateLeadershipHistory
  ) where

import           Data.Time.Clock (UTCTime)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Types

updateLeadershipHistory
  :: NodeId
  -> BlockchainHistory
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateLeadershipHistory nodeId (ChainHistory cHistory) metricName metricValue now =
  case metricName of
    -- Slot when the node was a leader, but couldn't forge the block.
    "Forge.NodeCannotForge"         -> updateNodeCannotForge
    -- Slot when this node forged last block.
    "Forge.ForgedSlotLast"          -> updateForgedSlotLast
    -- Slot when this node is leader.
    "Forge.NodeIsLeader"            -> updateNodeIsLeader
    -- Slot when this node made leadership check and concludes it's not leader.
    "Forge.NodeNotLeader"           -> updateNodeIsNotLeader
    -- Slot when invalid block was forged.
    "Forge.ForgedInvalidSlotLast"   -> updateForgedInvalidSlotLast
    -- Slot when the node adopted the block it forged.
    "Forge.AdoptedOwnBlockSlotLast" -> updateAdoptedSlotLast
    -- Slot when the node didn't adopted the block it forged, but the block was valid.
    "Forge.NotAdoptedSlotLast"      -> updateNotAdoptedSlotLast
    -- Slot when the leadership check is started.
    "Forge.AboutToLeadSlotLast"     -> updateAboutToLeadSlotLast
    -- Slot when the leadership check is failed.
    "Forge.CouldNotForgeSlotLast"   -> updateCouldNotForgeSlotLast
    _ -> return ()
 where
  updateNodeCannotForge =
    readValueI metricValue $ addHistoricalData cHistory nodeId now NodeCannotForgeData

  updateForgedSlotLast =
    readValueI metricValue $ addHistoricalData cHistory nodeId now ForgedSlotLastData

  updateNodeIsLeader =
    readValueI metricValue $ addHistoricalData cHistory nodeId now NodeIsLeaderData

  updateNodeIsNotLeader =
    readValueI metricValue $ addHistoricalData cHistory nodeId now NodeIsNotLeaderData

  updateForgedInvalidSlotLast =
    readValueI metricValue $ addHistoricalData cHistory nodeId now ForgedInvalidSlotLastData

  updateAdoptedSlotLast =
    readValueI metricValue $ addHistoricalData cHistory nodeId now AdoptedSlotLastData

  updateNotAdoptedSlotLast =
    readValueI metricValue $ addHistoricalData cHistory nodeId now NotAdoptedSlotLastData

  updateAboutToLeadSlotLast =
    readValueI metricValue $ addHistoricalData cHistory nodeId now AboutToLeadSlotLastData

  updateCouldNotForgeSlotLast =
    readValueI metricValue $ addHistoricalData cHistory nodeId now CouldNotForgeSlotLastData
