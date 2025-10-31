{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Leadership
  ( updateLeadershipHistory
  ) where

import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types

import           Data.Text (isInfixOf)
import           Data.Time.Clock (UTCTime)

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
    x | "nodeCannotForge" `isInfixOf` x         -> updateNodeCannotForge
    -- Slot when this node forged last block.
    x | "forgedSlotLast" `isInfixOf` x          -> updateForgedSlotLast
    -- Slot when this node is leader.
    x | "Forge.node-is-leader" `isInfixOf` x    -> updateNodeIsLeader
    -- Slot when this node made leadership check and concludes it's not leader.
    x | "Forge.node-not-leader" `isInfixOf` x   -> updateNodeIsNotLeader
    -- Slot when invalid block was forged.
    x | "Forge.forged-invalid" `isInfixOf` x    -> updateForgedInvalidSlotLast
    -- Slot when the node adopted the block it forged.
    x | "Forge.adopted" `isInfixOf` x -> updateAdoptedSlotLast
    -- Slot when the node didn't adopted the block it forged, but the block was valid.
    x | "Forge.didnt-adopt" `isInfixOf` x       -> updateNotAdoptedSlotLast
    -- Slot when the leadership check is started.
    x | "Forge.about-to-lead" `isInfixOf` x     -> updateAboutToLeadSlotLast
    -- Slot when the leadership check is failed.
    x | "Forge.could-not-forge" `isInfixOf` x   -> updateCouldNotForgeSlotLast
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
