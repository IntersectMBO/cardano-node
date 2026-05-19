{-# LANGUAGE MultiWayIf #-}
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
  if
    -- Slot when the node was a leader, but couldn't forge the block.
    | "nodeCannotForge" `isInfixOf` metricName       -> updateNodeCannotForge
    -- Slot when this node forged last block.
    | "forgedSlotLast" `isInfixOf` metricName        -> updateForgedSlotLast
    -- Slot when this node is leader.
    | "Forge.node-is-leader" `isInfixOf` metricName  -> updateNodeIsLeader
    -- Slot when this node made leadership check and concludes it's not leader.
    | "Forge.node-not-leader" `isInfixOf` metricName -> updateNodeIsNotLeader
    -- Slot when invalid block was forged.
    | "Forge.forged-invalid" `isInfixOf` metricName  -> updateForgedInvalidSlotLast
    -- Slot when the node adopted the block it forged.
    | "Forge.adopted" `isInfixOf` metricName         -> updateAdoptedSlotLast
    -- Slot when the node didn't adopted the block it forged, but the block was valid.
    | "Forge.didnt-adopt" `isInfixOf` metricName     -> updateNotAdoptedSlotLast
    -- Slot when the leadership check is started.
    | "Forge.about-to-lead" `isInfixOf` metricName   -> updateAboutToLeadSlotLast
    -- Slot when the leadership check is failed.
    | "Forge.could-not-forge" `isInfixOf` metricName -> updateCouldNotForgeSlotLast
    | otherwise                                      -> return ()
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
