{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Chain
  ( updateBlockchainHistory
  ) where

import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types

import           Data.Text (isInfixOf)
import           Data.Text.Read (double)
import           Data.Time.Clock (UTCTime)

updateBlockchainHistory
  :: NodeId
  -> BlockchainHistory
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateBlockchainHistory nodeId (ChainHistory cHistory) metricName metricValue now =
  case metricName of
    x | "density"     `isInfixOf` x  -> updateChainDensity
    x | "slotNum"     `isInfixOf` x  -> updateSlotNum
    x | "blockNum"    `isInfixOf` x  -> updateBlockNum
    x | "slotInEpoch" `isInfixOf` x  -> updateSlotInEpoch
    x | "epoch"       `isInfixOf` x  -> updateEpoch
    _ -> return ()
 where
  updateChainDensity =
    case double metricValue of
      Left _ -> return ()
      Right (density, _) -> do
        let !density' = 0.05 + density * 100.0
        addHistoricalData cHistory nodeId now ChainDensityData $ ValueD density'

  updateSlotNum =
    readValueI metricValue $ addHistoricalData cHistory nodeId now SlotNumData

  updateBlockNum =
    readValueI metricValue $ addHistoricalData cHistory nodeId now BlockNumData

  updateSlotInEpoch =
    readValueI metricValue $ addHistoricalData cHistory nodeId now SlotInEpochData

  updateEpoch =
    readValueI metricValue $ addHistoricalData cHistory nodeId now EpochData
