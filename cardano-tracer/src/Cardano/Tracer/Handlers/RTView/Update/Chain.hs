{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Chain
  ( updateBlockchainHistory
  ) where

import           Data.Text.Read (double)
import           Data.Time.Clock (UTCTime)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Types

updateBlockchainHistory
  :: NodeId
  -> BlockchainHistory
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateBlockchainHistory nodeId (ChainHistory cHistory) metricName metricValue now =
  case metricName of
    "cardano.node.density"     -> updateChainDensity
    "cardano.node.slotNum"     -> updateSlotNum
    "cardano.node.blockNum"    -> updateBlockNum
    "cardano.node.slotInEpoch" -> updateSlotInEpoch
    "cardano.node.epoch"       -> updateEpoch
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
