{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Transactions
  ( updateTransactionsHistory
  ) where

import           Data.Time.Clock
import           Data.Text.Read

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Types

updateTransactionsHistory
  :: NodeId
  -> TransactionsHistory
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateTransactionsHistory nodeId (TXHistory tHistory) metricName metricValue now =
  case metricName of
    "cardano.node.txsProcessedNum" -> updateTxsProcessedNum
    "cardano.node.mempoolBytes"    -> updateMempoolBytes
    "cardano.node.txsInMempool"    -> updateTxsInMempool
    _ -> return ()
 where
  updateTxsProcessedNum =
    readValueI metricValue $ addHistoricalData tHistory nodeId now TxsProcessedNumData

  updateTxsInMempool =
    readValueI metricValue $ addHistoricalData tHistory nodeId now TxsInMempoolData

  updateMempoolBytes =
    case decimal metricValue of
      Left _ -> return ()
      Right (mempoolBytes :: Int, _) -> do
        let !mempoolInMB = fromIntegral mempoolBytes / 1024 / 1024 :: Double
        addHistoricalData tHistory nodeId now MempoolBytesData $ ValueD mempoolInMB
