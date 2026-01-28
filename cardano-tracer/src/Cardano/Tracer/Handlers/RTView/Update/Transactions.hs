{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Transactions
  ( updateTransactionsHistory
  ) where

import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Utils
import           Cardano.Tracer.Types

import           Data.Text (isInfixOf)
import           Data.Text.Read (decimal)
import           Data.Time.Clock (UTCTime)

updateTransactionsHistory
  :: NodeId
  -> TransactionsHistory
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateTransactionsHistory nodeId (TXHistory tHistory) metricName metricValue now =
  if
    | "txsProcessedNum" `isInfixOf` metricName -> updateTxsProcessedNum
    | "mempoolBytes" `isInfixOf` metricName    -> updateMempoolBytes
    | "txsInMempool" `isInfixOf` metricName    -> updateTxsInMempool
    | otherwise                                -> return ()
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
