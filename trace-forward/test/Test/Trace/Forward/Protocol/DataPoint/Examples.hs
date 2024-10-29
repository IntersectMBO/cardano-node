{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use map" -}

module Test.Trace.Forward.Protocol.DataPoint.Examples
  ( dataPointAcceptorApply
  , dataPointForwarderCount
  ) where

import           Control.Concurrent.Class.MonadSTM.TVar
import           Control.Monad.Class.MonadSTM

import           Trace.Forward.Protocol.DataPoint.Acceptor
import           Trace.Forward.Protocol.DataPoint.Forwarder
import           Trace.Forward.Protocol.DataPoint.Type

dataPointAcceptorApply
  :: forall m. Monad m
  => (Int -> Int)
  -> Int
  -> Int -- ^ count of number of requests
  -> DataPointAcceptor m Int
dataPointAcceptorApply f = go
 where
  go :: Int -> Int -> DataPointAcceptor m Int
  go acc n
    | n <= 0 =
        SendMsgDone $ return acc
    | otherwise =
        SendMsgDataPointsRequest
          []
          $ \(_reply :: DataPointValues) -> return $ go (f acc) (pred n)

-- | A server which counts number received of 'MsgDataPointsRequest'.
dataPointForwarderCount
  :: MonadSTM m
  => m (DataPointForwarder m Int)
dataPointForwarderCount = do
  n <- newTVarIO 0
  return $
    DataPointForwarder
      { recvMsgDone = readTVarIO n
      , recvMsgDataPointsRequest =
          \(dpNames :: [DataPointName]) -> do
            atomically $ modifyTVar' n succ
            return ( zip dpNames (repeat Nothing)
                   )
      }
