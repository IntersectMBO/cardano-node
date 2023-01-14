{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Trace.Forward.Protocol.DataPoint.Examples
  ( dataPointAcceptorApply
  , dataPointForwarderCount
  ) where

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
--
dataPointForwarderCount
  :: forall m. Monad m
  => DataPointForwarder m Int
dataPointForwarderCount = go 0
 where
  go n =
    DataPointForwarder
      { recvMsgDone = return n
      , recvMsgDataPointsRequest =
          \(dpNames :: [DataPointName]) ->
            return ( zip dpNames (repeat Nothing)
                   , go (succ n)
                   )
      }
