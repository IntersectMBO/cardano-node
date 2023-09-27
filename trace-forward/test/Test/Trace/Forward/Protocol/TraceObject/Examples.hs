{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Trace.Forward.Protocol.TraceObject.Examples
  ( traceObjectAcceptorApply
  , traceObjectForwarderCount
  ) where

import qualified Data.List.NonEmpty as NE

import           Trace.Forward.Protocol.TraceObject.Acceptor
import           Trace.Forward.Protocol.TraceObject.Forwarder
import           Trace.Forward.Protocol.TraceObject.Type

traceObjectAcceptorApply
  :: forall m. Monad m
  => (Int -> Int)
  -> Int
  -> Int -- ^ count of number of requests
  -> TraceObjectAcceptor Int m Int
traceObjectAcceptorApply f = go
 where
  go :: Int -> Int -> TraceObjectAcceptor Int m Int
  go acc n
    | n <= 0 =
        SendMsgDone $ return acc
    | otherwise =
        SendMsgTraceObjectsRequest
          SingNonBlocking
          (NumberOfTraceObjects 1)
          $ \_reply -> return $ go (f acc) (pred n)

-- | A server which counts number received of 'MsgTraceObjectsRequest'.
--
traceObjectForwarderCount
  :: forall m. Monad m
  => TraceObjectForwarder Int m Int
traceObjectForwarderCount = go 0
 where
  go :: Int -> TraceObjectForwarder Int m Int
  go n =
    TraceObjectForwarder
      { recvMsgDone = return n
      , recvMsgTraceObjectsRequest =
          \blocking _numOfTO ->
            return ( case blocking of
                       SingBlocking    -> BlockingReply (NE.fromList [1, 2, 3])
                       SingNonBlocking -> NonBlockingReply [1, 2]
                   , go (succ n)
                   )
      }
