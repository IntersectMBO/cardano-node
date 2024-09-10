{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Trace.Forward.Protocol.TraceObject.Examples
  ( traceObjectAcceptorApply
  , traceObjectForwarderCount
  ) where

import           Control.Concurrent.Class.MonadSTM.TVar
import           Control.Monad.Class.MonadSTM
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
          TokNonBlocking
          (NumberOfTraceObjects 1)
          $ \_reply -> return $ go (f acc) (pred n)

-- | A server which counts number received of 'MsgTraceObjectsRequest'.
traceObjectForwarderCount
  :: MonadSTM m
  => m (TraceObjectForwarder Int m Int)
traceObjectForwarderCount = do
  n <- newTVarIO 0
  return $
    TraceObjectForwarder
      { recvMsgDone = readTVarIO n
      , recvMsgTraceObjectsRequest =
          \blocking _numOfTO -> do
            atomically $ modifyTVar' n succ
            return ( case blocking of
                       TokBlocking    -> BlockingReply (NE.fromList [1, 2, 3])
                       TokNonBlocking -> NonBlockingReply [1, 2]
                   )
      }
