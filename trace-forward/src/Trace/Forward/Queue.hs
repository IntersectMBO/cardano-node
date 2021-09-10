{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Queue
  ( readItems
  , getTraceObjects
  ) where

import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad (unless)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word16)

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import qualified Trace.Forward.Protocol.Forwarder as Forwarder
import           Trace.Forward.Protocol.Type
import           Trace.Forward.Utils

readItems
  :: ForwarderConfiguration lo -- ^ The forwarder configuration.
  -> ForwardSink lo            -- ^ The sink contains the queue we read 'TraceObject's from.
  -> Forwarder.TraceForwarder lo IO ()
readItems config@ForwarderConfiguration{getNodeInfo} sink@ForwardSink{forwardQueue, wasUsed} =
  Forwarder.TraceForwarder
    { Forwarder.recvMsgNodeInfoRequest = do
        reply <- getNodeInfo
        return (reply, readItems config sink)
    , Forwarder.recvMsgTraceObjectsRequest = \blocking (NumberOfTraceObjects n) -> do
        replyList <-
          case blocking of
            TokBlocking -> do
              objs <- atomically $ getNTraceObjects n forwardQueue >>= \case
                []     -> retry -- No 'TraceObject's yet, just wait...
                (x:xs) -> return $ x NE.:| xs
              atomically . modifyTVar' wasUsed . const $ True
              return $ BlockingReply objs
            TokNonBlocking -> do
              objs <- atomically $ getNTraceObjects n forwardQueue
              unless (null objs) $
                atomically . modifyTVar' wasUsed . const $ True
              return $ NonBlockingReply objs
        return (replyList, readItems config sink)
    , Forwarder.recvMsgDone = return ()
    }

-- | Returns at most N 'TraceObject's from the queue.
getNTraceObjects
  :: Word16
  -> TVar (TBQueue lo)
  -> STM [lo]
getNTraceObjects 0 _ = return []
getNTraceObjects n q =
  readTVar q >>= tryReadTBQueue >>= \case
    Just lo' -> (lo' :) <$> getNTraceObjects (n - 1) q
    Nothing  -> return []

getTraceObjects
  :: BlockingReplyList blocking lo -- ^ The reply with list of 'TraceObject's.
  -> [lo]
getTraceObjects (BlockingReply neList)  = NE.toList neList
getTraceObjects (NonBlockingReply list) = list
