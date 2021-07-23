{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Queue
  ( readItems
  , getTraceObjects
  ) where

import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TBQueue (TBQueue, tryReadTBQueue)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word16)

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import qualified Trace.Forward.Protocol.Forwarder as Forwarder
import           Trace.Forward.Protocol.Type

readItems
  :: ForwarderConfiguration lo -- ^ The forwarder configuration.
  -> TBQueue lo                -- ^ The queue we will read 'TraceObject's from.
  -> Forwarder.TraceForwarder lo IO ()
readItems config@ForwarderConfiguration{getNodeInfo} loQueue =
  Forwarder.TraceForwarder
    { Forwarder.recvMsgNodeInfoRequest = do
        reply <- getNodeInfo
        return (reply, readItems config loQueue)
    , Forwarder.recvMsgTraceObjectsRequest = \blocking (NumberOfTraceObjects n) -> do
        replyList <-
          case blocking of
            TokBlocking -> do
              objs <- atomically $ getNTraceObjects n loQueue >>= \case
                []     -> retry -- No 'TraceObject's yet, just wait...
                (x:xs) -> return $ x NE.:| xs
              return $ BlockingReply objs
            TokNonBlocking -> do
              objs <- atomically $ getNTraceObjects n loQueue
              return $ NonBlockingReply objs
        return (replyList, readItems config loQueue)
    , Forwarder.recvMsgDone = return ()
    }

-- | Returns at most N 'TraceObject's from the queue.
getNTraceObjects
  :: Word16
  -> TBQueue lo
  -> STM [lo]
getNTraceObjects 0 _ = return []
getNTraceObjects n q =
  tryReadTBQueue q >>= \case
    Just lo' -> (lo' :) <$> getNTraceObjects (n - 1) q
    Nothing  -> return []

getTraceObjects
  :: BlockingReplyList blocking lo -- ^ The reply with list of 'TraceObject's.
  -> [lo]
getTraceObjects (BlockingReply neList)  = NE.toList neList
getTraceObjects (NonBlockingReply list) = list
