{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

--------------------------------------------------------------------------------

module Trace.Forward.Utils.TraceObject
  ( initForwardSink
  , writeToSink
  , readFromSink
  , getTraceObjectsFromReply
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TBQueue
  ( TBQueue
  , newTBQueue
  , isFullTBQueue
  , lengthTBQueue
  , readTBQueue
  , writeTBQueue
  , flushTBQueue
  )
import           Control.Concurrent.STM.TVar
import           Control.Monad (replicateM, unless)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word16)

import           Trace.Forward.Configuration.TraceObject
import qualified Trace.Forward.Protocol.TraceObject.Forwarder as Forwarder
import           Trace.Forward.Protocol.TraceObject.Type
import           Trace.Forward.Utils.ForwardSink (ForwardSink (..))

--------------------------------------------------------------------------------

initForwardSink
  :: ForwarderConfiguration lo
  -> ([lo] -> IO ())
  -> IO (ForwardSink lo)
initForwardSink ForwarderConfiguration{disconnectedQueueSize, connectedQueueSize} callback = do
  -- Initially we always create a big queue, because during node's start
  -- the number of tracing items may be very big.
  (queue, used) <- atomically $
    (,) <$> (newTVar =<< newTBQueue (fromIntegral disconnectedQueueSize))
        <*> newTVar False
  return $ ForwardSink
    { forwardQueue     = queue
    , disconnectedSize = disconnectedQueueSize
    , connectedSize    = connectedQueueSize
    , wasUsed          = used
    , overflowCallback = callback
    }

--------------------------------------------------------------------------------

writeToSink :: ForwardSink lo -> lo -> IO ()
writeToSink ForwardSink{forwardQueue,overflowCallback} traceObject = do
  flushedTraceObjects <- atomically $ writeToSinkSTM forwardQueue traceObject
  -- The overflow callback last, outside of `atomically`.
  case flushedTraceObjects of
    [] -> pure ()
    -- Don't call the overflow function with an empty list.
    _ -> overflowCallback flushedTraceObjects

writeToSinkSTM :: TVar (TBQueue lo) -> lo -> STM [lo]
writeToSinkSTM queueTVar traceObject = do
    ---------- STM transaction: start ----------
    queue <- readTVar queueTVar
    isFull <- isFullTBQueue queue
    !flushedTraceObjects <- if isFull
                            then flushTBQueue queue
                            else pure []
    writeTBQueue queue traceObject
    pure flushedTraceObjects
    ---------- STM transaction: end ----------

--------------------------------------------------------------------------------

readFromSink
  :: ForwardSink lo -- ^ The sink contains the queue we read 'TraceObject's from.
  -> Forwarder.TraceObjectForwarder lo IO ()
readFromSink ForwardSink{forwardQueue, wasUsed} =
  Forwarder.TraceObjectForwarder
    { Forwarder.recvMsgTraceObjectsRequest = \blocking (NumberOfTraceObjects n) -> do
        res <- atomically $ readFromSinkSTM forwardQueue wasUsed blocking n
        -- Handle response format outside of `atomically`.
        pure $ case blocking of
                 TokBlocking    -> BlockingReply $ case res of
                                                     (x:xs) -> x NE.:| xs
                                                     -- If `n == 0` ?????
                                                     [] -> error "impossible"
                 TokNonBlocking -> NonBlockingReply res
    , Forwarder.recvMsgDone = return ()
    }

readFromSinkSTM :: TVar (TBQueue lo)
                -> TVar Bool
                -- If queue is empty, block or not?
                -> TokBlockingStyle blocking
                -- Maximum number of requested trace objects.
                -> Word16
                -> STM [lo]
readFromSinkSTM queueTVar wasUsedTVar blocking n = do
  ---------- STM transaction: start ----------
  queue <- readTVar queueTVar
  -- Instead of using `isEmptyTBQueue`, that internally may read only one TVar,
  -- we optimize for the critical path, the case in which the queue has objects
  -- and directly use `lengthTBQueue` that always reads two TVars.
  queueLength <- lengthTBQueue queue
  res <- if queueLength > 0
         then
             -- Here we already know the queue is NOT empty.
             -- We will either get the entire queue (flush) or do `n` reads.
             if fromEnum n >= fromEnum queueLength
             -- If the requested maximum number is more or equal length, return all.
             then flushTBQueue queue -- Flush is non-blocking, can return empty.
             -- If the requested maximum number is less than length, read `n` times.
             else replicateM (fromIntegral n) (readTBQueue queue)
         else
           -- The queue is empty, return nothing or wait.
           case blocking of
             TokBlocking    -> retry
             TokNonBlocking -> pure []
  unless (null res) $ modifyTVar' wasUsedTVar . const $ True
  pure res
  ---------- STM transaction: end ----------

--------------------------------------------------------------------------------

getTraceObjectsFromReply
  :: BlockingReplyList blocking lo -- ^ The reply with list of 'TraceObject's.
  -> [lo]
getTraceObjectsFromReply (BlockingReply neList)  = NE.toList neList
getTraceObjectsFromReply (NonBlockingReply list) = list
