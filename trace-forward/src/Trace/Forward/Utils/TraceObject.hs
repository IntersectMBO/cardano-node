{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

--------------------------------------------------------------------------------

module Trace.Forward.Utils.TraceObject
  ( initForwardSink
  , writeToSink
  , readFromSink
  , getTraceObjectsFromReply
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent.STM (STM, atomically, check)
import           Control.Concurrent.STM.TBQueue
  ( TBQueue
  , newTBQueue
  , isEmptyTBQueue
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
                            -- Of the biggest requested size, will flush.
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
    { Forwarder.recvMsgTraceObjectsRequest = \blocking (NumberOfTraceObjects n) ->
        case blocking of
          TokBlocking -> do
            objs <- atomically $ do
              queue <- readTVar forwardQueue
              check . not =<< isEmptyTBQueue queue
              res <- getNTraceObjectsNonBlocking n queue >>= \case
                []     -> error "impossible"
                (x:xs) -> return $ x NE.:| xs
              modifyTVar' wasUsed . const $ True
              pure res
            return $ BlockingReply objs
          TokNonBlocking -> do
            objs <- atomically $ do
              queue <- readTVar forwardQueue
              res <- getNTraceObjectsNonBlocking n queue
              unless (null res) $
                modifyTVar' wasUsed . const $ True
              pure res
            return $ NonBlockingReply objs
    , Forwarder.recvMsgDone = return ()
    }

getNTraceObjectsNonBlocking
  :: Word16
  -> TBQueue lo
  -> STM [lo]
getNTraceObjectsNonBlocking 0 _ = return []
getNTraceObjectsNonBlocking n q = do
  len <- lengthTBQueue q
  if len <= fromIntegral n
    then flushTBQueue q
    else replicateM (fromIntegral n) (readTBQueue q)

--------------------------------------------------------------------------------

getTraceObjectsFromReply
  :: BlockingReplyList blocking lo -- ^ The reply with list of 'TraceObject's.
  -> [lo]
getTraceObjectsFromReply (BlockingReply neList)  = NE.toList neList
getTraceObjectsFromReply (NonBlockingReply list) = list
