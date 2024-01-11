{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Utils.TraceObject
  ( ForwardSink (..)
  , initForwardSink
  , writeToSink
  , readFromSink
  , getTraceObjectsFromReply
  ) where

import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad (forM_, unless, when, (<$!>))
import           Control.Monad.Extra (whenM)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word16)

import           Trace.Forward.Configuration.TraceObject
import qualified Trace.Forward.Protocol.TraceObject.Forwarder as Forwarder
import           Trace.Forward.Protocol.TraceObject.Type


data ForwardSink lo = ForwardSink
  { forwardQueue     :: !(TVar (TBQueue lo))
  , disconnectedSize :: !Word
  , connectedSize    :: !Word
  , wasUsed          :: !(TVar Bool)
  , overflowCallback :: !([lo] -> IO ())
  }

initForwardSink
  :: ForwarderConfiguration lo
  -> ([lo] -> IO ())
  -> IO (ForwardSink lo)
initForwardSink ForwarderConfiguration{disconnectedQueueSize, connectedQueueSize} callback = do
  -- Initially we always create a big queue, because during node's start
  -- the number of tracing items may be very big.
  (queue, used) <-
    atomically $ (,) <$> (newTVar =<< newTBQueue (fromIntegral disconnectedQueueSize))
                     <*> newTVar False
  return $ ForwardSink
    { forwardQueue     = queue
    , disconnectedSize = disconnectedQueueSize
    , connectedSize    = connectedQueueSize
    , wasUsed          = used
    , overflowCallback = callback
    }

-- | There are 4 possible cases when we try to write tracing item:
--   1. The queue is __still__ empty (no tracing items were written in it).
--   2. The queue is __already__ empty (all previously written items were taken from it).
--   3. The queue is full. In this case flush all tracing items to stdout and continue.
--   4. The queue isn't empty and isn't full. Just continue writing.
writeToSink ::
     ForwardSink lo
  -> lo
  -> IO ()
writeToSink ForwardSink{
              forwardQueue,
              disconnectedSize,
              connectedSize,
              wasUsed,
              overflowCallback} traceObject = do
  condToFlush <- atomically $ do
    q <- readTVar forwardQueue
    ((,) <$> isFullTBQueue q
         <*> isEmptyTBQueue q) >>= \case
      (True, _)    -> do
                          res <- maybeFlushQueueToStdout q
                          q' <- readTVar forwardQueue
                          writeTBQueue q' traceObject
                          pure res
      (_,    True) -> do
                          maybeShrinkQueue q
                          q' <- readTVar forwardQueue
                          writeTBQueue q' traceObject
                          pure Nothing
      (_,    _)    -> do
                          writeTBQueue q traceObject
                          pure Nothing
  forM_ condToFlush overflowCallback

 where
  -- The queue is full, but if it's a small queue, we can switch it
  -- to a big one and give a chance not to flush items to stdout yet.
  maybeFlushQueueToStdout q = do
    qLen <- lengthTBQueue q
    if fromIntegral qLen == connectedSize
      then do
        -- The small queue is full, so we have to switch to a big one and
        -- then flush collected items from the small queue and store them in
        -- a big one.

        acceptedItems <- -- trace ("growQueue disconnected" ++ show disconnectedSize) $
                          flushTBQueue q
        switchQueue disconnectedSize
        bigQ <- readTVar forwardQueue
        mapM_ (writeTBQueue bigQ) acceptedItems
        pure Nothing
      else do
        -- The big queue is full, we have to flush it to stdout.
        Just <$!> flushTBQueue q

  -- if the sink was used and it
  maybeShrinkQueue q = do
    whenM (readTVar wasUsed) $ do
      qLen <- lengthTBQueue q
      when (fromIntegral qLen == disconnectedSize) $ do
        -- trace ("shrinkQueue connected " ++ show connectedSize) $
        switchQueue connectedSize

  switchQueue size =
    newTBQueue (fromIntegral size) >>= modifyTVar' forwardQueue . const

readFromSink
  :: ForwardSink lo -- ^ The sink contains the queue we read 'TraceObject's from.
  -> Forwarder.TraceObjectForwarder lo IO ()
readFromSink sink@ForwardSink{forwardQueue, wasUsed} =
  Forwarder.TraceObjectForwarder
    { Forwarder.recvMsgTraceObjectsRequest = \blocking (NumberOfTraceObjects n) -> do
        replyList <-
          case blocking of
            TokBlocking -> do
              objs <- atomically $ do
                queue <- readTVar forwardQueue
                res <- getNTraceObjectsBlocking n queue >>= \case
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
        return (replyList, readFromSink sink)
    , Forwarder.recvMsgDone = return ()
    }

-- | Returns at most N 'TraceObject's from the queue.
getNTraceObjectsNonBlocking
  :: Word16
  -> TBQueue lo
  -> STM [lo]
getNTraceObjectsNonBlocking 0 _ = return []
getNTraceObjectsNonBlocking n q =
  tryReadTBQueue q >>=
    \case
      Just lo -> (lo :) <$> getNTraceObjectsNonBlocking (n - 1) q
      Nothing  -> return []

-- | Returns at most N 'TraceObject's from the queue.
getNTraceObjectsBlocking
  :: Word16
  -> TBQueue lo
  -> STM [lo]
getNTraceObjectsBlocking 0 _ = return []
getNTraceObjectsBlocking n q = do
    lo <- readTBQueue q
    (lo :) <$> getNTraceObjectsNonBlocking (n - 1) q

getTraceObjectsFromReply
  :: BlockingReplyList blocking lo -- ^ The reply with list of 'TraceObject's.
  -> [lo]
getTraceObjectsFromReply (BlockingReply neList)  = NE.toList neList
getTraceObjectsFromReply (NonBlockingReply list) = list
