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

import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad (unless, (<$!>))
import           Control.Monad.Extra (whenM)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word16)
import           System.IO

import           Trace.Forward.Configuration.TraceObject
import qualified Trace.Forward.Protocol.TraceObject.Forwarder as Forwarder
import           Trace.Forward.Protocol.TraceObject.Type


data ForwardSink lo = ForwardSink
  { forwardQueue     :: !(TVar (TBQueue lo))
  , disconnectedSize :: !Word
  , connectedSize    :: !Word
  , wasUsed          :: !(TVar Bool)
  }

initForwardSink
  :: ForwarderConfiguration lo
  -> IO (ForwardSink lo)
initForwardSink ForwarderConfiguration{disconnectedQueueSize, connectedQueueSize} = do
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
    }

-- | There are 4 possible cases when we try to write tracing item:
--   1. The queue is __still__ empty (no tracing items were written in it).
--   2. The queue is __already__ empty (all previously written items were taken from it).
--   3. The queue is full. In this case flush all tracing items to stdout and continue.
--   4. The queue isn't empty and isn't full. Just continue writing.
writeToSink
  :: Show lo
  => ForwardSink lo
  -> lo
  -> IO ()
writeToSink ForwardSink{forwardQueue, disconnectedSize, connectedSize, wasUsed} traceObject = do
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
  case condToFlush of
    Nothing -> pure ()
    Just li -> do
                  mapM_ print li
                  hFlush stdout
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
      if fromIntegral qLen == disconnectedSize
        then -- trace ("shrinkQueue connected " ++ show connectedSize) $
              switchQueue connectedSize
        else pure ()

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
        return (replyList, readFromSink sink)
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

getTraceObjectsFromReply
  :: BlockingReplyList blocking lo -- ^ The reply with list of 'TraceObject's.
  -> [lo]
getTraceObjectsFromReply (BlockingReply neList)  = NE.toList neList
getTraceObjectsFromReply (NonBlockingReply list) = list
