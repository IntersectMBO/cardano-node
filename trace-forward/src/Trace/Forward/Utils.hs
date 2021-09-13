{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Utils
  ( ForwardSink (..)
  , initForwardSink
  , writeToSink
  , runActionInLoop
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception (SomeAsyncException (..), fromException, tryJust)
import           Control.Monad.Extra (whenM)
import           Control.Tracer (showTracing, stdoutTracer, traceWith)
import           System.IO
import           System.Time.Extra (sleep)

import           Trace.Forward.Configuration

-- | Run monadic action in a loop. If there's an exception, it will re-run
--   the action again, after pause that grows.
runActionInLoop
  :: IO ()
  -> HowToConnect
  -> Word
  -> IO ()
runActionInLoop action endpoint prevDelay =
  tryJust excludeAsyncExceptions action >>= \case
    Left e -> do
      logTrace $ "trace-forward, connection with " <> show endpoint <> " failed: " <> show e
      sleep $ fromIntegral currentDelay
      runActionInLoop action endpoint currentDelay
    Right _ -> return ()
 where
  excludeAsyncExceptions e =
    case fromException e of
      Just SomeAsyncException {} -> Nothing
      _ -> Just e

  logTrace = traceWith $ showTracing stdoutTracer

  currentDelay =
    if prevDelay < 60
      then prevDelay * 2
      else 60 -- After we reached 60+ secs delay, repeat an attempt every minute.

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
--   1. The queue is __still__ empty (no tracing items were writen in it).
--   2. The queue is __already__ empty (all previously written items were taken from it).
--   3. The queue is full. In this case flush all tracing items to stdout and continue.
--   4. The queue isn't empty and isn't full. Just continue writing.
writeToSink
  :: Show lo
  => ForwardSink lo
  -> lo
  -> IO ()
writeToSink ForwardSink{forwardQueue, disconnectedSize, connectedSize, wasUsed} traceObject = do
  q <- readTVarIO forwardQueue
  atomically ((,) <$> isFullTBQueue q
                  <*> isEmptyTBQueue q) >>= \case
    (True, _)    -> maybeFlushQueueToStdout q
    (_,    True) -> checkIfSinkWasUsed q
    (_,    _)    -> return ()
  atomically $ readTVar forwardQueue >>= flip writeTBQueue traceObject
 where
  -- The queue is full, but if it's a small queue, we can switch it
  -- to a big one and give a chance not to flush items to stdout yet.
  maybeFlushQueueToStdout q = do
    qLen <- atomically $ lengthTBQueue q
    if fromIntegral qLen == connectedSize
      then atomically $ switchQueue disconnectedSize
      else atomically (flushTBQueue q) >>= mapM_ print >> hFlush stdout

  checkIfSinkWasUsed q = atomically $
    whenM (readTVar wasUsed) $ switchToAnotherQueue q

  switchToAnotherQueue q = do
    qLen <- lengthTBQueue q
    if fromIntegral qLen == disconnectedSize
      then switchQueue connectedSize
      else switchQueue disconnectedSize

  switchQueue size =
    newTBQueue (fromIntegral size) >>= modifyTVar' forwardQueue . const
