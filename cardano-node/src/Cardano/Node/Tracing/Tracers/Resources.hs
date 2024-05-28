{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing.Tracers.Resources
  ( startResourceTracer
  ) where

import           Cardano.Logging.Resources

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Monad (forM_, forever)
import           Control.Monad.Class.MonadAsync (link)
import           GHC.Conc (labelThread, myThreadId)

import           "contra-tracer" Control.Tracer

-- | Starts a background thread to periodically trace resource statistics.
-- The thread reads resource stats and traces them using the given tracer.
-- It is linked to the parent thread to ensure proper error propagation.
startResourceTracer :: Tracer IO ResourceStats -> Int -> IO ()
startResourceTracer tracer delayMilliseconds = do
  thread <- async resourceThread
  -- Link the thread to the parent to propagate exceptions properly.
  link thread
  where
    -- | The background thread that periodically traces resource stats.
    resourceThread :: IO ()
    resourceThread = do
      -- Label the thread for easier debugging and identification.
      myThreadId >>= flip labelThread "Resource Stats Tracer"
      forever $ do
        maybeStats <- readResourceStats
        -- If stats are available, trace them using the provided tracer.
        forM_ maybeStats $ traceWith tracer
        threadDelay (delayMilliseconds * 1000)
