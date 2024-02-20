{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing.Tracers.Resources
  ( startResourceTracer
  ) where

import           Cardano.Logging.Resources

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Monad (forM_, forever)
import           Control.Monad.Class.MonadAsync (link)
import           "contra-tracer" Control.Tracer

startResourceTracer
  :: Tracer IO ResourceStats
  -> Int
  -> IO ()
startResourceTracer tr delayMilliseconds = do
    as <- async resourceThread
    link as
  where
    resourceThread :: IO ()
    resourceThread = forever $ do
      mbrs <- readResourceStats
      forM_ mbrs $ \rs -> traceWith tr rs
      threadDelay (delayMilliseconds * 1000)
      forM_ mbrs $ \rs -> traceWith tr rs
      threadDelay (delayMilliseconds * 1000)
