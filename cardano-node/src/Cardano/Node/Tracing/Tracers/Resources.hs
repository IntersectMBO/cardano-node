{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing.Tracers.Resources
  ( startResourceTracer
  ) where

import           "contra-tracer" Control.Tracer

import           Cardano.Logging.Resources
import           Cardano.Prelude hiding (trace)

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
      maybe (pure ()) (traceWith tr) mbrs
      threadDelay (delayMilliseconds * 1000)
