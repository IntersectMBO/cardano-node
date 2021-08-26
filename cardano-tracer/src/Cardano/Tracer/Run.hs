{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( runCardanoTracer
  -- | For testing purposes.
  , runCardanoTracerWithConfig
  ) where

import           Control.Concurrent.Async (withAsync, wait)

import           Cardano.Tracer.Acceptors (runAcceptors)
import           Cardano.Tracer.CLI (TracerParams (..))
import           Cardano.Tracer.Configuration (TracerConfig, readTracerConfig)
import           Cardano.Tracer.Handlers.Logs.Rotator (runLogsRotator)
import           Cardano.Tracer.Handlers.Metrics.Run (runMetricsHandler)
import           Cardano.Tracer.Types

runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig} =
  readTracerConfig tracerConfig >>= runCardanoTracerWithConfig

runCardanoTracerWithConfig :: TracerConfig -> IO ()
runCardanoTracerWithConfig config = do
  acceptedMetrics  <- initAcceptedMetrics
  acceptedNodeInfo <- initAcceptedNodeInfo

  runAndWait $ runLogsRotator    config
  runAndWait $ runMetricsHandler config acceptedMetrics acceptedNodeInfo
  runAndWait $ runAcceptors      config acceptedMetrics acceptedNodeInfo
 where
  runAndWait action = withAsync action wait
