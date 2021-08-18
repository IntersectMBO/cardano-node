module Cardano.Tracer.Handlers
  ( runHandlers
  ) where

import           Control.Concurrent.Async (concurrently_)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Rotator (runLogsRotator)
import           Cardano.Tracer.Handlers.Metrics.Run (runMetricsHandler)
import           Cardano.Tracer.Types

runHandlers
  :: TracerConfig
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> IO ()
runHandlers config acceptedMetrics acceptedNodeInfo =
  concurrently_ (runLogsRotator config)
                (runMetricsHandler config acceptedMetrics acceptedNodeInfo)
