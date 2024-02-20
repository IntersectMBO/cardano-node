{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Metrics.Servers
  ( runMetricsServers
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Monitoring
import           Cardano.Tracer.Handlers.Metrics.Prometheus
import           Cardano.Tracer.MetaTrace

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Monad (void)

-- | Runs metrics servers if needed:
--
--   1. Prometheus exporter.
--   2. EKG monitoring web-page.
--
runMetricsServers :: TracerEnv -> IO ()
runMetricsServers tracerEnv@TracerEnv{teConfig, teTracer} =
  case (hasEKG teConfig, hasPrometheus teConfig) of
    (Nothing,  Nothing)   -> return ()
    (Nothing,  Just prom) -> do
      traceWith teTracer TracerStartedPrometheus
      runPrometheusServer tracerEnv prom
    (Just ekg, Nothing)   -> runMonitoringServer tracerEnv ekg
    (Just ekg, Just prom) -> void . sequenceConcurrently $
                               [ runPrometheusServer tracerEnv prom
                               , runMonitoringServer tracerEnv ekg
                               ]
