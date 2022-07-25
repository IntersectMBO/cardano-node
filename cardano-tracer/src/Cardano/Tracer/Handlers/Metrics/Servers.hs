{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Metrics.Servers
  ( runMetricsServers
  ) where

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Monad (void)

import           Cardano.Tracer.Configuration (TracerConfig (..))
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Monitoring (runMonitoringServer)
import           Cardano.Tracer.Handlers.Metrics.Prometheus (runPrometheusServer)

-- | Runs metrics servers if needed:
--
--   1. Prometheus exporter.
--   2. EKG monitoring web-page.
--
runMetricsServers :: TracerEnv -> IO ()
runMetricsServers TracerEnv{teConfig, teConnectedNodes, teAcceptedMetrics} =
  case (hasEKG teConfig, hasPrometheus teConfig) of
    (Nothing,  Nothing)   -> return ()
    (Nothing,  Just prom) -> runPrometheusServer prom teConnectedNodes teAcceptedMetrics
    (Just ekg, Nothing)   -> runMonitoringServer ekg  teConnectedNodes teAcceptedMetrics
    (Just ekg, Just prom) -> void . sequenceConcurrently $
                               [ runPrometheusServer prom teConnectedNodes teAcceptedMetrics
                               , runMonitoringServer ekg  teConnectedNodes teAcceptedMetrics
                               ]
