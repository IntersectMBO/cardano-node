{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Metrics.Servers
  ( runMetricsServers
  ) where

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Monad (void)

import           Cardano.Tracer.Configuration (TracerConfig (..))
import           Cardano.Tracer.Handlers.Metrics.Monitoring (runMonitoringServer)
import           Cardano.Tracer.Handlers.Metrics.Prometheus (runPrometheusServer)
import           Cardano.Tracer.Types (ConnectedNodes, AcceptedMetrics)

-- | Runs metrics servers if needed:
--
--   1. Prometheus exporter.
--   2. EKG monitoring web-page.
--
runMetricsServers
  :: TracerConfig
  -> ConnectedNodes
  -> AcceptedMetrics
  -> IO ()
runMetricsServers TracerConfig{hasEKG, hasPrometheus} connectedNodes acceptedMetrics =
  case (hasEKG, hasPrometheus) of
    (Nothing,  Nothing)   -> return ()
    (Nothing,  Just prom) -> runPrometheusServer prom connectedNodes acceptedMetrics
    (Just ekg, Nothing)   -> runMonitoringServer ekg  connectedNodes acceptedMetrics
    (Just ekg, Just prom) -> void . sequenceConcurrently $
                               [ runPrometheusServer prom connectedNodes acceptedMetrics
                               , runMonitoringServer ekg  connectedNodes acceptedMetrics
                               ]
