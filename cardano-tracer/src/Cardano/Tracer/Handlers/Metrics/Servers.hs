{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Metrics.Servers
  ( runMetricsServers
  ) where

import           Cardano.Tracer.Configuration (TracerConfig (..))
import           Cardano.Tracer.Handlers.Metrics.Monitoring (runMonitoringServer)
import           Cardano.Tracer.Handlers.Metrics.Prometheus (runPrometheusServer)
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

runMetricsServers
  :: TracerConfig
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> IO ()
runMetricsServers TracerConfig{hasEKG, hasPrometheus} acceptedMetrics acceptedNodeInfo =
  case (hasEKG, hasPrometheus) of
    (Nothing, Nothing) ->
      return ()
    (Nothing, Just prom) ->
      runPrometheusServer prom acceptedMetrics acceptedNodeInfo
    (Just ekg, Nothing) ->
      runMonitoringServer ekg acceptedMetrics
    (Just ekg, Just prom) ->
      concurrently2
        (runPrometheusServer prom acceptedMetrics acceptedNodeInfo)
        (runMonitoringServer ekg acceptedMetrics)
