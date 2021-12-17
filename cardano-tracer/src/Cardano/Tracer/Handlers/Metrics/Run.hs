{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Metrics.Run
  ( runMetricsHandler
  ) where

import           Control.Concurrent.Async (concurrently_)

import           Cardano.Tracer.Configuration (TracerConfig (..))
import           Cardano.Tracer.Handlers.Metrics.Monitoring (runMonitoringServer)
import           Cardano.Tracer.Handlers.Metrics.Prometheus (runPrometheusServer)
import           Cardano.Tracer.Types

runMetricsHandler
  :: TracerConfig
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> IO ()
runMetricsHandler TracerConfig{hasEKG, hasPrometheus} acceptedMetrics acceptedNodeInfo =
  case (hasEKG, hasPrometheus) of
    (Nothing, Nothing) ->
      return ()
    (Nothing, Just prom) ->
      runPrometheusServer prom acceptedMetrics acceptedNodeInfo
    (Just ekg, Nothing) ->
      runMonitoringServer ekg acceptedMetrics
    (Just ekg, Just prom) ->
      concurrently_ (runPrometheusServer prom acceptedMetrics acceptedNodeInfo)
                    (runMonitoringServer ekg acceptedMetrics)
