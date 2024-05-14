{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module Cardano.Tracer.Handlers.Metrics.Servers
  ( runMetricsServers
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Monitoring
import           Cardano.Tracer.Handlers.Metrics.Prometheus
-- import           Cardano.Tracer.MetaTrace

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Monad (void)
import           Data.Maybe (catMaybes)

-- | Runs metrics servers if needed:
--
--   1. Prometheus exporter.
--   2. EKG monitoring web-page.
--
runMetricsServers 
  :: TracerEnv 
  -> IO ()
runMetricsServers tracerEnv = 
  void do
    sequenceConcurrently servers


  -- traceWith teTracer TracerStartedPrometheus

  where
  servers :: [IO ()]
  servers = catMaybes
--O     [ runPrometheusServer tracerEnv <$> Nothing
    [ runPrometheusServer tracerEnv <$> hasPrometheus --O
    , runMonitoringServer tracerEnv <$> hasEKG 
    ]

  TracerEnv 
--O     { teConfig = TracerConfig { hasEKG }
    { teConfig = TracerConfig { hasPrometheus, hasEKG } --O
    } = tracerEnv

   -- -- for_ @Maybe (hasPrometheus teConfig) do
  -- --   runPrometheusServer tracerEnv
  -- -- for_ @Maybe (hasEKG teConfig) do
  -- case (hasEKG teConfig, hasPrometheus teConfig) of
  --   (Nothing,  Nothing)   -> return ()
  --   (Nothing,  Just prom) -> do
  --     -- Move into `runPrometheusServer'
  --     traceWith teTracer TracerStartedPrometheus
  --     runPrometheusServer tracerEnv prom
  --   (Just ekg, Nothing)   -> runMonitoringServer tracerEnv ekg
  --   (Just ekg, Just prom) -> void . sequenceConcurrently $ 
  --                              [ runPrometheusServer tracerEnv prom
  --                              , runMonitoringServer tracerEnv ekg
  --                              ]

