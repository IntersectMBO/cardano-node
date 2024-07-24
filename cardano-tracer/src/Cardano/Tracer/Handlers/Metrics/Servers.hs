{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module Cardano.Tracer.Handlers.Metrics.Servers
  ( runMetricsServers
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Monitoring
import           Cardano.Tracer.Handlers.Metrics.Prometheus

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
runMetricsServers tracerEnv = void do sequenceConcurrently servers

  where
  servers :: [IO ()]
  servers = catMaybes
    [ runPrometheusServer tracerEnv <$> hasPrometheus
    , runMonitoringServer tracerEnv <$> hasEKG
    ]

  TracerEnv
    { teConfig = TracerConfig { hasPrometheus, hasEKG }
    } = tracerEnv
