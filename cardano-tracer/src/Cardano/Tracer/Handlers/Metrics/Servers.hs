{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module Cardano.Tracer.Handlers.Metrics.Servers
  ( runMetricsServers
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Monitoring
import           Cardano.Tracer.Handlers.Metrics.Prometheus
import qualified Cardano.Tracer.Handlers.Metrics.Utils as Utils
import           Cardano.Tracer.Utils (sequenceConcurrently_)

import           Control.AutoUpdate
import           Data.Maybe (catMaybes)
import           Control.Monad (unless)

-- | Runs metrics servers if needed:
--
--   1. Prometheus exporter.
--   2. EKG monitoring web-page.
--
runMetricsServers
  :: TracerEnv
  -> IO ()
runMetricsServers tracerEnv = do
  unless (null servers) do
    computeRoutes_autoUpdate :: IO Utils.RouteDictionary <-
      mkAutoUpdate defaultUpdateSettings
        { updateAction = Utils.computeRoutes tracerEnv
        , updateFreq   = 5_000_000 -- invalidate memoized RouteDictionary every 5 seconds
        }

    sequenceConcurrently_ do
      servers `routing` computeRoutes_autoUpdate

  where
  routing :: [IO Utils.RouteDictionary -> a] -> IO Utils.RouteDictionary -> [a]
  routing = sequence

  servers :: [IO Utils.RouteDictionary -> IO ()]
  servers = catMaybes
    [ runPrometheusServer tracerEnv <$> hasPrometheus
    , runMonitoringServer tracerEnv <$> hasEKG
    ]

  TracerEnv
    { teConfig = TracerConfig { hasPrometheus, hasEKG }
    } = tracerEnv
