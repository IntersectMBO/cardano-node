{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( runCardanoTracer
  -- | For testing purposes.
  , runCardanoTracerWithConfig
  , runCardanoTracerWithConfigBrake
  ) where

import           Control.Concurrent.STM.TVar (TVar)

import           Cardano.Tracer.Acceptors.Run (runAcceptors, runAcceptorsWithBrake)
import           Cardano.Tracer.CLI (TracerParams (..))
import           Cardano.Tracer.Configuration (TracerConfig, readTracerConfig)
import           Cardano.Tracer.Handlers.Logs.Rotator (runLogsRotator)
import           Cardano.Tracer.Handlers.Metrics.Servers (runMetricsServers)
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig} =
  readTracerConfig tracerConfig >>= runCardanoTracerWithConfig

runCardanoTracerWithConfig
  :: TracerConfig
  -> IO ()
runCardanoTracerWithConfig config = do
  acceptedMetrics <- initAcceptedMetrics
  acceptedNodeInfo <- initAcceptedNodeInfo
  dpAskers <- initDataPointAskers
  concurrently3
    (runLogsRotator config)
    (runMetricsServers config acceptedMetrics acceptedNodeInfo)
    (runAcceptors config acceptedMetrics acceptedNodeInfo dpAskers)

runCardanoTracerWithConfigBrake
  :: TracerConfig
  -> TVar Bool
  -> IO ()
runCardanoTracerWithConfigBrake config protocolsBrake = do
  acceptedMetrics <- initAcceptedMetrics
  acceptedNodeInfo <- initAcceptedNodeInfo
  dpAskers <- initDataPointAskers
  concurrently3
    (runLogsRotator config)
    (runMetricsServers config acceptedMetrics acceptedNodeInfo)
    (runAcceptorsWithBrake config acceptedMetrics acceptedNodeInfo dpAskers protocolsBrake)
