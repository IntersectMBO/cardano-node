{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( doRunCardanoTracer
  , runCardanoTracer
  ) where

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Monad (void)

import           Cardano.Tracer.Acceptors.Run (runAcceptors)
import           Cardano.Tracer.CLI (TracerParams (..))
import           Cardano.Tracer.Configuration (TracerConfig, readTracerConfig)
import           Cardano.Tracer.Handlers.Logs.Rotator (runLogsRotator)
import           Cardano.Tracer.Handlers.Metrics.Servers (runMetricsServers)
import           Cardano.Tracer.Handlers.RTView.Run (initSavedTraceObjects, runRTView)
import           Cardano.Tracer.Types (ProtocolsBrake)
import           Cardano.Tracer.Utils (initAcceptedMetrics, initConnectedNodes,
                   initDataPointAskers, initProtocolsBrake)

-- | Top-level run function, called by 'cardano-tracer' app.
runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig} = do
  config <- readTracerConfig tracerConfig
  doRunCardanoTracer config =<< initProtocolsBrake

-- | Runs all internal services of the tracer.
doRunCardanoTracer
  :: TracerConfig   -- ^ Tracer's configuration.
  -> ProtocolsBrake -- ^ The flag we use to stop all the protocols.
  -> IO ()
doRunCardanoTracer config protocolsBrake = do
  connectedNodes <- initConnectedNodes
  acceptedMetrics <- initAcceptedMetrics
  dpAskers <- initDataPointAskers
  currentLogLock <- newLock
  savedTO <- initSavedTraceObjects
  void . sequenceConcurrently $
    [ runLogsRotator    config currentLogLock
    , runMetricsServers config connectedNodes acceptedMetrics
    , runRTView         config connectedNodes acceptedMetrics dpAskers savedTO
    , runAcceptors      config connectedNodes acceptedMetrics dpAskers
                        protocolsBrake currentLogLock savedTO
    ]
