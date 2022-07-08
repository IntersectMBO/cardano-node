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
import           Cardano.Tracer.Handlers.RTView.Run (initEventsQueues, initSavedTraceObjects,
                   runRTView)
import           Cardano.Tracer.Types (DataPointRequestors, ProtocolsBrake)
import           Cardano.Tracer.Utils (initAcceptedMetrics, initConnectedNodes,
                   initDataPointRequestors, initProtocolsBrake)

-- | Top-level run function, called by 'cardano-tracer' app.
runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig} = do
  config <- readTracerConfig tracerConfig
  brake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  doRunCardanoTracer config brake dpRequestors

-- | Runs all internal services of the tracer.
doRunCardanoTracer
  :: TracerConfig        -- ^ Tracer's configuration.
  -> ProtocolsBrake      -- ^ The flag we use to stop all the protocols.
  -> DataPointRequestors -- ^ The DataPointRequestors to ask 'DataPoint's.
  -> IO ()
doRunCardanoTracer config protocolsBrake dpRequestors = do
  connectedNodes  <- initConnectedNodes
  acceptedMetrics <- initAcceptedMetrics
  currentLogLock  <- newLock
  currentDPLock   <- newLock
  savedTO         <- initSavedTraceObjects
  eventsQueues    <- initEventsQueues dpRequestors currentDPLock
  void . sequenceConcurrently $
    [ runLogsRotator    config currentLogLock
    , runMetricsServers config connectedNodes acceptedMetrics
    , runAcceptors      config connectedNodes acceptedMetrics savedTO
                        dpRequestors protocolsBrake currentLogLock
                        eventsQueues
    , runRTView         config connectedNodes acceptedMetrics savedTO
                        dpRequestors currentDPLock eventsQueues
    ]
