{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( doRunCardanoTracer
  , runCardanoTracer
  ) where

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Monad (void)

import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.CLI
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Rotator
import           Cardano.Tracer.Handlers.Metrics.Servers
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

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
  savedTO         <- initSavedTraceObjects

  resourcesHistory <- initResourcesHistory
  chainHistory     <- initBlockchainHistory
  txHistory        <- initTransactionsHistory

  currentLogLock <- newLock
  currentDPLock  <- newLock
  eventsQueues   <- initEventsQueues dpRequestors currentDPLock

  -- Specify what should be done before program stops.
  beforeProgramStops $
    backupAllHistory
      connectedNodes
      chainHistory
      resourcesHistory
      txHistory
      dpRequestors
      currentDPLock

  void . sequenceConcurrently $
    [ runLogsRotator    config currentLogLock
    , runMetricsServers config connectedNodes acceptedMetrics
    , runAcceptors      config connectedNodes acceptedMetrics savedTO
                        dpRequestors protocolsBrake currentLogLock
                        eventsQueues
    , runRTView         config connectedNodes acceptedMetrics savedTO
                        chainHistory resourcesHistory txHistory
                        dpRequestors currentDPLock eventsQueues
    ]
