{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( doRunCardanoTracer
  , runCardanoTracer
  ) where

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Monad (void)

import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.CLI
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.Rotator
import           Cardano.Tracer.Handlers.Metrics.Servers
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

-- | Top-level run function, called by 'cardano-tracer' app.
runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig, stateDir} = do
  config <- readTracerConfig tracerConfig
  brake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  doRunCardanoTracer config stateDir brake dpRequestors

-- | Runs all internal services of the tracer.
doRunCardanoTracer
  :: TracerConfig        -- ^ Tracer's configuration.
  -> Maybe FilePath      -- ^ Path to RTView's internal state files.
  -> ProtocolsBrake      -- ^ The flag we use to stop all the protocols.
  -> DataPointRequestors -- ^ The DataPointRequestors to ask 'DataPoint's.
  -> IO ()
doRunCardanoTracer config rtViewStateDir protocolsBrake dpRequestors = do
  connectedNodes      <- initConnectedNodes
  connectedNodesNames <- initConnectedNodesNames
  acceptedMetrics <- initAcceptedMetrics
  savedTO         <- initSavedTraceObjects

  chainHistory     <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory        <- initTransactionsHistory

  currentLogLock <- newLock
  currentDPLock  <- newLock
  eventsQueues   <- initEventsQueues rtViewStateDir connectedNodesNames dpRequestors currentDPLock

  rtViewPageOpened <- newTVarIO False

  -- Environment for all following functions.
  let tracerEnv =
        TracerEnv
          { teConfig              = config
          , teConnectedNodes      = connectedNodes
          , teConnectedNodesNames = connectedNodesNames
          , teAcceptedMetrics     = acceptedMetrics
          , teSavedTO             = savedTO
          , teBlockchainHistory   = chainHistory
          , teResourcesHistory    = resourcesHistory
          , teTxHistory           = txHistory
          , teCurrentLogLock      = currentLogLock
          , teCurrentDPLock       = currentDPLock
          , teEventsQueues        = eventsQueues
          , teDPRequestors        = dpRequestors
          , teProtocolsBrake      = protocolsBrake
          , teRTViewPageOpened    = rtViewPageOpened
          , teRTViewStateDir      = rtViewStateDir
          }

  -- Specify what should be done before 'cardano-tracer' stops.
  beforeProgramStops $ do
    backupAllHistory tracerEnv
    applyBrake (teProtocolsBrake tracerEnv)

  void . sequenceConcurrently $
    [ runLogsRotator    tracerEnv
    , runMetricsServers tracerEnv
    , runAcceptors      tracerEnv
    , runRTView         tracerEnv
    ]
