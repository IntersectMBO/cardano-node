{- HLINT ignore "Avoid lambda" -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( doRunCardanoTracer
  , runCardanoTracer
  ) where

import           Cardano.Logging.Resources
import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.CLI
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.Rotator
import           Cardano.Tracer.Handlers.Metrics.Servers
import           Cardano.Tracer.Handlers.ReForwarder
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, link)
import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Monad
import           Data.Foldable (for_)

-- | Top-level run function, called by 'cardano-tracer' app.
runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig, stateDir, logSeverity} = do
  tr <- mkTracerTracer $ SeverityF logSeverity
  traceWith tr $ TracerParamsAre tracerConfig stateDir logSeverity

  config <- readTracerConfig tracerConfig
  traceWith tr $ TracerConfigIs config

  for_ (resourceFreq config) \msInterval -> do
    threadId <- async do
      forever do
        mbrs <- readResourceStats
        for_ mbrs \resourceStat ->
          traceWith tr (TracerResource resourceStat)
        threadDelay (1_000 * msInterval) -- Delay in seconds, given milliseconds
    link threadId

  brake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  doRunCardanoTracer config stateDir tr brake dpRequestors

-- | Runs all internal services of the tracer.
doRunCardanoTracer
  :: TracerConfig        -- ^ Tracer's configuration.
  -> Maybe FilePath      -- ^ Path to RTView's internal state files.
  -> Trace IO TracerTrace
  -> ProtocolsBrake      -- ^ The flag we use to stop all the protocols.
  -> DataPointRequestors -- ^ The DataPointRequestors to ask 'DataPoint's.
  -> IO ()
doRunCardanoTracer config rtViewStateDir tr protocolsBrake dpRequestors = do
  traceWith tr TracerInitStarted
  connectedNodes      <- initConnectedNodes
  connectedNodesNames <- initConnectedNodesNames
  acceptedMetrics <- initAcceptedMetrics
  savedTO <- initSavedTraceObjects

  chainHistory     <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory        <- initTransactionsHistory

  currentLogLock <- newLock
  currentDPLock  <- newLock

  traceWith tr TracerInitEventQueues
  eventsQueues   <- initEventsQueues rtViewStateDir connectedNodesNames dpRequestors currentDPLock

  rtViewPageOpened <- newTVarIO False

  (reforwardTraceObject,_trDataPoint) <- initReForwarder config tr

  -- Environment for all following functions.
  let tracerEnv =
        TracerEnv
          { teConfig                = config
          , teConnectedNodes        = connectedNodes
          , teConnectedNodesNames   = connectedNodesNames
          , teAcceptedMetrics       = acceptedMetrics
          , teSavedTO               = savedTO
          , teBlockchainHistory     = chainHistory
          , teResourcesHistory      = resourcesHistory
          , teTxHistory             = txHistory
          , teCurrentLogLock        = currentLogLock
          , teCurrentDPLock         = currentDPLock
          , teEventsQueues          = eventsQueues
          , teDPRequestors          = dpRequestors
          , teProtocolsBrake        = protocolsBrake
          , teRTViewPageOpened      = rtViewPageOpened
          , teRTViewStateDir        = rtViewStateDir
          , teTracer                = tr
          , teReforwardTraceObjects = reforwardTraceObject
          }

  -- Specify what should be done before 'cardano-tracer' stops.
  beforeProgramStops $ do
    traceWith tr TracerShutdownInitiated
    backupAllHistory tracerEnv
    traceWith tr TracerShutdownHistBackup
    applyBrake (teProtocolsBrake tracerEnv)
    traceWith tr TracerShutdownComplete

  traceWith tr TracerInitDone
  void . sequenceConcurrently $
    [ runLogsRotator    tracerEnv
    , runMetricsServers tracerEnv
    , runAcceptors      tracerEnv
    , runRTView         tracerEnv
    ]
