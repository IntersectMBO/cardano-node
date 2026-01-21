{- HLINT ignore "Avoid lambda" -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( doRunCardanoTracer
  , runCardanoTracer
  , CardanoTracerHandle (..)
  , cleanupCardanoTracer
  ) where

import           Cardano.Logging.Resources
import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.CLI
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.Rotator
import           Cardano.Tracer.Handlers.Metrics.Servers
import           Cardano.Tracer.Handlers.ReForwarder
#if RTVIEW
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Handlers.RTView.Run
#endif
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.Async (async, link)
import           Control.Concurrent.Extra (newLock)
#if RTVIEW
import           Control.Concurrent.STM.TVar (newTVarIO)
#endif
import           Control.Exception (SomeException, try)
import           Data.Aeson (decodeFileStrict')
import           Data.Foldable (for_)
import           Data.Kind (Type)
import qualified Data.Map.Strict as M (Map, empty, filter, toList)
import           Data.Maybe (fromMaybe)
import           Data.Text as T (Text, null)
import           Data.Text.Lazy.Builder as TB (Builder, fromText)

type    CardanoTracerHandle :: Type -> Type
newtype CardanoTracerHandle user = CardanoTracerHandle
  { inChan :: InChan (CardanoTracerMessage user)
  }

cleanupCardanoTracer :: CardanoTracerHandle user -> IO ()
cleanupCardanoTracer handle =
  writeChan handle.inChan Shutdown

-- | Top-level run function, called by 'cardano-tracer' app.
runCardanoTracer :: Trace IO TracerTrace -> TracerParams -> IO (CardanoTracerHandle ())
runCardanoTracer tracer TracerParams{tracerConfig, stateDir, logSeverity} = do
  traceWith tracer TracerBuildInfo
#if RTVIEW
    { ttBuiltWithRTView = True }
#else
    { ttBuiltWithRTView = False }
#endif
  traceWith tracer TracerParamsAre
    { ttConfigPath     = tracerConfig
    , ttStateDir       = stateDir
    , ttMinLogSeverity = logSeverity
    }

  config <- readTracerConfig tracerConfig
  traceWith tracer TracerConfigIs
    { ttConfig            = config
#if RTVIEW
    , ttWarnRTViewMissing = False
#else
    , ttWarnRTViewMissing = case hasRTView config of { Just{} -> True; Nothing -> False; }
#endif
    }

  brake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  cardanoTracerHandle@CardanoTracerHandle{inChan} <- doRunCardanoTracer config stateDir tracer brake dpRequestors
  traceResourceStats inChan tracer (resourceFreq config)
  pure cardanoTracerHandle

-- | Runs all internal services of the tracer.
doRunCardanoTracer
  :: TracerConfig        -- ^ Tracer's configuration.
  -> Maybe FilePath      -- ^ Path to RTView's internal state files.
  -> Trace IO TracerTrace
  -> ProtocolsBrake      -- ^ The flag we use to stop all the protocols.
  -> DataPointRequestors -- ^ The DataPointRequestors to ask 'DataPoint's.
  -> IO (CardanoTracerHandle ())
doRunCardanoTracer config rtViewStateDir tracer protocolsBrake dpRequestors = do
  traceWith tracer TracerInitStarted
  connectedNodes      <- initConnectedNodes
  connectedNodesNames <- initConnectedNodesNames
  acceptedMetrics     <- initAcceptedMetrics
  mHelp               <- loadMetricsHelp $ metricsHelp config

#if RTVIEW
  savedTO <- initSavedTraceObjects

  chainHistory     <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory        <- initTransactionsHistory
#endif

  currentLogLock <- newLock
  currentDPLock  <- newLock

  traceWith tracer TracerInitEventQueues
#if RTVIEW
  eventsQueues     <- initEventsQueues tracer rtViewStateDir connectedNodesNames dpRequestors currentDPLock
  rtViewPageOpened <- newTVarIO False
#endif

  (reforwardTraceObject, _trDataPoint) <- initReForwarder config tracer

  registry <- newRegistry

  (inChan, _outChan) <- newChan

  -- Environment for all following functions.
  let tracerEnv :: TracerEnv
      tracerEnv = TracerEnv
        { teConfig                = config
        , teConnectedNodes        = connectedNodes
        , teConnectedNodesNames   = connectedNodesNames
        , teAcceptedMetrics       = acceptedMetrics
        , teCurrentLogLock        = currentLogLock
        , teCurrentDPLock         = currentDPLock
        , teDPRequestors          = dpRequestors
        , teProtocolsBrake        = protocolsBrake
        , teTracer                = tracer
        , teReforwardTraceObjects = reforwardTraceObject
        , teRegistry              = registry
        , teStateDir              = rtViewStateDir
        , teMetricsHelp           = mHelp
        , teInChan                = inChan
        }

      tracerEnvRTView :: TracerEnvRTView
      tracerEnvRTView = TracerEnvRTView
#if RTVIEW
        { teSavedTO           = savedTO
        , teBlockchainHistory = chainHistory
        , teResourcesHistory  = resourcesHistory
        , teTxHistory         = txHistory
        , teEventsQueues      = eventsQueues
        , teRTViewPageOpened  = rtViewPageOpened
        }
#endif

  -- Specify what should be done before 'cardano-tracer' stops.
  beforeProgramStops do
    traceWith tracer TracerShutdownInitiated
#if RTVIEW
    backupAllHistory tracerEnv tracerEnvRTView
    traceWith tracer TracerShutdownHistBackup
#endif
    applyBrake (teProtocolsBrake tracerEnv)
    traceWith tracer TracerShutdownComplete

  traceWith tracer TracerInitDone

  let runs :: [IO ()]
      runs =
        [ runLogsRotator    tracerEnv
        , runMetricsServers tracerEnv
        , runAcceptors      tracerEnv tracerEnvRTView
#if RTVIEW
        , runRTView         tracerEnv tracerEnvRTView
#endif
        ]
  sequenceConcurrently_ runs

  pure CardanoTracerHandle
    { inChan
    }

-- NB. this fails silently if there's any read or decode error when an external JSON file is provided
loadMetricsHelp :: Maybe FileOrMap -> IO [(Text, Builder)]
loadMetricsHelp Nothing        = pure []
loadMetricsHelp (Just (FOM x)) = do
  result <- case x of
    Left file -> do
      inp :: Either SomeException (Maybe (M.Map Text Text))
        <- try (decodeFileStrict' file)
      pure $ either (const M.empty) (fromMaybe M.empty) inp
    Right object ->
      pure object
  pure $ (M.toList . fmap TB.fromText . M.filter (not . T.null)) result

traceResourceStats :: InChan (CardanoTracerMessage ()) -> Trace IO TracerTrace -> Maybe Int -> IO ()
traceResourceStats inChan tracer freq =
  for_ @Maybe freq \msInterval -> do
    outChan <- dupChan inChan
    asyncId <- async do
      forever'tilShutdown handleNoop outChan do
        mbrs <- readResourceStats
        for_ @Maybe mbrs \resourceStat -> do
          traceWith tracer (TracerResource resourceStat)
        threadDelay (1_000 * msInterval) -- Delay in seconds, given milliseconds
    link asyncId
