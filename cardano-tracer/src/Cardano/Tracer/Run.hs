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
#if RTVIEW
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Handlers.RTView.Run
#endif
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, link)
import           Control.Concurrent.Extra (newLock)
#if RTVIEW
import           Control.Concurrent.STM.TVar (newTVarIO)
#endif
import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.Aeson (decodeFileStrict')
import           Data.Foldable (for_)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M (Map, empty, filter, toList)
import           Data.Text as T (Text, null)
import           Data.Text.Lazy.Builder as TB (Builder, fromText)


-- | Top-level run function, called by 'cardano-tracer' app.
runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig, stateDir, logSeverity} = do
  tr <- mkTracerTracer $ SeverityF $ logSeverity <|> Just Info          -- default severity filter to Info
  traceWith tr TracerBuildInfo
#if RTVIEW
    { ttBuiltWithRTView = True
#else
    { ttBuiltWithRTView = False
#endif
    }
  traceWith tr TracerParamsAre
    { ttConfigPath     = tracerConfig
    , ttStateDir       = stateDir
    , ttMinLogSeverity = logSeverity
    }

  config <- readTracerConfig tracerConfig
  traceWith tr TracerConfigIs
    { ttConfig            = config
#if RTVIEW
    , ttWarnRTViewMissing = False
#else
    , ttWarnRTViewMissing = case hasRTView config of { Just{} -> True; Nothing -> False; }
#endif
    }

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
  mHelp               <- loadMetricsHelp $ metricsHelp config

#if RTVIEW
  savedTO <- initSavedTraceObjects

  chainHistory     <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory        <- initTransactionsHistory
#endif

  currentLogLock <- newLock
  currentDPLock  <- newLock

  traceWith tr TracerInitEventQueues
#if RTVIEW
  eventsQueues   <- initEventsQueues tr rtViewStateDir connectedNodesNames dpRequestors currentDPLock
  rtViewPageOpened <- newTVarIO False
#endif

  (reforwardTraceObject,_trDataPoint) <- initReForwarder config tr

  registry <- newRegistry

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
        , teTracer                = tr
        , teReforwardTraceObjects = reforwardTraceObject
        , teRegistry              = registry
        , teStateDir              = rtViewStateDir
        , teMetricsHelp           = mHelp
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
  beforeProgramStops $ do
    traceWith tr TracerShutdownInitiated
#if RTVIEW
    backupAllHistory tracerEnv tracerEnvRTView
#endif
    traceWith tr TracerShutdownHistBackup
    applyBrake (teProtocolsBrake tracerEnv)
    traceWith tr TracerShutdownComplete

  traceWith tr TracerInitDone
  sequenceConcurrently_
    [ runLogsRotator    tracerEnv
    , runMetricsServers tracerEnv
    , runAcceptors      tracerEnv tracerEnvRTView
#if RTVIEW
    , runRTView         tracerEnv tracerEnvRTView
#endif
    ]

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
