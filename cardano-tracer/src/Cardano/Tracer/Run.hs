{- HLINT ignore "Avoid lambda" -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | This top-level module is used by 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( doRunCardanoTracer
  , runCardanoTracer
  ) where

import           Hermod.Tracing.Resources
import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.CLI
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.Rotator
import           Cardano.Tracer.Handlers.Metrics.Servers
import           Cardano.Tracer.Handlers.ReForwarder
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils
import           Cardano.Timeseries.API (Tree)
import qualified Cardano.Timeseries.Component as Timeseries

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, link)
import           Control.Concurrent.Extra (newLock)
import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.Aeson (decodeFileStrict')
import           Data.Foldable (for_)
import           Data.Traversable (for)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M (Map, empty, filter, toList)
import           Data.Text as T (Text, null)
import           Data.Text.Lazy.Builder as TB (Builder, fromText)


-- | Top-level run function, called by 'cardano-tracer' app.
runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{tracerConfig, stateDir, logSeverity} = do
  !tr <- mkTraceBundle $ SeverityF $ logSeverity <|> Just Info          -- default severity filter to Info
  traceWith tr.assorted TracerBuildInfo
  traceWith tr.assorted TracerParamsAre
    { ttConfigPath     = tracerConfig
    , ttStateDir       = stateDir
    , ttMinLogSeverity = logSeverity
    }

  config <- readTracerConfig tracerConfig
  traceWith tr.assorted TracerConfigIs
    { ttConfig = config
    }

  for_ (resourceFreq config) \msInterval -> do
    threadId <- async do
      forever do
        mbrs <- readResourceStats
        for_ mbrs \resourceStat ->
          traceWith tr.assorted (TracerResource resourceStat)
        threadDelay (1_000 * msInterval) -- Delay in seconds, given milliseconds
    link threadId

  brake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  doRunCardanoTracer config stateDir tr brake dpRequestors

-- | Runs all internal services of the tracer.
doRunCardanoTracer
  :: TracerConfig        -- ^ Tracer's configuration.
  -> Maybe FilePath      -- ^ Path to tracer's internal state files.
  -> TraceBundle
  -> ProtocolsBrake      -- ^ The flag we use to stop all the protocols.
  -> DataPointRequestors -- ^ The DataPointRequestors to ask 'DataPoint's.
  -> IO ()
doRunCardanoTracer config stateDir tr protocolsBrake dpRequestors = do
  traceWith tr.assorted TracerInitStarted
  connectedNodes      <- initConnectedNodes
  connectedNodesNames <- initConnectedNodesNames
  acceptedMetrics <- initAcceptedMetrics
  mHelp               <- loadMetricsHelp $ metricsHelp config

  currentLogLock <- newLock
  currentDPLock  <- newLock

  (reforwardTraceObject,_trDataPoint) <- initReForwarder config tr.assorted

  registry <- newRegistry

  !timeseriesHandle <- for (hasTimeseries config) (const $ Timeseries.create @(Tree _) tr.timeseries Nothing)

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
        , teTracer                = tr.assorted
        , teReforwardTraceObjects = reforwardTraceObject
        , teRegistry              = registry
        , teStateDir              = stateDir
        , teMetricsHelp           = mHelp
        , teTimeseriesHandle      = timeseriesHandle
        }

  -- Specify what should be done before 'cardano-tracer' stops.
  beforeProgramStops $ do
    traceWith tr.assorted TracerShutdownInitiated
    applyBrake (teProtocolsBrake tracerEnv)
    traceWith tr.assorted TracerShutdownComplete

  traceWith tr.assorted TracerInitDone
  sequenceConcurrently_
    [ runLogsRotator    tracerEnv
    , runMetricsServers tracerEnv
    , runAcceptors      tracerEnv
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
