{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit
  ( runTxSubmitWebapi
  , opts
  , TxSubmitCommand(..)
  ) where

import           Cardano.Logging (TraceConfig)
import qualified Cardano.Logging.Configuration as TraceD
import           Cardano.Logging.ConfigurationParser (readConfigurationWithDefault)
import           Cardano.Logging.Trace (traceWith)
import qualified Cardano.Logging.Trace as TraceD
import qualified Cardano.Logging.Tracer.Composed as TraceD
import           Cardano.Logging.Tracer.EKG (ekgTracer)
import           Cardano.Logging.Tracer.Standard (standardTracer)
import           Cardano.Logging.Types (BackendConfig (..),
                   ConfigOption (ConfBackend, ConfSeverity), FormatLogging (HumanFormatColoured),
                   SeverityF (SeverityF), SeverityS (Info))
import qualified Cardano.Logging.Types as TraceD
import           Cardano.TxSubmit.CLI.Parsers (opts)
import           Cardano.TxSubmit.CLI.Types (ConfigFile (unConfigFile), TxSubmitCommand (..),
                   TxSubmitNodeParams (..))
import           Cardano.TxSubmit.Metrics (registerMetricsServer)
import           Cardano.TxSubmit.Tracing.TraceSubmitApi (TraceSubmitApi (..))
import           Cardano.TxSubmit.Web (runTxSubmitServer)

import qualified Control.Concurrent.Async as Async
import           Data.Map
import qualified System.Metrics as EKG
import           System.Metrics.Prometheus.Registry (RegistrySample, sample)
import           System.Remote.Monitoring.Prometheus (defaultOptions, toPrometheusRegistry)

defaultTraceConfig :: TraceConfig
defaultTraceConfig =
  TraceD.emptyTraceConfig
    { TraceD.tcOptions = Data.Map.fromList
        [([], [ ConfSeverity (SeverityF (Just Info))
              , ConfBackend [Stdout HumanFormatColoured, EKGBackend]])
        ]
    }

runTxSubmitWebapi :: TxSubmitNodeParams -> IO ()
runTxSubmitWebapi tsnp = do
    tracingConfig <- readConfigurationWithDefault (unConfigFile tspConfigFile) defaultTraceConfig
    (trce, registrySample) <- mkTraceDispatcher tracingConfig
    Async.withAsync
      (runTxSubmitServer trce tspWebserverConfig tspProtocol tspNetworkId tspSocketPath)
      $ \txSubmitServer ->
        Async.withAsync (registerMetricsServer trce registrySample tspMetricsPort) $ \_ ->
          Async.wait txSubmitServer
    TraceD.traceWith trce ApplicationStopping
  where
    TxSubmitNodeParams
      { tspProtocol
      , tspNetworkId
      , tspSocketPath
      , tspWebserverConfig
      , tspMetricsPort
      , tspConfigFile
      } = tsnp

mkTraceDispatcher :: TraceConfig -> IO (TraceD.Trace IO TraceSubmitApi, IO RegistrySample)
mkTraceDispatcher config = do
  trBase <- standardTracer
  ekgStore <- EKG.newStore
  let registry = toPrometheusRegistry ekgStore (defaultOptions mempty) -- Convert EKG metrics store to prometheus metrics registry on-demand
  trEkg  <- ekgTracer config ekgStore
  configReflection <- TraceD.emptyConfigReflection
  tr <- TraceD.mkCardanoTracer trBase mempty (Just trEkg) ["TxSubmitApi"]
  TraceD.configureTracers configReflection config [tr]
  traceWith tr ApplicationInitializeMetrics
  pure (tr, registry >>= sample)
