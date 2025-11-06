{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit
  ( runTxSubmitWebapi
  , opts
  , TxSubmitCommand(..)
  ) where

import           Cardano.Logging (BackendConfig (..), ConfigOption (ConfBackend, ConfSeverity),
                   FormatLogging (HumanFormatColoured), SeverityF (SeverityF), SeverityS (Info),
                   Trace, TraceConfig, configureTracers, ekgTracer, emptyConfigReflection,
                   emptyTraceConfig, mkCardanoTracer, readConfigurationWithDefault, standardTracer,
                   tcOptions, traceWith)
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
   emptyTraceConfig
    { tcOptions = Data.Map.fromList
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
    traceWith trce ApplicationStopping
  where
    TxSubmitNodeParams
      { tspProtocol
      , tspNetworkId
      , tspSocketPath
      , tspWebserverConfig
      , tspMetricsPort
      , tspConfigFile
      } = tsnp

mkTraceDispatcher :: TraceConfig -> IO (Trace IO TraceSubmitApi, IO RegistrySample)
mkTraceDispatcher config = do
  trBase <- standardTracer
  ekgStore <- EKG.newStore
  let registry = toPrometheusRegistry ekgStore (defaultOptions mempty) -- Convert EKG metrics store to prometheus metrics registry on-demand
  trEkg  <- ekgTracer config ekgStore
  configReflection <- emptyConfigReflection
  tr <- mkCardanoTracer trBase mempty (Just trEkg) ["TxSubmitApi"]
  configureTracers configReflection config [tr]
  traceWith tr ApplicationInitializeMetrics
  pure (tr, registry >>= sample)
