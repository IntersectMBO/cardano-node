{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit
  ( runTxSubmitWebapi
  , opts
  , TxSubmitCommand(..)
  ) where

import qualified Cardano.BM.Setup as Logging
import           Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.BM.Trace as Logging
import           Cardano.Logging (TraceConfig)
import qualified Cardano.Logging.Configuration as TraceD
import           Cardano.Logging.ConfigurationParser (readConfigurationWithDefault)
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
import           Cardano.TxSubmit.Config (GenTxSubmitNodeConfig (..), ToggleLogging (..),
                   TxSubmitNodeConfig, readTxSubmitNodeConfig)
import           Cardano.TxSubmit.Metrics (registerMetricsServer)
import           Cardano.TxSubmit.Tracing.TraceSubmitApi (TraceSubmitApi (..))
import           Cardano.TxSubmit.Web (runTxSubmitServer)

import qualified Control.Concurrent.Async as Async
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Map
import           Data.Text (Text)
import qualified System.Metrics as EKG
import           System.Metrics.Prometheus.Registry (RegistrySample, sample)
import           System.Remote.Monitoring.Prometheus (defaultOptions, samplingFrequency,
                   toPrometheusRegistry)

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
    tsnc <- readTxSubmitNodeConfig (unConfigFile tspConfigFile)
    tracingConfig <- readConfigurationWithDefault (unConfigFile tspConfigFile) defaultTraceConfig
    trce <- mkTracer tsnc
    (trce', registrySample) <- mkTraceDispatcher tracingConfig
    (metrics, runMetricsServer) <- registerMetricsServer trce trce' registrySample tspMetricsPort
    Async.withAsync
      (runTxSubmitServer trce trce' metrics tspWebserverConfig tspProtocol tspNetworkId tspSocketPath)
      $ \txSubmitServer ->
        Async.withAsync runMetricsServer $ \_ ->
          Async.wait txSubmitServer
    logInfo trce "runTxSubmitWebapi: Stopping TxSubmit API"
    TraceD.traceWith trce' ApplicationStopping
  where
    TxSubmitNodeParams
      { tspProtocol
      , tspNetworkId
      , tspSocketPath
      , tspWebserverConfig
      , tspMetricsPort
      , tspConfigFile
      } = tsnp

mkTracer :: TxSubmitNodeConfig -> IO (Trace IO Text)
mkTracer enc = case tscToggleLogging enc of
  LoggingOn -> liftIO $ Logging.setupTrace (Right $ tscLoggingConfig enc) "cardano-tx-submit"
  LoggingOff -> pure Logging.nullTracer

mkTraceDispatcher :: TraceConfig -> IO (TraceD.Trace IO TraceSubmitApi, IO RegistrySample)
mkTraceDispatcher config = do
  trBase <- standardTracer
  ekgStore <- EKG.newStore
  -- TODO: (@russoul) trace-dispatcher addes a postfix "counter" instead of "count" is that expected?
  -- Also, adding those lines below breaks trace-dispatcher
  -- void $ EKG.createCounter "tx_submit_counter" ekgStore
  -- void $ EKG.createCounter "tx_submit_failed_counter" ekgStore
  let registry = toPrometheusRegistry ekgStore (defaultOptions mempty & samplingFrequency .~ 1) -- Convert EKG metrics store to prometheus metrics registry on-demand
  trEkg  <- ekgTracer config ekgStore
  configReflection <- TraceD.emptyConfigReflection
  tr <- TraceD.mkCardanoTracer trBase mempty (Just trEkg) ["TxSubmitApi"]
  TraceD.configureTracers configReflection config [tr]
  pure (tr, registry >>= sample)
