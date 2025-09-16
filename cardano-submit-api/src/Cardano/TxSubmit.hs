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
import qualified Cardano.Logging.Trace as TraceD
import qualified Cardano.Logging.Tracer.Composed as TraceD
import           Cardano.Logging.Tracer.Standard (standardTracer)
import           Cardano.Logging.Types (BackendConfig (EKGBackend, Stdout), ConfigOption (..),
                   FormatLogging (HumanFormatColoured), SeverityF (..), SeverityS (Info),
                   emptyConfigReflection, tcOptions)
import qualified Cardano.Logging.Types as TraceD
import           Cardano.TxSubmit.CLI.Parsers (opts)
import           Cardano.TxSubmit.CLI.Types (ConfigFile (unConfigFile), TxSubmitCommand (..),
                   TxSubmitNodeParams (..))
import           Cardano.TxSubmit.Config (GenTxSubmitNodeConfig (..), ToggleLogging (..),
                   TxSubmitNodeConfig, readTxSubmitNodeConfig)
import           Cardano.TxSubmit.Metrics (registerMetricsServer)
import           Cardano.TxSubmit.Tracing.Message (Message (..), MetricAction (..))
import qualified Cardano.TxSubmit.Tracing.Message as TraceD
import           Cardano.TxSubmit.Web (runTxSubmitServer)

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

runTxSubmitWebapi :: TxSubmitNodeParams -> IO ()
runTxSubmitWebapi tsnp = do
    tsnc <- readTxSubmitNodeConfig (unConfigFile tspConfigFile)
    trce <- mkTracer tsnc
    trce' <- mkTraceDispatcher tsnc
    (metrics, runMetricsServer) <- registerMetricsServer trce trce' tspMetricsPort
    Async.withAsync
      (runTxSubmitServer trce trce' metrics tspWebserverConfig tspProtocol tspNetworkId tspSocketPath)
      $ \txSubmitServer ->
        Async.withAsync runMetricsServer $ \_ ->
          Async.wait txSubmitServer
    logInfo trce "runTxSubmitWebapi: Stopping TxSubmit API"
    TraceD.traceWith trce' (Message Info "runTxSubmitWebapi: Stopping TxSubmit API" MetricActionNone)
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

mkTraceDispatcher :: TxSubmitNodeConfig -> IO (TraceD.Trace IO Message)
mkTraceDispatcher config = case tscToggleLogging config of
  LoggingOn -> do -- TODO: (@russoul) apply the configuration from `config` to the tracer
    let trConfig = TraceD.emptyTraceConfig
            { tcOptions = Map.fromList
                [([], [ ConfSeverity (SeverityF (Just Info))
                      , ConfBackend [Stdout HumanFormatColoured, EKGBackend]])
                ]
            }

    trBase <- standardTracer
    -- TODO: (@russoul) set up EKG?
    -- ekgStore <- EKG.newStore
    -- trEkg  <- ekgTracer trConfig ekgStore
    configReflection <- emptyConfigReflection
    TraceD.mkCardanoTracer trBase mempty Nothing [{- TODO @russoul -}]
  LoggingOff -> pure mempty
