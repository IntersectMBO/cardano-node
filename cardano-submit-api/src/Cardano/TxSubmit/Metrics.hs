{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.TxSubmit.Metrics
  ( TxSubmitMetrics (..)
  , makeMetrics
  , registerMetricsServer
  )
where

import           Cardano.Api.Pretty (textShow)

import           Cardano.BM.Data.Trace (Trace)
import           Cardano.BM.Trace (logError, logInfo, logWarning)
import           Cardano.Logging.Trace (traceWith)
import qualified Cardano.Logging.Types as TraceD
import           Cardano.TxSubmit.Tracing.Message (TraceSubmitApi (..))

import           Control.Exception.Safe
import           Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT (..), registerGauge,
                   runRegistryT, unRegistryT)
import           System.Metrics.Prometheus.Http.Scrape (serveMetricsT)
import           System.Metrics.Prometheus.Metric.Gauge (Gauge)

data TxSubmitMetrics = TxSubmitMetrics
  { tsmCount :: Gauge
  , tsmFailCount :: Gauge
  }

-- | Register metrics server. Returns metrics and an IO action which starts metrics server and should
-- be passed to 'withAsync'.
registerMetricsServer
  :: Trace IO Text
  -> TraceD.Trace IO TraceSubmitApi
  -> Int
  -> IO (TxSubmitMetrics, IO ())
registerMetricsServer tracer tracer' metricsPort =
  runRegistryT $ do
    metrics <- makeMetrics
    registry <- RegistryT ask
    let runServer =
          tryWithPort metricsPort $ \port -> do
            logInfo tracer $ "Starting metrics server on port " <> textShow port
            traceWith tracer' $ MetricsServerStarted port
            flip runReaderT registry . unRegistryT $ serveMetricsT port []
    pure (metrics, runServer)
 where
  -- try opening the metrics server on the specified port, if it fails, try using next. Gives up after 1000 attempts and disables metrics server.
  tryWithPort :: Int -> (Int -> IO ()) -> IO ()
  tryWithPort startingPort f = go startingPort
   where
    go port = do
      catch @_ @IOException (f port) $ \e -> do
        logWarning tracer $ T.pack $ "Metrics server error: " <> displayException e
        traceWith tracer' $ MetricsServerError e
        if port <= (startingPort + 1000)
          then do
            logWarning tracer $ "Could not allocate metrics server port " <> textShow port <> " - trying next available..."
            traceWith tracer' $ MetricsServerPortOccupied port
            go $ port + 1
          else do
            logError tracer $
              "Could not allocate any metrics port until " <> textShow port <> " - metrics endpoint disabled"
            traceWith tracer' $ MetricsServerPortNotBound port

makeMetrics :: RegistryT IO TxSubmitMetrics
makeMetrics =
  TxSubmitMetrics
    <$> registerGauge "tx_submit_count" mempty       -- TODO: (@russoul) Make it into a counter.
    <*> registerGauge "tx_submit_fail_count" mempty  -- TODO: Immediately set to 0 when we create an EKG store
      -- Suffix should be left out in the trace-dispatcher code (asMetrics)
