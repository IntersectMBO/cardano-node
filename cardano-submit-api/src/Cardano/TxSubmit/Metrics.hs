{-# LANGUAGE TypeApplications #-}

module Cardano.TxSubmit.Metrics
  (registerMetricsServer)
where

import           Cardano.Logging.Trace (traceWith)
import qualified Cardano.Logging.Types as TraceD
import           Cardano.TxSubmit.Tracing.TraceSubmitApi (TraceSubmitApi (..))

import           Control.Exception.Safe
import           System.Metrics.Prometheus.Http.Scrape (serveMetrics)
import           System.Metrics.Prometheus.Registry (RegistrySample)

-- | Register metrics server. Returns metrics and an IO action which starts metrics server and should
-- be passed to 'withAsync'.
registerMetricsServer
  :: TraceD.Trace IO TraceSubmitApi
  -> IO RegistrySample
  -> Int
  -> IO ()
registerMetricsServer tracer registrySample metricsPort = do
  tryWithPort metricsPort $ \port -> do
    traceWith tracer $ MetricsServerStarted port
    serveMetrics port [] registrySample
 where

  -- try opening the metrics server on the specified port, if it fails, try using next. Gives up after 1000 attempts and disables metrics server.
  tryWithPort :: Int -> (Int -> IO ()) -> IO ()
  tryWithPort startingPort f = go startingPort
   where
    go port = do
      catch @_ @IOException (f port) $ \e -> do
        traceWith tracer $ MetricsServerError e
        if port <= (startingPort + 1000)
          then do
            traceWith tracer $ MetricsServerPortOccupied port
            go $ port + 1
          else
            traceWith tracer $ MetricsServerPortNotBound port
