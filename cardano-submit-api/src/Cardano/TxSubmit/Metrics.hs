{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Metrics
  ( TxSubmitMetrics(..)
  , makeMetrics
  , registerMetricsServer
  ) where

import           Cardano.Prelude

import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT (..), registerGauge,
                    runRegistryT, unRegistryT)
import           System.Metrics.Prometheus.Metric.Gauge (Gauge)
import           System.Metrics.Prometheus.Http.Scrape (serveMetricsT)


newtype TxSubmitMetrics = TxSubmitMetrics
  { tsmCount :: Gauge
  }

registerMetricsServer :: IO (TxSubmitMetrics, Async ())
registerMetricsServer =
  runRegistryT $ do
    metrics <- makeMetrics
    registry <- RegistryT ask
    server <- liftIO . async $ runReaderT (unRegistryT $ serveMetricsT 8081 []) registry
    pure (metrics, server)

makeMetrics :: RegistryT IO TxSubmitMetrics
makeMetrics =
  TxSubmitMetrics
    <$> registerGauge "tx_submit_count" mempty
