{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.EKG (
  ekgTracer
) where

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils (showTReal)

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, intercalate)
import qualified System.Metrics as Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label


-- Using a hashmap, as metrics names typically contain long common prefixes, which is suboptimal for key lookup based on Ord
type Map = Map.HashMap

-- | It is mandatory to construct only one standard tracer in any application!
-- Throwing away a standard tracer and using a new one will result in an exception
ekgTracer :: MonadIO m => TraceConfig -> Metrics.Store -> m (Trace m FormattedMessage)
ekgTracer TraceConfig{tcMetricsPrefix} store = liftIO $ do
    rgsGauges   <- newMVar Map.empty
    rgsLabels   <- newMVar Map.empty
    rgsCounters <- newMVar Map.empty
    pure $ Trace $ T.arrow $ T.emit $
      output rgsGauges rgsLabels rgsCounters
  where
    metricsPrefix = fromMaybe mempty tcMetricsPrefix

    output :: MonadIO m =>
         MVar (Map Text Gauge.Gauge)
      -> MVar (Map Text Label.Label)
      -> MVar (Map Text Counter.Counter)
      -> (LoggingContext, Either TraceControl FormattedMessage)
      -> m ()
    output rgsGauges rgsLabels rgsCounters
      (_, Right (FormattedMetrics m)) =
        liftIO $ mapM_
          (setIt rgsGauges rgsLabels rgsCounters) m
    output _ _ _ p@(_, Left TCDocument {}) =
      docIt EKGBackend p
    output _ _ _ (LoggingContext{}, _) =
      pure ()

    setIt ::
         MVar (Map Text Gauge.Gauge)
      -> MVar (Map Text Label.Label)
      -> MVar (Map Text Counter.Counter)
      -> Metric
      -> IO ()
    setIt rgsGauges rgsLabels rgsCounters = \case
        IntM name theInt -> do
          let fullName = metricsPrefix <> name <> "_int"
          gauge <- modifyMVar rgsGauges (setFunc Metrics.createGauge fullName)
          Gauge.set gauge (fromIntegral theInt)
        DoubleM name theDouble -> do
          let fullName = metricsPrefix <> name <> "_real"
          label <- modifyMVar rgsLabels (setFunc Metrics.createLabel fullName)
          Label.set label (showTReal theDouble)
        PrometheusM name keyLabels -> do
          let fullName = metricsPrefix <> name
          label <- modifyMVar rgsLabels (setFunc Metrics.createLabel fullName)
          Label.set label (presentPrometheusM keyLabels)
        CounterM name mbInt -> do
          let fullName = metricsPrefix <> name <> "_counter"
          counter <- modifyMVar rgsCounters (setFunc Metrics.createCounter fullName)
          case mbInt of
            Nothing -> Counter.inc counter
            Just i  -> Counter.add counter (fromIntegral i)

    setFunc ::
         (Text -> Metrics.Store -> IO m)
      -> Text
      -> Map Text m
      -> IO (Map Text m, m)
    setFunc createAction name rgsMap =
        case Map.lookup name rgsMap of
          Just metric -> pure (rgsMap, metric)
          Nothing -> do
            metric <- createAction name store
            let rgsMap' = Map.insert name metric rgsMap
            pure (rgsMap', metric)

    presentPrometheusM :: [(Text, Text)] -> Text
    presentPrometheusM =
      label . map pair
      where
        label pairs = "{" <> intercalate "," pairs <> "} 1"
        pair (k, v) = k <> "=\"" <> v <> "\""
