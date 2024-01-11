{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.EKG (
  ekgTracer
) where

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import qualified Data.Map.Strict as Map
import           Data.Text (Text, pack)
import qualified System.Metrics as Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import           System.Remote.Monitoring (Server, getCounter, getGauge, getLabel)


-- | It is mandatory to construct only one standard tracer in any application!
-- Throwing away a standard tracer and using a new one will result in an exception
ekgTracer :: MonadIO m => Either Metrics.Store Server-> m (Trace m FormattedMessage)
ekgTracer storeOrServer = liftIO $ do
    rgsGauges   <- newMVar Map.empty
    rgsLabels   <- newMVar Map.empty
    rgsCounters <- newMVar Map.empty
    pure $ Trace $ T.arrow $ T.emit $
      output rgsGauges rgsLabels rgsCounters
  where
    output :: MonadIO m =>
         MVar (Map.Map Text Gauge.Gauge)
      -> MVar (Map.Map Text Label.Label)
      -> MVar (Map.Map Text Counter.Counter)
      -> (LoggingContext, Either TraceControl FormattedMessage)
      -> m ()
    output rgsGauges rgsLabels rgsCounters
      (LoggingContext{..}, Right (FormattedMetrics m)) =
        liftIO $ mapM_
          (setIt rgsGauges rgsLabels rgsCounters (lcNSPrefix ++ lcNSInner)) m
    output _ _ _ p@(_, Left TCDocument {}) =
      docIt EKGBackend p
    output _ _ _ (LoggingContext{}, _) =
      pure ()

    setIt ::
         MVar (Map.Map Text Gauge.Gauge)
      -> MVar (Map.Map Text Label.Label)
      -> MVar (Map.Map Text Counter.Counter)
      -> [Text]
      -> Metric
      -> IO ()
    setIt rgsGauges _rgsLabels _rgsCounters _namespace
      (IntM name theInt) = do
      gauge <- modifyMVar rgsGauges (setFunc Metrics.createGauge getGauge name)
      Gauge.set gauge (fromIntegral theInt)
    setIt _rgsGauges rgsLabels _rgsCounters _namespace
      (DoubleM name theDouble) = do
        label <- modifyMVar rgsLabels (setFunc Metrics.createLabel getLabel name)
        Label.set label ((pack . show) theDouble)
    setIt _rgsGauges _rgsLabels rgsCounters _namespace
      (CounterM name mbInt) = do
        counter <- modifyMVar rgsCounters (setFunc Metrics.createCounter getCounter name)
        case mbInt of
          Nothing -> Counter.inc counter
          Just i  -> Counter.add counter (fromIntegral i)

    setFunc ::
         (Text -> Metrics.Store -> IO m)
      -> (Text -> Server -> IO m)
      -> Text
      -> Map.Map Text m
      -> IO (Map.Map Text m, m)
    setFunc creator1 creator2 name rgsMap = do
        case Map.lookup name rgsMap of
          Just gauge -> do
            pure (rgsMap, gauge)
          Nothing -> do
            gauge <- case storeOrServer of
                        Left store   -> creator1 name store
                        Right server -> creator2 name server
            let rgsMap' = Map.insert name gauge rgsMap
            pure (rgsMap', gauge)
