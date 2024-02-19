{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Monad law, left identity" -}

module Cardano.Logging.Tracer.Composed (
    mkCardanoTracer
  , mkCardanoTracer'
  , mkMetricsTracer
  , traceTracerInfo
  , traceConfigWarnings
  , traceEffectiveConfiguration
  ) where

import           Cardano.Logging.Configuration
import           Cardano.Logging.Formatter
import           Cardano.Logging.Trace
import           Cardano.Logging.TraceDispatcherMessage
import           Cardano.Logging.Types

import           Control.Concurrent.MVar
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import           Data.Text hiding (map)



-- | Construct a tracer according to the requirements for cardano node.
-- The tracer gets a 'name', which is appended to its namespace.
-- The tracer has to be an instance of LogFormatting for the display of
-- messages and an instance of MetaTrace for meta information such as
-- severity, privacy, details and backends'.
-- The tracer gets the backends': 'trStdout', 'trForward' and 'mbTrEkg'
-- as arguments.
-- The returned tracer needs to be configured with a configuration
-- before it is used.
mkCardanoTracer :: forall evt.
     ( LogFormatting evt
     , MetaTrace evt)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> IO (Trace IO evt)
mkCardanoTracer trStdout trForward mbTrEkg tracerPrefix =
    mkCardanoTracer' trStdout trForward mbTrEkg tracerPrefix noHook
  where
    noHook :: Trace IO evt -> IO (Trace IO evt)
    noHook = pure

-- | Adds the possibility to add special tracers via the hook function
mkCardanoTracer' :: forall evt evt1.
     ( LogFormatting evt1
     , MetaTrace evt1
     )
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> (Trace IO evt1 -> IO (Trace IO evt))
  -> IO (Trace IO evt)
mkCardanoTracer' trStdout trForward mbTrEkg tracerPrefix hook = do

    !internalTr <-  backendsAndFormat
                      trStdout
                      trForward
                      Nothing
                      (Trace T.nullTracer)
                    >>= addContextAndFilter

    -- handle the messages
    !messageTrace <- withBackendsFromConfig (backendsAndFormat trStdout trForward)
                    >>= withLimitersFromConfig internalTr
                    >>= traceNamespaceErrors internalTr
                    >>= addContextAndFilter
                    >>= maybeSilent isSilentTracer tracerPrefix False
                    >>= hook

    -- handle the metrics
    !metricsTrace <- case mbTrEkg of
                      Nothing -> pure $ Trace T.nullTracer
                      Just ekgTrace ->
                        pure (metricsFormatter ekgTrace)
--                      >>= recordMetricsStatistics internalTr
                        >>= maybeSilent hasNoMetrics tracerPrefix True
                        >>= hook

    pure (messageTrace <> metricsTrace)

  where
    {-# INLINE addContextAndFilter #-}
    addContextAndFilter :: MetaTrace a => Trace IO a -> IO (Trace IO a)
    addContextAndFilter tr = do
      tr'  <- withDetailsFromConfig
                $ withPrivacy
                  $ withDetails tr
      tr'' <- filterSeverityFromConfig tr'
      pure $ withNames tracerPrefix
             $ withSeverity tr''

    traceNamespaceErrors ::
         Trace IO TraceDispatcherMessage
      -> Trace IO evt1
      -> IO (Trace IO evt1)
    traceNamespaceErrors internalTr (Trace tr) = do
        pure $ Trace (T.arrow (T.emit
          (\case
            (lc, Right e) -> process lc (Right e)
            (lc, Left e) -> T.traceWith tr (lc, Left e))))
      where
        process :: LoggingContext -> Either TraceControl evt1 -> IO ()
        process lc cont = do
          when (isNothing (lcPrivacy lc)) $
                  traceWith
                    (appendPrefixNames ["Reflection"] internalTr)
                    (UnknownNamespace (lcNSPrefix lc) (lcNSInner lc) UKFPrivacy)
          when (isNothing (lcSeverity lc)) $
                  traceWith
                    (appendPrefixNames ["Reflection"] internalTr)
                    (UnknownNamespace (lcNSPrefix lc) (lcNSInner lc) UKFSeverity)
          when (isNothing (lcDetails lc)) $
                  traceWith
                    (appendPrefixNames ["Reflection"] internalTr)
                    (UnknownNamespace (lcNSPrefix lc) (lcNSInner lc) UKFDetails)
          T.traceWith tr (lc, cont)

backendsAndFormat ::
     LogFormatting a
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe [BackendConfig]
  -> Trace IO x
  -> IO (Trace IO a)
backendsAndFormat trStdout trForward mbBackends _ =
  let backends' = fromMaybe
                    [Forwarder, Stdout MachineFormat]
                    mbBackends
  in do
    let mbForwardTrace  = if Forwarder `L.elem` backends'
                            then Just $ filterTraceByPrivacy (Just Public)
                                (forwardFormatter' Nothing trForward)
                            else Nothing
        mbStdoutTrace   | Stdout HumanFormatColoured `L.elem` backends'
                        = Just (humanFormatter' True Nothing trStdout)
                        | Stdout HumanFormatUncoloured `L.elem` backends'
                        = Just (humanFormatter' False Nothing trStdout)
                        | Stdout MachineFormat `L.elem` backends'
                        = Just (machineFormatter' Nothing trStdout)
                        | otherwise = Nothing
    case mbForwardTrace <> mbStdoutTrace of
      Nothing -> pure $ Trace T.nullTracer
      Just tr -> preFormatted backends' tr

traceConfigWarnings ::
     Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> [Text]
  -> IO ()
traceConfigWarnings trStdout trForward errs = do
    internalTr <- backendsAndFormat
                      trStdout
                      trForward
                      Nothing
                      (Trace T.nullTracer)
    traceWith ((withInnerNames . appendPrefixNames ["Reflection"]. withSeverity)
                  internalTr)
              (TracerConsistencyWarnings errs)

traceEffectiveConfiguration ::
     Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> TraceConfig
  -> IO ()
traceEffectiveConfiguration trStdout trForward trConfig = do
    internalTr <- backendsAndFormat
                      trStdout
                      trForward
                      Nothing
                      (Trace T.nullTracer)
    traceWith ((withInnerNames . appendPrefixNames ["Reflection"]. withSeverity)
                  internalTr)
              (TracerInfoConfig trConfig)

traceTracerInfo ::
     Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> ConfigReflection
  -> IO ()
traceTracerInfo trStdout trForward cr = do
    internalTr <- backendsAndFormat
                      trStdout
                      trForward
                      Nothing
                      (Trace T.nullTracer)
    silentSet <- readIORef (crSilent cr)
    metricSet <- readIORef (crNoMetrics cr)
    allTracerSet <- readIORef (crAllTracers cr)
    let silentList  = map (intercalate (singleton '.')) (Set.toList silentSet)
    let metricsList = map (intercalate (singleton '.')) (Set.toList metricSet)
    let allTracersList = map (intercalate (singleton '.')) (Set.toList allTracerSet)
    traceWith ((withInnerNames . appendPrefixNames ["Reflection"]. withSeverity)
                  internalTr)
              (TracerInfo silentList metricsList allTracersList)
    writeIORef (crSilent cr) Set.empty
    writeIORef (crNoMetrics cr) Set.empty
    writeIORef (crAllTracers cr) Set.empty

-- A basic tracer just for metrics
mkMetricsTracer :: Maybe (Trace IO FormattedMessage) -> Trace IO FormattedMessage
mkMetricsTracer mbTrEkg = case mbTrEkg of
                          Nothing -> Trace T.nullTracer
                          Just ekgTrace -> ekgTrace

_recordMetricsStatistics :: forall a m . (LogFormatting a, MonadIO m)
  => Trace m TraceDispatcherMessage
  -> Trace m a
  -> m (Trace m a)
_recordMetricsStatistics internalTr (Trace tr) = do
    ref <- liftIO $ newMVar (0, Map.empty)
    pure $ Trace (T.arrow (T.emit
      (\case
        (lc, Right e) -> process ref lc e
        (lc, Left e) -> T.traceWith tr (lc, Left e))))
  where
    process :: MVar (Int, Map.Map Text Int) -> LoggingContext -> a -> m ()
    process ref lc msg = do
      let metrics = asMetrics msg
      mapM_ (\m ->
              let mName = getMetricName m
              in liftIO $ modifyMVar ref (\ (i', mmap) ->
                  case Map.lookup mName mmap of
                    Nothing -> pure ((i' + 1, Map.insert mName 1 mmap), ())
                    Just _  -> pure ((i' + 1, Map.adjust (+1) mName mmap), ()))) metrics
      (i,mmap) <- liftIO $ readMVar ref
      when (i >= 1000) $ do
        traceWith (withInnerNames (appendPrefixNames ["Reflection"] internalTr))
                  (MetricsInfo mmap)
        liftIO $ modifyMVar ref (\ (_i, mmap') -> pure ((0,mmap'), ()))
      T.traceWith tr (lc, Right msg)
