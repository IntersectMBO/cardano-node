{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Monad law, left identity" -}

module Cardano.Logging.Tracer.Composed (
    traceTracerInfo
  , mkCardanoTracer
  , mkCardanoTracer'
  , mkMetricsTracer
  ) where

import           Cardano.Logging.Configuration
import           Cardano.Logging.Formatter
import           Cardano.Logging.Trace
import           Cardano.Logging.TraceDispatcherMessage
import           Cardano.Logging.Types


import qualified Control.Tracer as NT
import           Data.IORef
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text hiding (map)


traceTracerInfo ::
     Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> ConfigReflection
  -> IO ()
traceTracerInfo trStdout trForward (ConfigReflection silentRef metricsRef) = do
    internalTr <- backendsAndFormat
                      trStdout
                      trForward
                      (Just [Forwarder, Stdout MachineFormat])
                      (Trace NT.nullTracer)
    silentSet <- readIORef silentRef
    metricSet <- readIORef metricsRef
    let silentList  = map (intercalate (singleton '.')) (Set.toList silentSet)
    let metricsList = map (intercalate (singleton '.')) (Set.toList metricSet)
    traceWith (withInnerNames (appendPrefixNames ["Reflection"] internalTr))
              (TracerInfo silentList metricsList)
    writeIORef silentRef Set.empty
    writeIORef metricsRef Set.empty

-- | Construct a tracer according to the requirements for cardano node.
-- The tracer gets a 'name', which is appended to its namespace.
-- The tracer has to be an instance of LogFormat-ting for the display of
-- messages and an instance of MetaTrace for meta information such as
-- severity, privacy, details and backends'.
-- The tracer gets the backends': 'trStdout', 'trForward' and 'mbTrEkg'
-- as arguments.
-- The returned tracer needs to be configured with a configuration.

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
     , MetaTrace evt1)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> (Trace IO evt1 -> IO (Trace IO evt))
  -> IO (Trace IO evt)
mkCardanoTracer' trStdout trForward mbTrEkg tracerPrefix hook = do

    internalTr <- fmap (appendPrefixNames ["Reflection"])
                       (withBackendsFromConfig (backendsAndFormat trStdout trForward))

    -- handle the messages
    messageTrace <- withBackendsFromConfig (backendsAndFormat trStdout trForward)
                    >>= withLimitersFromConfig internalTr
                    >>= addContextAndFilter internalTr
                    >>= maybeSilent isSilentTracer tracerPrefix False
                    >>= hook

    -- handle the metrics
    metricsTrace <- (maybeSilent hasNoMetrics tracerPrefix True
                        . filterTrace (\ (_, v) -> not (Prelude.null (asMetrics v))))
                        (case mbTrEkg of
                            Nothing -> Trace NT.nullTracer
                            Just ekgTrace -> metricsFormatter "Cardano" ekgTrace)
                    >>= hook
    pure (messageTrace <> metricsTrace)


  where
    -- TODO YUP: More flexible error handling
    addContextAndFilter ::
         Trace IO TraceDispatcherMessage
      -> Trace IO evt1
      -> IO (Trace IO evt1)
    addContextAndFilter tri tr = do
      tr' <- withDetailsFromConfig tr
              >>= filterSeverityFromConfig
              >>= withSeverity' (traceWith tri)
              >>= withPrivacy' (traceWith tri)
              >>= withDetails' (traceWith tri)
      pure $ withInnerNames $ appendPrefixNames tracerPrefix tr'

backendsAndFormat ::
     LogFormatting a
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe [BackendConfig]
  -> Trace IO x
  -> IO (Trace IO a)
backendsAndFormat trStdout trForward mbBackends _ =
  let backends' = fromMaybe
                  [EKGBackend, Forwarder, Stdout HumanFormatColoured]
                  mbBackends
  in do
    mbForwardTrace <- if Forwarder `L.elem` backends'
                        then fmap (Just . filterTraceByPrivacy (Just Public))
                              (forwardFormatter Nothing trForward)
                        else pure Nothing
    mbStdoutTrace  <-  if Stdout HumanFormatColoured `L.elem` backends'
                        then fmap Just
                            (humanFormatter True Nothing trStdout)
                        else if Stdout HumanFormatUncoloured `L.elem` backends'
                          then fmap Just
                              (humanFormatter False Nothing trStdout)
                          else if Stdout MachineFormat `L.elem` backends'
                            then fmap Just
                              (machineFormatter Nothing trStdout)
                            else pure Nothing
    case mbForwardTrace <> mbStdoutTrace of
      Nothing -> pure $ Trace NT.nullTracer
      Just tr -> pure $ preFormatted backends' tr

-- A basic ttracer just for metrics
mkMetricsTracer :: Maybe (Trace IO FormattedMessage) -> Trace IO FormattedMessage
mkMetricsTracer mbTrEkg = case mbTrEkg of
                          Nothing -> Trace NT.nullTracer
                          Just ekgTrace -> ekgTrace
