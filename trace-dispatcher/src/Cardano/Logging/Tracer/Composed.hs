{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Composed (
    mkCardanoTracer
  , mkCardanoTracer'
  ) where

import           Data.Maybe                       (fromMaybe)
import           Data.Text

import           Cardano.Logging.Configuration
import           Cardano.Logging.Formatter
import           Cardano.Logging.FrequencyLimiter (LimitingMessage (..))
import           Cardano.Logging.Trace
import           Cardano.Logging.Types

import qualified Control.Tracer                   as NT

data MessageOrLimit m = Message m | Limit LimitingMessage

instance (LogFormatting m) => LogFormatting (MessageOrLimit m) where
  forMachine dtal (Message m) = forMachine dtal m
  forMachine dtal (Limit m)   = forMachine dtal m
  forHuman (Message m) = forHuman m
  forHuman (Limit m)   = forHuman m
  asMetrics (Message m) = asMetrics m
  asMetrics (Limit m)   = asMetrics m

-- | Construct a tracer according to the requirements for cardano node.
--
-- The tracer gets a 'name', which is appended to its namespace.
--
-- The tracer gets a 'namesFor', 'severityFor' and 'privacyFor' function
-- as arguments, to set the logging context accordingly.
--
-- The tracer gets the backends: 'trStdout', 'trForward' and 'mbTrEkg'
-- as arguments.
--
-- The returned tracer need to be configured for the specification of
-- filtering, detailLevel, frequencyLimiting and backends with formatting before use.
mkCardanoTracer :: forall evt.
     LogFormatting evt
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> (evt -> Privacy)
  -> IO (Trace IO evt)
mkCardanoTracer trStdout trForward mbTrEkg name namesFor severityFor privacyFor =
    mkCardanoTracer' trStdout trForward mbTrEkg name namesFor severityFor
        privacyFor noHook
  where
    noHook :: Trace IO evt -> IO (Trace IO evt)
    noHook tr = pure tr

-- | Adds the possibility to add special tracers via the hook function
mkCardanoTracer' :: forall evt evt1.
     LogFormatting evt1
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> (evt -> Privacy)
  -> (Trace IO evt1 -> IO (Trace IO evt))
  -> IO (Trace IO evt)
mkCardanoTracer' trStdout trForward mbTrEkg name namesFor severityFor privacyFor
  hook = do
    tr    <- withBackendsFromConfig backendsAndFormat
    tr'   <- withLimitersFromConfig (NT.contramap Message tr) (NT.contramap Limit tr)
    tr''  <- hook tr'
    addContextAndFilter tr''
  where
    addContextAndFilter :: Trace IO evt -> IO (Trace IO evt)
    addContextAndFilter tr = do
      tr'  <- withDetailsFromConfig tr
      tr'' <- filterSeverityFromConfig tr'
      pure $ withNamesAppended namesFor
            $ appendName name
              $ appendName "Node"
                $ withSeverity severityFor
                  $ withPrivacy privacyFor
                    tr''

    backendsAndFormat ::
         Maybe [BackendConfig]
      -> Trace m x
      -> IO (Trace IO (MessageOrLimit evt1))
    backendsAndFormat mbBackends _ =
      let backends = fromMaybe
                      [EKGBackend, Forwarder, Stdout HumanFormatColoured]
                      mbBackends
      in do
        mbEkgTrace     <- case mbTrEkg of
                            Nothing -> pure Nothing
                            Just ekgTrace ->
                              if EKGBackend `elem` backends
                                then fmap Just
                                      (metricsFormatter "Cardano" ekgTrace)
                                else pure Nothing
        mbForwardTrace <- if Forwarder `elem` backends
                            then fmap (Just . filterTraceByPrivacy (Just Public))
                                  (forwardFormatter "Cardano" trForward)
                            else pure Nothing
        mbStdoutTrace  <-  if Stdout HumanFormatColoured `elem` backends
                            then fmap Just
                                (humanFormatter True "Cardano" trStdout)
                            else if Stdout HumanFormatUncoloured `elem` backends
                              then fmap Just
                                  (humanFormatter False "Cardano" trStdout)
                              else if Stdout MachineFormat `elem` backends
                                then fmap Just
                                  (machineFormatter "Cardano" trStdout)
                                else pure Nothing
        case mbEkgTrace <> mbForwardTrace <> mbStdoutTrace of
          Nothing -> pure $ Trace NT.nullTracer
          Just tr -> pure (preFormatted backends tr)
