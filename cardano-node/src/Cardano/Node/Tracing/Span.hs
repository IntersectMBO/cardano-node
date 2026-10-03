{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | OpenTelemetry-style spans on top of the @trace-dispatcher@ framework.
--
-- A span is a pair of correlated trace messages — 'SpanBegin' and 'SpanEnd' —
-- sharing a single 'SpanId'. 'withSpan' brackets an action so that the end
-- message is always emitted, even on exception, which is what lets a
-- Loki-style alerting system fire on \"span that never ended\".
--
-- The duration is measured /client-side/ (in this process, monotonic clock)
-- and shipped inside 'SpanEnd', so the consumer (cardano-tracer / timeseries /
-- Prometheus) stays stateless — it never has to pair begin\/end itself.
--
--   * For __timeseries\/Prometheus__: 'asMetrics' emits @spanDurationMs@.
--   * For __Loki (LogQL)__: 'forMachine' emits a flat JSON object with a
--     stable @span_id@ (log field, high cardinality) and @name@
--     (label-friendly, low cardinality) plus @event=begin|end@.
module Cardano.Node.Tracing.Span
  ( SpanId (..)
  , SpanTrace (..)
  , newSpanId
  , withSpan
  ) where

import           Cardano.Logging

import           Control.Exception.Safe (MonadMask, finally)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (Value (String), (.=))
import           Data.Text (Text)
import           Data.Unique (hashUnique, newUnique)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

-- | Correlation id shared by a 'SpanBegin' / 'SpanEnd' pair.
--   Process-unique for the lifetime of the run (see 'newSpanId').
newtype SpanId = SpanId { unSpanId :: Word64 }
  deriving (Eq, Ord, Show)

-- | The two ends of a span.
data SpanTrace
  = SpanBegin !SpanId !Text
    -- ^ Start of a span: id + human name of the operation.
  | SpanEnd   !SpanId !Text !Double
    -- ^ End of a span: id, name, and measured duration in milliseconds.
  deriving (Show)

-- | Allocate a fresh, process-unique span id.
--
--   Uses 'Data.Unique' so it needs no extra dependency and never blocks. Ids
--   are unique within a single node run; they are /not/ stable across restarts
--   (fine for correlating one begin with one end — which is all we need).
newSpanId :: MonadIO m => m SpanId
newSpanId = liftIO (SpanId . fromIntegral . hashUnique <$> newUnique)

-- | Run @action@ inside a span, emitting 'SpanBegin' before and 'SpanEnd'
--   after — even if @action@ throws.
--
-- @
--   withSpan tr \"replayLedger\" $ do
--     ...work...
-- @
withSpan
  :: (MonadIO m, MonadMask m)
  => Trace m SpanTrace   -- ^ where to emit the span messages
  -> Text                -- ^ human name of the operation
  -> m a                 -- ^ the work to measure
  -> m a
withSpan tr name action = do
  sid <- newSpanId
  !t0 <- liftIO getMonotonicTimeNSec
  traceWith tr (SpanBegin sid name)
  action `finally` do
    !t1 <- liftIO getMonotonicTimeNSec
    let !ms = fromIntegral (t1 - t0) / 1e6 :: Double
    traceWith tr (SpanEnd sid name ms)

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

instance LogFormatting SpanTrace where
  forMachine _ (SpanBegin sid name) =
    mconcat
      [ "kind"    .= String "SpanBegin"
      , "event"   .= String "begin"
      , "span_id" .= unSpanId sid
      , "name"    .= name
      ]
  forMachine _ (SpanEnd sid name ms) =
    mconcat
      [ "kind"        .= String "SpanEnd"
      , "event"       .= String "end"
      , "span_id"     .= unSpanId sid
      , "name"        .= name
      , "duration_ms" .= ms
      ]

  forHuman (SpanBegin sid name) =
    "Span begin [" <> showT (unSpanId sid) <> "] " <> name
  forHuman (SpanEnd sid name ms) =
    "Span end   [" <> showT (unSpanId sid) <> "] " <> name
      <> " (" <> showT ms <> " ms)"

  -- Only the end carries a measurement. The metric name embeds the span name so
  -- distinct operations are distinguishable; keep the set of names SMALL to
  -- avoid Prometheus/timeseries cardinality blow-up.
  asMetrics (SpanBegin _ _)        = []
  asMetrics (SpanEnd _ name ms)    =
    [ DoubleM ("spanDurationMs." <> name) ms ]

--------------------------------------------------------------------------------
-- Documentation / metadata
--------------------------------------------------------------------------------

instance MetaTrace SpanTrace where
  namespaceFor SpanBegin{} = Namespace [] ["Span", "Begin"]
  namespaceFor SpanEnd{}   = Namespace [] ["Span", "End"]

  severityFor (Namespace _ ["Span", "Begin"]) _ = Just Info
  severityFor (Namespace _ ["Span", "End"])   _ = Just Info
  severityFor _ _ = Nothing

  documentFor (Namespace _ ["Span", "Begin"]) = Just
    "Start of a correlated span. Carries a span_id shared with the matching \
    \Span.End, and the human name of the operation."
  documentFor (Namespace _ ["Span", "End"]) = Just
    "End of a correlated span. Carries the same span_id as Span.Begin plus \
    \the client-side measured duration in milliseconds."
  documentFor _ = Nothing

  metricsDocFor (Namespace _ ["Span", "End"]) =
    [("spanDurationMs", "Client-side measured span duration, in milliseconds")]
  metricsDocFor _ = []

  allNamespaces =
    [ Namespace [] ["Span", "Begin"]
    , Namespace [] ["Span", "End"]
    ]
