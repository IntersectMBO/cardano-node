{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | OpenTelemetry-style spans on top of the @trace-dispatcher@ framework.
--
-- A span is a pair of correlated trace messages — 'SpanBegin' and 'SpanEnd' —
-- sharing a single 'SpanId'. 'withSpan' brackets an action so that the end
-- message is always emitted, even on exception, which is what lets a
-- Loki-style alerting system fire on \"span that never ended\".
--
-- Spans nest: every span in one logical operation shares the same
-- 'TraceId', and each nested span carries its parent's 'SpanId' as
-- @parent_span_id@. Nesting is threaded through a 'SpanContext'; inner
-- 'withSpan' calls read the currently-active span from the context, attach
-- themselves as its child, and restore the previous state on the way out.
--
-- The duration is measured /client-side/ (in this process, monotonic clock)
-- and shipped inside 'SpanEnd', so the consumer (cardano-tracer / timeseries /
-- Prometheus) stays stateless — it never has to pair begin\/end itself.
--
--   * For __timeseries\/Prometheus__: 'asMetrics' emits @spanDurationMs@.
--   * For __Loki (LogQL)__: 'forMachine' emits a flat JSON object with the
--     standard OTel fields @trace_id@, @span_id@, @parent_span_id@ (as
--     lowercase hex strings), @name@, @event=begin|end@, and, on end,
--     @duration_ms@.
--
-- /Concurrency:/ the 'SpanContext' is a single 'IORef', safe for sequential
-- nesting inside one logical thread of work. If you fork off parallel work
-- that should form its own subtree, snapshot with a fresh 'newSpanContext'
-- (optionally seeded from 'readCurrentSpan') rather than sharing one context
-- across threads.
module Cardano.Node.Tracing.Span
  ( SpanId (..)
  , TraceId (..)
  , SpanTrace (..)
  , SpanContext
  , newSpanContext
  , newSpanId
  , newTraceId
  , readCurrentSpan
  , withSpan
  , formatSpanIdHex
  , formatTraceIdHex
  ) where

import           Cardano.Logging

import           Control.Exception.Safe (MonadMask, finally)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (Value (String), (.=))
import           Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Unique (hashUnique, newUnique)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)
import           Numeric (showHex)

-- | 64-bit span identifier. OTel conventionally renders this as
--   16-character lowercase hex; see 'formatSpanIdHex'.
newtype SpanId = SpanId { unSpanId :: Word64 }
  deriving stock (Eq, Ord, Show)

-- | 128-bit trace identifier shared by every span belonging to the same
--   logical operation. OTel conventionally renders this as 32-character
--   lowercase hex; see 'formatTraceIdHex'.
data TraceId = TraceId !Word64 !Word64
  deriving stock (Eq, Ord, Show)

-- | The two ends of a span.
--
--   'SpanBegin' carries the shared 'TraceId', the span's own 'SpanId', a
--   'Maybe SpanId' for the parent ('Nothing' at the root of a trace), and
--   the human name of the operation. 'SpanEnd' carries the same 'TraceId'
--   and 'SpanId' plus the measured duration in milliseconds.
data SpanTrace
  = SpanBegin !TraceId !SpanId !(Maybe SpanId) !Text
  | SpanEnd   !TraceId !SpanId !Text !Double
  deriving stock (Show)

-- | Mutable context threaded through nested 'withSpan' calls. Holds the
--   currently-active @(trace_id, span_id)@ so an inner 'withSpan' can attach
--   itself as a child of the outer one without explicit plumbing.
newtype SpanContext = SpanContext
  { spanCurrent :: IORef (Maybe (TraceId, SpanId))
  }

-- | Allocate a fresh top-level context. The first 'withSpan' against it
--   mints a new trace id and starts a root span.
newSpanContext :: MonadIO m => m SpanContext
newSpanContext = liftIO $ SpanContext <$> newIORef Nothing

-- | Allocate a fresh, process-unique span id.
--
--   Uses 'Data.Unique' so it needs no extra dependency and never blocks. Ids
--   are unique within a single node run; they are /not/ stable across restarts
--   (fine for correlating one begin with one end — which is all we need).
newSpanId :: MonadIO m => m SpanId
newSpanId = liftIO (SpanId . fromIntegral . hashUnique <$> newUnique)

-- | Allocate a fresh 128-bit trace id. Uses two consecutive 'Data.Unique'
--   allocations; process-unique, sufficient for downstream correlation. Not
--   cryptographically random.
newTraceId :: MonadIO m => m TraceId
newTraceId = liftIO $
      (TraceId . fromIntegral . hashUnique <$> newUnique)
  <*> (fromIntegral . hashUnique <$> newUnique)

-- | Snapshot the span currently active in a context. 'Nothing' when no
--   span is active. Useful in tests and when injecting trace context into
--   a downstream request header (e.g. W3C @traceparent@).
readCurrentSpan :: MonadIO m => SpanContext -> m (Maybe (TraceId, SpanId))
readCurrentSpan = liftIO . readIORef . spanCurrent

-- | Run @action@ inside a span, emitting 'SpanBegin' before and 'SpanEnd'
--   after — even if @action@ throws.
--
--   The span's parent is whatever span is currently active in @ctx@; when
--   nothing is active, a fresh 'TraceId' is minted and this becomes a root
--   span. The context is updated on the way in and restored on the way out,
--   so sequential nesting composes without explicit threading.
--
-- @
--   ctx <- newSpanContext
--   withSpan tr ctx \"replayLedger\" $ do
--     ...outer work...
--     withSpan tr ctx \"flushWAL\" $ do
--       ...inner work — its parent_span_id is the outer span's id...
-- @
withSpan
  :: (MonadIO m, MonadMask m)
  => Trace m SpanTrace   -- ^ where to emit the span messages
  -> SpanContext         -- ^ nesting context shared by the enclosing scope
  -> Text                -- ^ human name of the operation
  -> m a                 -- ^ the work to measure
  -> m a
withSpan tr ctx name action = do
  parent <- readCurrentSpan ctx
  (traceId, parentSid) <- case parent of
    Nothing         -> do !tid <- newTraceId; pure (tid, Nothing)
    Just (tid, pid) -> pure (tid, Just pid)
  sid <- newSpanId
  !t0 <- liftIO getMonotonicTimeNSec
  liftIO $ atomicWriteIORef (spanCurrent ctx) (Just (traceId, sid))
  traceWith tr (SpanBegin traceId sid parentSid name)
  action `finally` do
    !t1 <- liftIO getMonotonicTimeNSec
    let !ms = fromIntegral (t1 - t0) / 1_000_000 :: Double
    traceWith tr (SpanEnd traceId sid name ms)
    liftIO $ atomicWriteIORef (spanCurrent ctx) parent

--------------------------------------------------------------------------------
-- OTel-style hex formatting
--------------------------------------------------------------------------------

-- | Render a 'SpanId' as a 16-character lowercase hex string, per OTel
--   convention.
formatSpanIdHex :: SpanId -> Text
formatSpanIdHex (SpanId n) = padHex 16 n

-- | Render a 'TraceId' as a 32-character lowercase hex string, per OTel
--   convention.
formatTraceIdHex :: TraceId -> Text
formatTraceIdHex (TraceId hi lo) = padHex 16 hi <> padHex 16 lo

padHex :: Int -> Word64 -> Text
padHex width n =
  let raw = showHex n ""
      pad = replicate (width - length raw) '0'
  in Text.pack (pad <> raw)

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

instance LogFormatting SpanTrace where
  forMachine _ (SpanBegin tid sid parentSid name) =
    mconcat
      [ "kind"           .= String "SpanBegin"
      , "event"          .= String "begin"
      , "trace_id"       .= formatTraceIdHex tid
      , "span_id"        .= formatSpanIdHex sid
        -- 'null' for a root span; a hex string for a nested one. OTel
        -- SDKs vary on whether to emit "0000000000000000" or omit the
        -- field; 'null' is unambiguous and LogQL handles it cleanly.
      , "parent_span_id" .= fmap formatSpanIdHex parentSid
      , "name"           .= name
      ]
  forMachine _ (SpanEnd tid sid name ms) =
    mconcat
      [ "kind"        .= String "SpanEnd"
      , "event"       .= String "end"
      , "trace_id"    .= formatTraceIdHex tid
      , "span_id"     .= formatSpanIdHex sid
      , "name"        .= name
      , "duration_ms" .= ms
      ]

  forHuman (SpanBegin _ sid _ name) =
    "Span begin [" <> formatSpanIdHex sid <> "] " <> name
  forHuman (SpanEnd _ sid name ms) =
    "Span end   [" <> formatSpanIdHex sid <> "] " <> name
      <> " (" <> showT ms <> " ms)"

  -- Only the end carries a measurement. The metric name embeds the span name so
  -- distinct operations are distinguishable; keep the set of names SMALL to
  -- avoid Prometheus/timeseries cardinality blow-up.
  asMetrics (SpanBegin{})         = []
  asMetrics (SpanEnd _ _ name ms) = [DoubleM ("spanDurationMs." <> name) ms]

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
    "Start of a correlated span. Carries the shared trace_id, the span's \
    \own span_id, and the parent_span_id (null at the root of a trace)."
  documentFor (Namespace _ ["Span", "End"]) = Just
    "End of a correlated span. Carries the same trace_id and span_id as \
    \Span.Begin plus the client-side measured duration in milliseconds."
  documentFor _ = Nothing

  metricsDocFor (Namespace _ ["Span", "End"]) =
    [("spanDurationMs", "Client-side measured span duration, in milliseconds")]
  metricsDocFor _ = []

  allNamespaces =
    [ Namespace [] ["Span", "Begin"]
    , Namespace [] ["Span", "End"]
    ]
