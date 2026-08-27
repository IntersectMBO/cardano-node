{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Hedgehog properties for "Cardano.Node.Tracing.Span". The test surface
-- is kept to the pure API — 'forMachine', 'asMetrics', 'namespaceFor',
-- 'severityFor', hex formatting, plus 'withSpan' round-trip and nesting
-- observed via 'readCurrentSpan'. No capturing tracer is needed:
-- everything nesting-related is checked either by direct value
-- construction (for the JSON shape) or by asking the context which span
-- is active (for the runtime bracket).
module Test.Cardano.Node.Tracing.Span
  ( tests
  ) where

import           Cardano.Logging (DetailLevel (..), LogFormatting (..), MetaTrace (..),
                   Metric (..), Namespace (..), SeverityS (..))
import           Cardano.Node.Tracing.Span

import           Control.Exception.Safe (SomeException, try)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Hedgehog (Property, discover, (===))
import qualified Hedgehog


-- | Two fresh span ids should never collide within one run.
prop_newSpanId_distinct :: Property
prop_newSpanId_distinct = Hedgehog.property $ do
  ids <- liftIO $ sequence [newSpanId | _ <- [1 .. (128 :: Int)]]
  length (Set.fromList ids) === length ids

-- | And neither should trace ids — the whole point of a 128-bit trace id
-- is that concurrent operations can't collide.
prop_newTraceId_distinct :: Property
prop_newTraceId_distinct = Hedgehog.property $ do
  ids <- liftIO $ sequence [newTraceId | _ <- [1 .. (128 :: Int)]]
  length (Set.fromList ids) === length ids

-- | 'SpanBegin' at the root of a trace carries the six OTel fields
-- Loki/Tempo dashboards read; parent_span_id specifically is 'Null' so
-- consumers can filter for root spans.
prop_forMachine_begin_root_has_expected_fields :: Property
prop_forMachine_begin_root_has_expected_fields = Hedgehog.property $ do
  let obj = forMachine DNormal (SpanBegin (TraceId 1 2) (SpanId 3) Nothing "root")
  fieldsPresent obj ["kind", "event", "trace_id", "span_id", "parent_span_id", "name"]
  KeyMap.lookup (Key.fromString "parent_span_id") obj === Just Aeson.Null

-- | A nested 'SpanBegin' emits the parent's id as a 16-char hex string.
prop_forMachine_begin_child_has_parent :: Property
prop_forMachine_begin_child_has_parent = Hedgehog.property $ do
  let obj = forMachine DNormal
              (SpanBegin (TraceId 1 2) (SpanId 3) (Just (SpanId 4)) "child")
  KeyMap.lookup (Key.fromString "parent_span_id") obj
    === Just (Aeson.String "0000000000000004")

-- | 'SpanEnd' carries the same identity fields plus the measured
-- duration.
prop_forMachine_end_has_expected_fields :: Property
prop_forMachine_end_has_expected_fields = Hedgehog.property $ do
  let obj = forMachine DNormal (SpanEnd (TraceId 1 2) (SpanId 3) "replayLedger" 12.5)
  fieldsPresent obj ["kind", "event", "trace_id", "span_id", "name", "duration_ms"]

-- | Hex widths match OTel conventions exactly: 16 for span, 32 for trace.
prop_span_id_hex_width :: Property
prop_span_id_hex_width = Hedgehog.property $
  Text.length (formatSpanIdHex (SpanId 0xdeadbeef)) === 16

prop_trace_id_hex_width :: Property
prop_trace_id_hex_width = Hedgehog.property $
  Text.length (formatTraceIdHex (TraceId 0xaa 0xbb)) === 32

-- | 'asMetrics' emits nothing on begin (metrics live only on the end,
-- carrying the measured duration), and emits exactly one gauge on end
-- whose name embeds the span name so distinct operations are visible in
-- Prometheus/EKG without extra state.
prop_asMetrics_begin_empty :: Property
prop_asMetrics_begin_empty = Hedgehog.property $
  asMetrics (SpanBegin (TraceId 0 0) (SpanId 1) Nothing "flushWAL") === ([] :: [Metric])

prop_asMetrics_end_emits_duration :: Property
prop_asMetrics_end_emits_duration = Hedgehog.property $
  asMetrics (SpanEnd (TraceId 0 0) (SpanId 1) "flushWAL" 42.0)
    === [DoubleM "spanDurationMs.flushWAL" 42.0]

-- | Namespace / severity metadata drives the trace-dispatcher's routing,
-- so a regression that renames or drops a namespace is a break in the
-- Loki/Prometheus contract, not an internal detail.
prop_namespaceFor_begin :: Property
prop_namespaceFor_begin = Hedgehog.property $
  namespaceFor (SpanBegin (TraceId 0 0) (SpanId 1) Nothing "x")
    === (Namespace [] ["Span", "Begin"] :: Namespace SpanTrace)

prop_namespaceFor_end :: Property
prop_namespaceFor_end = Hedgehog.property $
  namespaceFor (SpanEnd (TraceId 0 0) (SpanId 1) "x" 0)
    === (Namespace [] ["Span", "End"] :: Namespace SpanTrace)

prop_severity_is_info :: Property
prop_severity_is_info = Hedgehog.property $ do
  severityFor (Namespace [] ["Span", "Begin"] :: Namespace SpanTrace) Nothing === Just Info
  severityFor (Namespace [] ["Span", "End"]   :: Namespace SpanTrace) Nothing === Just Info

prop_allNamespaces_covers_both :: Property
prop_allNamespaces_covers_both = Hedgehog.property $ do
  let nss = allNamespaces :: [Namespace SpanTrace]
  Hedgehog.assert (Namespace [] ["Span", "Begin"] `elem` nss)
  Hedgehog.assert (Namespace [] ["Span", "End"]   `elem` nss)

-- | 'withSpan' returns the action's value on success. The null tracer
-- ('mempty') exercises the bracket without asserting anything about
-- emissions, which are the underlying framework's responsibility.
prop_withSpan_returns_result :: Property
prop_withSpan_returns_result = Hedgehog.property $ do
  ctx <- liftIO newSpanContext
  r   <- liftIO $ withSpan mempty ctx "test" (pure (42 :: Int))
  r === 42

-- | 'withSpan' rethrows exceptions from the wrapped action. The
-- 'finally'-based end emission is exercised by GHC's exception
-- semantics; we only need to check that the exception surfaces rather
-- than being swallowed by the bracket.
prop_withSpan_rethrows :: Property
prop_withSpan_rethrows = Hedgehog.property $ do
  ctx    <- liftIO newSpanContext
  result <- liftIO $ try @IO @SomeException $
    withSpan mempty ctx "test" (error "boom" :: IO ())
  case result of
    Left _  -> Hedgehog.success
    Right _ -> Hedgehog.footnote "exception was swallowed" >> Hedgehog.failure

-- | A brand-new context has no active span.
prop_context_starts_empty :: Property
prop_context_starts_empty = Hedgehog.property $ do
  ctx <- liftIO newSpanContext
  cur <- liftIO $ readCurrentSpan ctx
  cur === Nothing

-- | Inside a nested 'withSpan', the inner span sees the same 'TraceId'
-- as the outer one — that is the whole point of trace_id.
prop_nested_shares_trace_id :: Property
prop_nested_shares_trace_id = Hedgehog.property $ do
  ctx <- liftIO newSpanContext
  (outerTid, innerTid) <- liftIO $ withSpan mempty ctx "outer" $ do
    Just (oTid, _) <- readCurrentSpan ctx
    iTid <- withSpan mempty ctx "inner" $ do
      Just (t, _) <- readCurrentSpan ctx
      pure t
    pure (oTid, iTid)
  outerTid === innerTid

-- | The inner and outer spans are distinct span ids — nesting doesn't
-- reuse the parent's id.
prop_nested_has_own_span_id :: Property
prop_nested_has_own_span_id = Hedgehog.property $ do
  ctx <- liftIO newSpanContext
  (outerSid, innerSid) <- liftIO $ withSpan mempty ctx "outer" $ do
    Just (_, oSid) <- readCurrentSpan ctx
    iSid <- withSpan mempty ctx "inner" $ do
      Just (_, s) <- readCurrentSpan ctx
      pure s
    pure (oSid, iSid)
  Hedgehog.assert (outerSid /= innerSid)

-- | After a nested span ends, the context is restored to the outer
-- span. Without this the second sibling of a nested span would end up
-- attached to the wrong parent.
prop_current_restored_after_inner :: Property
prop_current_restored_after_inner = Hedgehog.property $ do
  ctx <- liftIO newSpanContext
  (before, after) <- liftIO $ withSpan mempty ctx "outer" $ do
    b <- readCurrentSpan ctx
    withSpan mempty ctx "inner" (pure ())
    a <- readCurrentSpan ctx
    pure (b, a)
  before === after

-- | After the outermost 'withSpan' returns, the context is empty
-- again — root-level bracketing is symmetric.
prop_root_restored_after_outermost :: Property
prop_root_restored_after_outermost = Hedgehog.property $ do
  ctx <- liftIO newSpanContext
  liftIO $ withSpan mempty ctx "root" (pure ())
  cur <- liftIO $ readCurrentSpan ctx
  cur === Nothing

-- | Two sequential top-level spans on the same context get different
-- trace ids — each is the root of its own trace.
prop_sequential_roots_have_different_trace_ids :: Property
prop_sequential_roots_have_different_trace_ids = Hedgehog.property $ do
  ctx <- liftIO newSpanContext
  t1 <- liftIO $ withSpan mempty ctx "first"  $ do
    Just (t, _) <- readCurrentSpan ctx
    pure t
  t2 <- liftIO $ withSpan mempty ctx "second" $ do
    Just (t, _) <- readCurrentSpan ctx
    pure t
  Hedgehog.assert (t1 /= t2)


-- | Helper: assert that a 'forMachine' object contains every named key.
fieldsPresent :: KeyMap.KeyMap v -> [String] -> Hedgehog.PropertyT IO ()
fieldsPresent obj keys = do
  let actual  = List.sort (map show (KeyMap.keys obj))
      missing = filter (\k -> not (KeyMap.member (Key.fromString k) obj)) keys
  case missing of
    [] -> Hedgehog.success
    _  -> do
      Hedgehog.footnote ("missing fields: " <> show missing <> ", actual keys: " <> show actual)
      Hedgehog.failure


tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
