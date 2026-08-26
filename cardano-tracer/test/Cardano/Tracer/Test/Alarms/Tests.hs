{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Real property tests for the alarm subsystem. Each property targets a
-- concrete promise from @cardano-tracer/docs/alarm-system-concept.md@:
-- idempotency, cursor ordering, retention, filter algebra, reader
-- ceiling, and trace-rule flood suppression. Fixture-style tests (a
-- single hand-picked input) are kept only where the behaviour is truly
-- singleton — everything else is generator-driven.
module Cardano.Tracer.Test.Alarms.Tests
  ( tests
  ) where

import           Cardano.Logging (DetailLevel (..), SeverityF (..), SeverityS (..),
                   TraceObject (..))
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Alarms.Registry
import           Cardano.Tracer.Handlers.Alarms.Store
import           Cardano.Tracer.Handlers.Alarms.Types
import           Cardano.Tracer.MetaTrace (TraceBundle (..), mkTraceBundle)

import           Control.Concurrent.Async (forConcurrently)
import           Data.Aeson (decode, encode)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (labels)

--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Alarms"
  [ testGroup "envelope"
      [ testProperty "severity vocabulary lowercase round-trip"      propSeverityRoundTrip
      , testProperty "AlarmEvent JSON round-trip"                    propAlarmEventRoundTrip
      ]
  , testGroup "store"
      [ testProperty "idempotent double-submit returns same eventId" propIdempotentSubmit
      , testProperty "concurrent submit creates exactly one"         propConcurrentSubmit
      , testProperty "prune then resubmit issues a fresh eventId"    propPruneThenResubmit
      , testProperty "cursor is strictly monotonic across inserts"   propCursorMonotonic
      , testProperty "retention maxEvents keeps exactly maxN"        propRetentionMaxEvents
      , testProperty "retention maxAgeSeconds drops older events"    propRetentionMaxAge
      , testProperty "readHistory after=cursor is exclusive"         propHistoryAfterExclusive
      ]
  , testGroup "filter algebra"
      [ testProperty "empty filter accepts every event"              propEmptyFilterAccepts
      , testProperty "scope is submap, not intersection"             propScopeIsSubmap
      , testProperty "minSeverity is <= (inclusive)"                 propMinSeverityInclusive
      , testProperty "filterNarrows is reflexive"                    propFilterNarrowsReflexive
      , testProperty "filterNarrows is transitive"                   propFilterNarrowsTransitive
      , testProperty "broader-than-ceiling filter is rejected"       propBroaderRejected
      ]
  , testGroup "trace rules"
      [ testProperty "rule fires only at or above threshold"         propTraceRuleThreshold
      , testProperty "matches in one window collapse to one alarm"   propTraceRuleSuppression
      , testProperty "matches in a new window raise a fresh alarm"   propTraceRuleNewWindow
      ]
  ]

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genSeverity :: Gen SeverityS
genSeverity = elements [Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency]

genShortText :: Gen Text
genShortText = do
  n <- chooseInt (1, 16)
  Text.pack <$> vectorOf n (elements (['a'..'z'] <> ['0'..'9'] <> "_-"))

genSource :: Gen AlarmSource
genSource = AlarmSource <$> elements ["trace", "hermod-recon", "timeseries", "test-src"]

genRule :: Gen RuleId
genRule = RuleId <$> elements ["rule-a", "rule-b", "rule-c", "rule-d"]

genLabels :: Gen (Map Text Text)
genLabels = do
  n <- chooseInt (0, 3)
  Map.fromList <$> vectorOf n ((,) <$> elements ["team", "site", "region", "shard"]
                                    <*> elements ["ops", "dev", "eu", "us", "0", "1"])

genUTC :: Gen UTCTime
genUTC = do
  s <- chooseInteger (1_700_000_000, 1_900_000_000)
  pure (posixSecondsToUTCTime (fromIntegral s))

genIngress :: Gen IngressRequest
genIngress = IngressRequest
  <$> genShortText
  <*> genUTC
  <*> genRule
  <*> genSeverity
  <*> genShortText
  <*> genLabels
  <*> genLabels
  <*> pure Nothing

genEvent :: Gen AlarmEvent
genEvent = do
  ir  <- genIngress
  src <- genSource
  now <- genUTC
  eid <- genShortText
  pure AlarmEvent
    { schemaVersion = 1
    , eventId       = eid
    , sourceEventId = irSourceEventId ir
    , raisedAt      = irRaisedAt ir
    , receivedAt    = now
    , source        = src
    , ruleId        = irRuleId ir
    , severity      = irSeverity ir
    , summary       = irSummary ir
    , scope         = irScope ir
    , labels        = irLabels ir
    , details       = irDetails ir
    }

genFilter :: Gen AlarmFilter
genFilter = AlarmFilter
  <$> oneof [pure Nothing, Just <$> genSource]
  <*> oneof [pure Nothing, Just <$> genRule]
  <*> oneof [pure Nothing, Just <$> genSeverity]
  <*> genLabels
  <*> genLabels

--------------------------------------------------------------------------------
-- Envelope properties
--------------------------------------------------------------------------------

propSeverityRoundTrip :: Property
propSeverityRoundTrip = forAll genSeverity \sev ->
  parseAlarmSeverityText (alarmSeverityToText sev) === Just sev

propAlarmEventRoundTrip :: Property
propAlarmEventRoundTrip = forAll genEvent \ev ->
  decode (encode ev) === Just ev

--------------------------------------------------------------------------------
-- Store properties
--------------------------------------------------------------------------------

testSource :: AlarmSource
testSource = AlarmSource "hermod-recon"

mkRequestFromParts :: Text -> SeverityS -> IO IngressRequest
mkRequestFromParts sid sev = do
  now <- getCurrentTime
  pure IngressRequest
    { irSourceEventId = sid
    , irRaisedAt      = now
    , irRuleId        = RuleId "test-rule"
    , irSeverity      = sev
    , irSummary       = "test summary"
    , irScope         = Map.empty
    , irLabels        = Map.empty
    , irDetails       = Nothing
    }

propIdempotentSubmit :: Property
propIdempotentSubmit = forAll genShortText \sid -> forAll genSeverity \sev -> ioProperty do
  store <- newAlarmStore (AlarmsRetentionConfig Nothing Nothing)
  req <- mkRequestFromParts sid sev
  now <- getCurrentTime
  (_, ev1, created1) <- insertOrGetExisting store testSource now req
  (_, ev2, created2) <- insertOrGetExisting store testSource now req
  pure $ conjoin
    [ counterexample "first submit must create"      (property created1)
    , counterexample "second submit must not create" (property (not created2))
    , counterexample "eventIds must match"           (eventId ev1 === eventId ev2)
    ]

propConcurrentSubmit :: Property
propConcurrentSubmit = forAll genShortText \sid -> forAll genSeverity \sev ->
  forAll (chooseInt (2, 32)) \fanout -> ioProperty do
    store <- newAlarmStore (AlarmsRetentionConfig Nothing Nothing)
    req <- mkRequestFromParts sid sev
    now <- getCurrentTime
    results <- forConcurrently [1 .. fanout] \_ ->
      insertOrGetExisting store testSource now req
    let createdCount = length (filter (\(_, _, c) -> c) results)
    pure (createdCount === 1)

propPruneThenResubmit :: Property
propPruneThenResubmit = forAll genShortText \sid -> forAll genSeverity \sev -> ioProperty do
  store <- newAlarmStore (AlarmsRetentionConfig (Just 0) Nothing)
  req <- mkRequestFromParts sid sev
  now <- getCurrentTime
  (_, ev1, _) <- insertOrGetExisting store testSource now req
  later <- getCurrentTime
  pruneOnce store later
  (_, ev2, created2) <- insertOrGetExisting store testSource later req
  pure $ conjoin
    [ counterexample "resubmission after prune must create anew" (property created2)
    , counterexample "eventIds must differ"                       (eventId ev1 =/= eventId ev2)
    ]

propCursorMonotonic :: Property
propCursorMonotonic = forAll (chooseInt (2, 16)) \n -> ioProperty do
  store <- newAlarmStore (AlarmsRetentionConfig Nothing Nothing)
  now   <- getCurrentTime
  cursors <- traverse (\i -> do
      req <- mkRequestFromParts (Text.pack ("evt-" <> show i)) Warning
      (cursor, _, _) <- insertOrGetExisting store testSource now req
      pure cursor) [1 .. n]
  pure (cursors === List.sort cursors .&&. length cursors === length (List.nub cursors))

-- | Insert N events; configure retention to keep exactly K < N; after
-- 'pruneOnce', exactly K events remain.
propRetentionMaxEvents :: Property
propRetentionMaxEvents =
  forAll (chooseInt (4, 12)) \n ->
  forAll (chooseInt (1, n - 1)) \k -> ioProperty do
    store <- newAlarmStore (AlarmsRetentionConfig Nothing (Just (fromIntegral k)))
    now   <- getCurrentTime
    _ <- traverse (\i -> do
        req <- mkRequestFromParts (Text.pack ("evt-" <> show i)) Warning
        insertOrGetExisting store testSource now req) [1 .. n]
    pruneOnce store now
    kept <- readHistory store Nothing 100 emptyAlarmFilter
    pure (length kept === k)

-- | An event received long ago is dropped by a maxAgeSeconds=1 prune.
propRetentionMaxAge :: Property
propRetentionMaxAge = forAll genShortText \sid -> ioProperty do
  store <- newAlarmStore (AlarmsRetentionConfig (Just 1) Nothing)
  now   <- getCurrentTime
  let long_ago = addUTCTime (negate 3600) now
  req <- mkRequestFromParts sid Warning
  _ <- insertOrGetExisting store testSource long_ago req
  pruneOnce store now
  kept <- readHistory store Nothing 100 emptyAlarmFilter
  pure (length kept === 0)

-- | 'readHistory's 'after' parameter is exclusive: every returned
--   cursor is strictly greater than 'after'.
propHistoryAfterExclusive :: Property
propHistoryAfterExclusive = forAll (chooseInt (2, 8)) \n -> ioProperty do
  store <- newAlarmStore (AlarmsRetentionConfig Nothing Nothing)
  now   <- getCurrentTime
  _ <- traverse (\i -> do
      req <- mkRequestFromParts (Text.pack ("evt-" <> show i)) Warning
      insertOrGetExisting store testSource now req) [1 .. n]
  all_ <- readHistory store Nothing 100 emptyAlarmFilter
  case all_ of
    [] -> pure (property False)
    ((c, _) : _) -> do
      rest <- readHistory store (Just c) 100 emptyAlarmFilter
      pure $ counterexample ("first cursor " <> show c <> " leaked into after-read")
                            (all ((> c) . fst) rest)

--------------------------------------------------------------------------------
-- Filter algebra properties
--------------------------------------------------------------------------------

-- | The empty filter accepts every event unconditionally.
propEmptyFilterAccepts :: Property
propEmptyFilterAccepts = forAll genEvent \ev ->
  property (matchesFilter emptyAlarmFilter ev)

-- | Concept doc: "requires the event to carry every key/value pair
-- listed here (a submap check), not just an intersection." Test: if
-- the filter demands a key the event does not have, the match must
-- fail even when the event has other scope entries.
propScopeIsSubmap :: Property
propScopeIsSubmap =
  forAll genEvent \ev ->
  forAll genLabels \extraScope ->
    let evWithScope = ev { scope = Map.union (scope ev) extraScope }
        filt = emptyAlarmFilter { afScope = Map.insert "not-in-event" "x" (scope evWithScope) }
    in property (not (matchesFilter filt evWithScope))

-- | 'minSeverity' is inclusive: an event whose severity equals the
-- filter's minimum matches.
propMinSeverityInclusive :: Property
propMinSeverityInclusive = forAll genEvent \ev ->
  let filt = emptyAlarmFilter { afMinSeverity = Just (severity ev) }
  in property (matchesFilter filt ev)

-- | Any filter narrows itself. Reflexivity is the base case of the
--   ceiling algebra: without it a reader cannot even keep its own
--   credential's filter.
propFilterNarrowsReflexive :: Property
propFilterNarrowsReflexive = forAll genFilter \f ->
  property (filterNarrows f f)

-- | If 'b' narrows 'a' and 'c' narrows 'b', then 'c' narrows 'a'.
--   Guarantees the ceiling relation composes across chained checks.
propFilterNarrowsTransitive :: Property
propFilterNarrowsTransitive =
  forAll genFilter \a ->
  forAll genFilter \b ->
  forAll genFilter \c ->
    filterNarrows a b && filterNarrows b c ==>
      counterexample ("a=" <> show a <> " b=" <> show b <> " c=" <> show c)
                     (filterNarrows a c)

-- | Broadening is rejected: given a ceiling that fixes 'source', a
--   requested filter that drops the source field or picks a different
--   value is not permitted.
propBroaderRejected :: Property
propBroaderRejected =
  let ceiling_    = emptyAlarmFilter { afSource = Just (AlarmSource "trace") }
      dropped     = emptyAlarmFilter -- afSource = Nothing broadens the ceiling
      differing   = emptyAlarmFilter { afSource = Just (AlarmSource "somewhere-else") }
  in conjoin
       [ counterexample "dropping 'source' must be rejected"        (not (filterNarrows ceiling_ dropped))
       , counterexample "differing 'source' value must be rejected" (not (filterNarrows ceiling_ differing))
       ]

--------------------------------------------------------------------------------
-- Trace-rule properties
--------------------------------------------------------------------------------

-- | One trace rule at Error threshold, 60s suppression window.
testAlarmsConfig :: AlarmsConfig
testAlarmsConfig = AlarmsConfig
  { alEndpoint        = Endpoint "127.0.0.1" 0 Nothing
  , alAllowInsecure   = Just True
  , alRetention       = Nothing
  , alLimits          = Nothing
  , alAuthentication  = AlarmsAuthConfig [] []
  , alConsumers       = []
  , alTraceRules      = Just
      [ AlarmsTraceRuleConfig
          { atrRuleId          = "error-traces"
          , atrSummary         = Nothing
          , atrThreshold       = Error
          , atrSuppressForSecs = Just 60
          , atrLabels          = Nothing
          }
      ]
  , alTimeseriesRules = Nothing
  }

newTestRegistry :: IO AlarmRegistry
newTestRegistry = do
  bundle <- mkTraceBundle (SeverityF (Just Warning))
  newAlarmRegistry (assorted bundle) testAlarmsConfig

mkTraceObject :: SeverityS -> UTCTime -> TraceObject
mkTraceObject sev at = TraceObject
  { toHuman     = Nothing
  , toMachine   = "{\"msg\":\"test\"}"
  , toNamespace = ["Test", "Alarm"]
  , toSeverity  = sev
  , toDetails   = DNormal
  , toTimestamp = at
  , toHostname  = "localhost"
  , toThreadId  = "1"
  }

-- | Round a UTCTime down to the nearest 'window'-second boundary.
snapToWindow :: Integer -> UTCTime -> UTCTime
snapToWindow window t =
  let epoch = floor (utcTimeToPOSIXSeconds t) :: Integer
      snap  = (epoch `div` window) * window
  in posixSecondsToUTCTime (fromIntegral snap)

-- | For any severity, the rule fires iff sev >= threshold.
propTraceRuleThreshold :: Property
propTraceRuleThreshold = forAll genSeverity \sev -> ioProperty do
  registry <- newTestRegistry
  now      <- getCurrentTime
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject sev now]
  events <- readHistoryFiltered registry Nothing 10 emptyAlarmFilter
  let expected = if sev >= Error then 1 else 0
  pure (length events === expected)

-- | Two matches whose timestamps land in the same 60s window collapse
-- into one alarm regardless of their severities. Both timestamps are
-- taken relative to a window-aligned boundary so we can guarantee they
-- land in the same window index.
propTraceRuleSuppression :: Property
propTraceRuleSuppression = ioProperty $ do
  registry <- newTestRegistry
  now      <- getCurrentTime
  let boundary = snapToWindow 60 now
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject Error boundary]
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject Critical (addUTCTime 30 boundary)]
  events <- readHistoryFiltered registry Nothing 10 emptyAlarmFilter
  pure (length events === 1)

-- | A match one full window after another raises a fresh alarm. The
-- second timestamp is +60s from a window boundary, guaranteed to land
-- in the next window index.
propTraceRuleNewWindow :: Property
propTraceRuleNewWindow = ioProperty $ do
  registry <- newTestRegistry
  now      <- getCurrentTime
  let boundary = snapToWindow 60 now
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject Error boundary]
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject Error (addUTCTime 60 boundary)]
  events <- readHistoryFiltered registry Nothing 10 emptyAlarmFilter
  pure (length events === 2)
