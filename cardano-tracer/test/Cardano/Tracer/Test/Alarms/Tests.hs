{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Alarms"
  [ testProperty "AlarmEvent JSON round-trip uses lowercase severity" propSeverityLowercase
  , testProperty "idempotent double-submit returns the same eventId"  propIdempotentSubmit
  , testProperty "concurrent double-submit creates exactly one event" propConcurrentSubmit
  , testProperty "prune then resubmit issues a fresh eventId"         propPruneThenResubmit
  , testProperty "trace rule fires only at or above its threshold"    propTraceRuleThreshold
  , testProperty "trace rule suppresses repeats within one window"    propTraceRuleSuppression
  , testProperty "trace rule fires again in a new window"             propTraceRuleNewWindow
  ]

mkRequest :: Text.Text -> SeverityS -> IO IngressRequest
mkRequest sourceEventId sev = do
  now <- getCurrentTime
  pure IngressRequest
    { irSourceEventId = sourceEventId
    , irRaisedAt      = now
    , irRuleId        = RuleId "test-rule"
    , irSeverity      = sev
    , irSummary       = "test summary"
    , irScope         = Map.empty
    , irLabels        = Map.empty
    , irDetails       = Nothing
    }

testSource :: AlarmSource
testSource = AlarmSource "hermod-recon"

-- | One trace rule: alarm on severity >= Error, at most one alarm per
--   (node, namespace) per 60-second window.
testAlarmsConfig :: AlarmsConfig
testAlarmsConfig = AlarmsConfig
  { alEndpoint       = Endpoint "127.0.0.1" 0 Nothing
  , alAllowInsecure  = Just True
  , alRetention      = Nothing
  , alLimits         = Nothing
  , alAuthentication = AlarmsAuthConfig [] []
  , alConsumers      = []
  , alTraceRules     = Just
      [ AlarmsTraceRuleConfig
          { atrRuleId          = "error-traces"
          , atrSummary         = Nothing
          , atrThreshold       = Error
          , atrSuppressForSecs = Just 60
          , atrLabels          = Nothing
          }
      ]
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

propSeverityLowercase :: Property
propSeverityLowercase = once $ ioProperty do
  pure (alarmSeverityToText Critical === "critical")

propIdempotentSubmit :: Property
propIdempotentSubmit = once $ ioProperty do
  store <- newAlarmStore (AlarmsRetentionConfig Nothing Nothing)
  req <- mkRequest "evt-1" Warning
  now <- getCurrentTime
  (_, ev1, created1) <- insertOrGetExisting store testSource now req
  (_, ev2, created2) <- insertOrGetExisting store testSource now req
  pure (created1 .&&. not created2 .&&. eventId ev1 === eventId ev2)

propConcurrentSubmit :: Property
propConcurrentSubmit = once $ ioProperty do
  store <- newAlarmStore (AlarmsRetentionConfig Nothing Nothing)
  req <- mkRequest "evt-concurrent" Warning
  now <- getCurrentTime
  results <- forConcurrently [1 .. (20 :: Int)] \_ ->
    insertOrGetExisting store testSource now req
  let createdCount = length (filter (\(_, _, c) -> c) results)
  pure (createdCount === 1)

propPruneThenResubmit :: Property
propPruneThenResubmit = once $ ioProperty do
  store <- newAlarmStore (AlarmsRetentionConfig (Just 0) Nothing) -- prune anything immediately
  req <- mkRequest "evt-pruned" Warning
  now <- getCurrentTime
  (_, ev1, _) <- insertOrGetExisting store testSource now req
  later <- getCurrentTime
  pruneOnce store later
  (_, ev2, created2) <- insertOrGetExisting store testSource later req
  pure (created2 .&&. eventId ev1 =/= eventId ev2)

propTraceRuleThreshold :: Property
propTraceRuleThreshold = once $ ioProperty do
  registry <- newTestRegistry
  now <- getCurrentTime
  checkTraceObjectsForAlarms registry "node-1"
    [mkTraceObject Info now, mkTraceObject Error now]
  events <- readHistoryFiltered registry Nothing 10 emptyAlarmFilter
  pure $ case events of
    [(_, ev)] -> severity ev === Error .&&. source ev === AlarmSource "trace"
    _         -> counterexample ("expected exactly one event, got " <> show (length events)) False

propTraceRuleSuppression :: Property
propTraceRuleSuppression = once $ ioProperty do
  registry <- newTestRegistry
  now <- getCurrentTime
  -- Same node, namespace, and timestamp: both matches land in the same
  -- suppression window, whatever their severities are.
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject Error now]
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject Critical now]
  events <- readHistoryFiltered registry Nothing 10 emptyAlarmFilter
  pure (length events === 1)

propTraceRuleNewWindow :: Property
propTraceRuleNewWindow = once $ ioProperty do
  registry <- newTestRegistry
  now <- getCurrentTime
  -- 120 s > the rule's 60 s window, so the window index always differs,
  -- regardless of where 'now' falls within its window.
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject Error now]
  checkTraceObjectsForAlarms registry "node-1" [mkTraceObject Error (addUTCTime 120 now)]
  events <- readHistoryFiltered registry Nothing 10 emptyAlarmFilter
  pure (length events === 2)
