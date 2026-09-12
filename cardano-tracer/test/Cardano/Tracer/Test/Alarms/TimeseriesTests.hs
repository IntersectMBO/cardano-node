{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example test cases for timeseries alarm rules: three rules ported
-- from the Grafana catalogue in
-- @cardano-tracer/docs/grafana-alerts-as-timeseries-rules.md@, each
-- driven once with data that must NOT raise an alarm and once with data
-- that MUST raise exactly one.
--
-- These are Level-1 tests in the sense of
-- @cardano-tracer/docs/timeseries-alarm-testing.md@: no server and no
-- forwarder — samples are inserted at fixed millisecond timestamps and
-- the rules are evaluated at fixed instants through
-- 'evaluateTimeseriesRules', so every case is fully deterministic. The
-- whole production pipeline below the evaluator loop is exercised:
-- query execution against the store, sample decoding, the per-series
-- @for@ state machine, ingress via the alarm registry, and the history
-- read used for inspection.
module Cardano.Tracer.Test.Alarms.TimeseriesTests
  ( tests
  ) where

import           Cardano.Logging (SeverityF (..), SeverityS (..))
import           Cardano.Timeseries.API (Config (..), Tree)
import           Cardano.Timeseries.Component (TimeseriesConfig (..))
import qualified Cardano.Timeseries.Component as Timeseries
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Alarms.Registry
import           Cardano.Tracer.Handlers.Alarms.Types
import           Cardano.Tracer.MetaTrace (TraceBundle (..), mkTraceBundle)

import           Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word (Word64)

import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (labels)

tests :: TestTree
tests = testGroup "Test.Alarms.Timeseries"
  [ testGroup "mempool-high"
      [ testProperty "mempool below threshold raises no alarm"          propMempoolQuiet
      , testProperty "sustained high mempool raises exactly one alarm"  propMempoolAlarm
      ]
  , testGroup "blockheight-unchanged"
      [ testProperty "growing chain raises no alarm"                    propBlockheightQuiet
      , testProperty "stalled blockheight raises exactly one alarm"     propBlockheightAlarm
      ]
  , testGroup "high-ping-latency"
      [ testProperty "latency spike shorter than for raises no alarm"   propPingQuiet
      , testProperty "sustained high latency raises exactly one alarm"  propPingAlarm
      ]
  ]

--------------------------------------------------------------------------------
-- Fixture
--------------------------------------------------------------------------------

-- | Fixed base timestamp (ms since epoch). Must stay far above the
--   store's 300 s staleness window: the Tree store's window arithmetic
--   is on 'Word64' and underflows for query times below it.
t0 :: Word64
t0 = 1_700_000_000_000

-- | Every sample belongs to one node, labelled the way the acceptor
--   labels forwarded metrics ("Cardano.Tracer.Acceptors.Utils").
testSeries :: Set.Set (Text, Text)
testSeries = Set.fromList [("node_name", "node-1")]

-- | Store configuration for tests. The pruner compares against the real
--   wall clock, so the retention window must be large enough that its
--   cutoff (now - retention) stays below the fixed 2023-era sample
--   timestamps — but small enough that the 'Word64' subtraction cannot
--   wrap. 10^12 ms (~32 years) satisfies both for decades. The pruning
--   period must be 'Just': with 'Nothing' the pruner parks forever on an
--   MVar, and once the abandoned test handle becomes garbage the RTS's
--   deadlock detector kills it, rethrowing through 'link' into whatever
--   test runs later.
testTimeseriesConfig :: TimeseriesConfig
testTimeseriesConfig = TimeseriesConfig
  { retentionMillis     = 1_000_000_000_000
  , pruningPeriodMillis = Just (24 * 60 * 60 * 1000)
  , interpCfg           = Config { defaultRangeSamplingRateMillis = 15_000 }
  }

mkAlarmsConfig :: AlarmsTimeseriesRuleConfig -> AlarmsConfig
mkAlarmsConfig rule = AlarmsConfig
  { alEndpoint        = Endpoint "127.0.0.1" 0 Nothing
  , alAllowInsecure   = Just True
  , alRetention       = Nothing
  , alLimits          = Nothing
  , alAuthentication  = AlarmsAuthConfig [] []
  , alConsumers       = []
  , alTraceRules      = Nothing
  , alTimeseriesRules = Just [rule]
  }

-- | Evaluation timestamp of round @k@: 30 simulated seconds per round
--   after 't0'.
roundTime :: Word64 -> UTCTime
roundTime k = posixSecondsToUTCTime (fromIntegral (t0 `div` 1000 + 30 * k))

-- | One sample per 30 s starting at 't0': offsets and values for
--   indices @0 .. count-1@.
samplesEvery30s :: Word64 -> (Word64 -> Double) -> [(Word64, Double)]
samplesEvery30s count valueAt = [ (i * 30_000, valueAt i) | i <- [0 .. count - 1] ]

-- | Build a store and a registry holding one rule, insert all samples
--   of one metric up front (every query looks strictly backward, so
--   later samples are invisible to earlier rounds), evaluate the rule
--   at rounds @k = 1 .. rounds@, and return the alarm history.
--
--   Rounds start at @k = 1@: at @k = 0@ a range window holds a single
--   populated grid point and @rate@ has no defined value there.
runScenario
  :: AlarmsTimeseriesRuleConfig
  -> Text                    -- ^ metric name
  -> [(Word64, Double)]      -- ^ samples: (ms after 't0', value)
  -> Word64                  -- ^ number of 30 s evaluation rounds
  -> IO [(AlarmCursor, AlarmEvent)]
runScenario rule metric samples rounds = do
  bundle   <- mkTraceBundle (SeverityF (Just Warning))
  handle   <- Timeseries.create @(Tree Double) (timeseries bundle) (Just testTimeseriesConfig)
  registry <- newAlarmRegistry (assorted bundle) (mkAlarmsConfig rule)
  for_ samples \(offset, value) ->
    Timeseries.insert handle testSeries (t0 + offset) [(metric, value)]
  for_ [1 .. rounds] \k ->
    evaluateTimeseriesRules registry handle (roundTime k)
  readHistoryFiltered registry Nothing 100 emptyAlarmFilter

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

expectNoAlarm :: [(AlarmCursor, AlarmEvent)] -> Property
expectNoAlarm events =
  counterexample ("unexpected alarms: " <> show (map snd events))
                 (length events === 0)

-- | Exactly one alarm, carrying the rule identity, the severity, the
--   trusted source, the series label, and a per-series source event id.
expectOneAlarm :: AlarmsTimeseriesRuleConfig -> [(AlarmCursor, AlarmEvent)] -> Property
expectOneAlarm rule events = case events of
  [(_, ev)] -> conjoin
    [ counterexample "ruleId"   (ruleId ev   === RuleId (atsRuleId rule))
    , counterexample "severity" (severity ev === atsSeverity rule)
    , counterexample "source"   (source ev   === AlarmSource "timeseries")
    , counterexample "summary"  (Just (summary ev) === atsSummary rule)
    , counterexample "node_name label"
        (Map.lookup "node_name" (labels ev) === Just "node-1")
    , counterexample ("sourceEventId: " <> show (sourceEventId ev))
        (property (("ts:" <> atsRuleId rule <> ":node_name=node-1:")
                     `Text.isPrefixOf` sourceEventId ev))
    ]
  _ -> counterexample ("expected exactly one alarm, got: " <> show (map snd events))
                      (property False)

--------------------------------------------------------------------------------
-- mempool-high: instant threshold
--------------------------------------------------------------------------------

-- | Catalogue rule @cardano_node_mempool_high@. Note the applied @now@:
--   a bare metric is a function of time in the query language.
mempoolRule :: AlarmsTimeseriesRuleConfig
mempoolRule = AlarmsTimeseriesRuleConfig
  { atsRuleId        = "mempool-high"
  , atsSummary       = Just "More than 200 transactions in mempool for over 10 minutes"
  , atsSeverity      = Warning
  , atsQuery         = "cardano_node_metrics_txsInMempool_int now > 200"
  , atsEvaluateEvery = 30
  , atsFor           = Just 600
  , atsRepeatEvery   = Nothing
  , atsLabels        = Nothing
  }

mempoolMetric :: Text
mempoolMetric = "cardano_node_metrics_txsInMempool_int"

-- | The mempool hovers between 120 and 180 transactions — always below
--   the threshold, so the expression is never true.
propMempoolQuiet :: Property
propMempoolQuiet = once $ ioProperty do
  events <- runScenario mempoolRule mempoolMetric
              (samplesEvery30s 25 \i -> 120 + fromIntegral (i `mod` 3) * 30)
              24
  pure (expectNoAlarm events)

-- | The mempool sits at 250 transactions for the whole horizon. The
--   expression turns true at round 1, satisfies @for@ = 600 s at round
--   21, and the three extra rounds prove the edge stays published only
--   once.
propMempoolAlarm :: Property
propMempoolAlarm = once $ ioProperty do
  events <- runScenario mempoolRule mempoolMetric
              (samplesEvery30s 25 (const 250))
              24
  pure (expectOneAlarm mempoolRule events)

--------------------------------------------------------------------------------
-- blockheight-unchanged: rate over a range window
--------------------------------------------------------------------------------

-- | Catalogue rule @cardano_node_blockheight_unchanged@. The range form
--   needs no applied @now@; @== 0@ keeps exactly the series whose block
--   height did not move inside the 5 min window.
blockheightRule :: AlarmsTimeseriesRuleConfig
blockheightRule = AlarmsTimeseriesRuleConfig
  { atsRuleId        = "blockheight-unchanged"
  , atsSummary       = Just "Blockheight unchanged for more than 7 minutes"
  , atsSeverity      = Critical
  , atsQuery         = "rate (cardano_node_metrics_blockNum_int[now - 5m; now]) == 0"
  , atsEvaluateEvery = 30
  , atsFor           = Just 120
  , atsRepeatEvery   = Nothing
  , atsLabels        = Nothing
  }

blockheightMetric :: Text
blockheightMetric = "cardano_node_metrics_blockNum_int"

-- | The chain grows by one block per sample, so the rate over the
--   window is strictly positive and the comparison filters the series
--   out of the result on every round.
propBlockheightQuiet :: Property
propBlockheightQuiet = once $ ioProperty do
  events <- runScenario blockheightRule blockheightMetric
              (samplesEvery30s 17 \i -> 1000 + fromIntegral i)
              16
  pure (expectNoAlarm events)

-- | The block height never moves: the rate is exactly 0 on every round,
--   and after @for@ = 120 s (round 5) exactly one critical alarm is
--   published.
propBlockheightAlarm :: Property
propBlockheightAlarm = once $ ioProperty do
  events <- runScenario blockheightRule blockheightMetric
              (samplesEvery30s 17 (const 1000))
              16
  pure (expectOneAlarm blockheightRule events)

--------------------------------------------------------------------------------
-- high-ping-latency: avg_over_time over a range window
--------------------------------------------------------------------------------

-- | Catalogue rule @high_cardano_ping_latency@ (also the concept doc's
--   own example rule).
pingRule :: AlarmsTimeseriesRuleConfig
pingRule = AlarmsTimeseriesRuleConfig
  { atsRuleId        = "high-ping-latency"
  , atsSummary       = Just "Average node ping latency above 500 ms"
  , atsSeverity      = Warning
  , atsQuery         = "avg_over_time (netdata_statsd_cardano_node_ping_latency_ms_gauge_value_average[now - 5m; now]) > 500"
  , atsEvaluateEvery = 30
  , atsFor           = Just 3600
  , atsRepeatEvery   = Nothing
  , atsLabels        = Nothing
  }

pingMetric :: Text
pingMetric = "netdata_statsd_cardano_node_ping_latency_ms_gauge_value_average"

-- | A 10-minute latency spike (600 ms), then recovery to 100 ms. The
--   expression is true while the spike dominates the trailing 5 min
--   average — far shorter than @for@ = 60 min — then turns false and
--   resets the pending state, so nothing may ever be published.
propPingQuiet :: Property
propPingQuiet = once $ ioProperty do
  events <- runScenario pingRule pingMetric
              (samplesEvery30s 61 \i -> if i < 20 then 600 else 100)
              60
  pure (expectNoAlarm events)

-- | Latency stays at 800 ms for 62 minutes. The expression is true from
--   round 1 on, satisfies @for@ = 3600 s at round 121, and exactly one
--   warning alarm is published.
propPingAlarm :: Property
propPingAlarm = once $ ioProperty do
  events <- runScenario pingRule pingMetric
              (samplesEvery30s 125 (const 800))
              124
  pure (expectOneAlarm pingRule events)
