# Writing test cases for timeseries alarm rules

Status: guide (companion to `alarm-system-concept.md`)

This document describes how to write test cases for alarms driven by
timeseries rules (Phase 2 of the alarm concept), using the pieces that exist
today: the timeseries store with its `insert`/`execute` API, the alarm
registry, and the test patterns already used in `cardano-tracer-test`.

## The data entry point: `insert`

Test data is added through the in-process API of `cardano-timeseries-io`
(`Cardano.Timeseries.Component`):

```haskell
insert  :: TimeseriesHandle -> SeriesIdentifier -> Timestamp -> [(MetricIdentifier, Double)] -> IO ()
execute :: TimeseriesHandle -> Timestamp -> Text -> IO (Either ExecutionError Value)
```

- A series is identified by labels, e.g. `Set.fromList [("node_name", "node-1")]` --
  the same shape production uses when node metrics arrive
  (`Cardano.Tracer.Acceptors.Utils.store`).
- Timestamps are milliseconds and are **supplied by the caller**. This is what
  makes tests deterministic: insert samples at chosen times and evaluate the
  query at a chosen `at`, instead of racing the real clock.

There is deliberately **no HTTP insert endpoint** -- the timeseries server only
serves `query`, `prune`, `config`, and `nodes`. In production, data enters
only as metrics forwarded from nodes. Test strategies have to respect that.

## Level 1 (recommended): in-process tests

Follow the style of `Cardano.Tracer.Test.Alarms.Tests`: no server, no
processes, everything driven directly. This is the right level for testing
rule logic (thresholds, `for` durations, edge-triggering, repeat intervals).

```haskell
propHighLatencyRaisesAlarm :: Property
propHighLatencyRaisesAlarm = once $ ioProperty do
  bundle <- mkTraceBundle (SeverityF (Just Warning))
  handle <- Timeseries.create @(Tree Double) (timeseries bundle) (Just noPruneConfig)
  registry <- newAlarmRegistry (assorted bundle) testAlarmsConfig

  -- 1. Arrange: insert all samples up front (queries only look backward)
  let t0 = 1_700_000_000_000                       -- fixed ms timestamp
      series = Set.fromList [("node_name", "node-1")]
  for_ [0 .. 10] \i ->
    Timeseries.insert handle series (t0 + i * 30_000) [("ping_latency_ms", 640)]

  -- 2. Act: evaluate all configured rules at fixed instants, one round
  --    per simulated 30 s (the state machine needs several rounds to
  --    cross the rule's `for` duration)
  for_ [1 .. 10] \k ->
    evaluateTimeseriesRules registry handle
      (posixSecondsToUTCTime (fromIntegral (t0 `div` 1000 + 30 * k)))

  -- 3. Assert: exactly one alarm in the store
  events <- readHistoryFiltered registry Nothing 10 emptyAlarmFilter
  pure (length events === 1)
```

`evaluateTimeseriesRules :: AlarmRegistry -> TimeseriesHandle -> UTCTime -> IO ()`
(and its single-rule sibling `evaluateTimeseriesRule`) in
`Cardano.Tracer.Handlers.Alarms.Registry` **take the evaluation time
explicitly** (like `pruneOnce` and `traceRuleRequest` already do); only the
production `evaluatorLoop` supplies `getCurrentTime`. This is what makes the
rounds above deterministic.

The same pattern also works for testing query semantics alone: insert known
samples, call `execute handle at query`, and assert on the returned `Value`
(thresholds crossing true/false, missing data, window boundaries).

Determinism pitfalls, learned the hard way (all verified against
`cardano-timeseries-io`):

- **Create the store with pruning disabled** (`pruningPeriodMillis =
  Nothing`). The pruner compares against the real wall clock and silently
  deletes fixed historical test timestamps.
- **Instant lookups have a 300 s staleness bound** — a rule evaluated at `t`
  sees a series only if it has a sample in `(t - 300 s, t]`, so test data
  must keep sampling at least that often across the whole horizon.
- **Keep timestamps large.** The Tree store's staleness-window arithmetic is
  on `Word64` and underflows for query times below 300 000 ms.
- **`rate` has no value on a single-point series** (hard error, traced as an
  evaluation failure). Start evaluation rounds only after the range window
  contains at least two populated grid points.
- **A bare metric is a function of time**: instant threshold rules must
  apply it, `m now > 200`; range forms `m[now - 5m; now]` take the metric
  unapplied.
- **Comparisons on instant vectors are filters**, not boolean vectors: the
  result keeps the surviving series with their original values, and the
  evaluator treats series *presence* as "condition holds" (exactly PromQL's
  alert semantics).

## Example test cases

`Cardano.Tracer.Test.Alarms.TimeseriesTests` instantiates this pattern with
three rules ported from the Grafana catalogue
(`grafana-alerts-as-timeseries-rules.md`), each driven once with data that
must not raise an alarm and once with data that must raise exactly one — six
cases total. Samples arrive every 30 s from `t0`, one evaluation round per
30 simulated seconds starting at round 1, and the history is read back
through `readHistoryFiltered` for inspection.

| Rule (query construct) | Quiet case — why no alarm | Alarm case — publish round |
| --- | --- | --- |
| `mempool-high` — `cardano_node_metrics_txsInMempool_int now > 200`, for 600 s | mempool hovers at 120–180: expression never true | constant 250: true from round 1, `for` satisfied at round 21; extra rounds prove the edge publishes once |
| `blockheight-unchanged` — `rate (cardano_node_metrics_blockNum_int[now - 5m; now]) == 0`, for 120 s | chain grows by 1 block per sample: rate > 0, series filtered out every round | constant block height: rate exactly 0, publishes at round 5 with severity `critical` |
| `high-ping-latency` — `avg_over_time (…ping_latency…[now - 5m; now]) > 500`, for 3600 s | 10 min spike at 600 ms, then recovery to 100 ms: expression true far shorter than `for`, pending state resets | constant 800 ms for 62 min: publishes at round 121 |

The three quiet cases deliberately cover the three distinct ways a rule stays
silent: the expression is never true (mempool), the series is filtered out of
the result (blockheight), and the expression is true for less than the `for`
duration before recovering (ping latency). The alarm cases assert exactly one
event and inspect its `ruleId`, `severity`, `source` (`timeseries`), the
`node_name` label carried over from the series key, and the
per-series `sourceEventId` prefix (`ts:<ruleId>:node_name=node-1:`).

## Level 2: end-to-end with a launched `cardano-tracer`

One smoke test should cover the full wiring: config parsing, store, evaluator
thread, alarm history endpoint. The pattern is the one `Test.Logs` uses:
start the tracer and a forwarder in-process inside a tasty test, then assert
over HTTP.

Configuration for the test (YAML):

```yaml
networkMagic: 42
network:
  acceptAt: "tracer.sock"
logging:
  - logRoot: "logs"
    logMode: FileMode
    logFormat: ForMachine
hasTimeseries:
  epHost: "127.0.0.1"
  epPort: 3300
alarms:
  endpoint:
    epHost: "127.0.0.1"
    epPort: 3210
  allowInsecure: true            # test only -- no TLS certificate
  authentication:
    producers: []
    readers:
      - name: test-reader
        tokenFile: "reader.token" # written by the test setup
        allowHistory: true
  timeseriesRules:
    - ruleId: high-latency
      summary: "latency above threshold"
      severity: warning
      query: "avg_over_time (ping_latency_ms[now - 5m; now]) > 500"
      evaluateEvery: 1            # seconds; keep the test fast
  consumers: []
```

Feeding data -- two workable options:

1. **Reuse the test forwarder** (`Cardano.Tracer.Test.Forwarder`, also
   available as the `demo-forwarder` executable). It forwards EKG metrics over
   `trace-forward` exactly like a node, and the acceptor inserts them into the
   timeseries store with the `node_name` label. Extend it (or configure it) to
   register a gauge with the metric name the rule queries. Caveats: metric
   names pass through `sanitiseMetricName`, only numeric values are inserted,
   and timestamps are "now" -- so the test asserts *eventually* (poll with a
   deadline), not at exact instants.

2. **A script cannot insert directly** -- there is no HTTP insert route, and
   adding one for tests is not recommended (it would create a second,
   test-only ingestion path that production never exercises). If a scriptable
   entry point is ever needed, prefer a small Haskell driver that links the
   forwarder library rather than a raw HTTP endpoint.

Asserting:

```bash
# data arrived?
curl 'http://127.0.0.1:3300/timeseries/query?query=ping_latency_ms'
# alarm raised?
curl -H "Authorization: Bearer $(cat reader.token)" \
     'http://127.0.0.1:3210/alarms/v1/events?minSeverity=warning'
```

In a tasty test, do the same with a few lines of `http-client` and poll until
the history response contains the expected `ruleId` or a timeout expires.

## What about preloading test data via the configuration?

A config option like `hasTimeseries: { preloadFile: testdata.json }` -- a file
of `(labels, timestamp, metric, value)` rows loaded into the store right after
`Timeseries.create` in `Run.hs` -- would make end-to-end tests fully
deterministic (fixed timestamps, no forwarder needed) and is cheap to build.
It does not exist yet. If Level-2 tests become flaky because of the
"eventually" polling, this is the first improvement to make; until then,
Level 1 covers determinism and Level 2 covers wiring.

## Summary

| Level | What it tests | Data entry | Determinism |
| --- | --- | --- | --- |
| 1: in-process | query + rule semantics | `Timeseries.insert`, fixed timestamps | full |
| 2: launched tracer | config, threads, HTTP, history | test forwarder (EKG metrics) | poll with deadline |

Put Level-1 properties in `Cardano.Tracer.Test.Alarms.Tests` (they need no
work directory) and the Level-2 smoke test in its own module following
`Test.Logs`' `propRunInLogsStructure` pattern.
