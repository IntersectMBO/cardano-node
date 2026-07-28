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
  handle <- Timeseries.create @(Tree _) (timeseries bundle) Nothing
  registry <- newAlarmRegistry (assorted bundle) testAlarmsConfig

  -- 1. Arrange: five samples, 30 s apart, all above the threshold
  let t0 = 1_700_000_000_000                       -- fixed ms timestamp
      series = Set.fromList [("node_name", "node-1")]
  for_ [0 .. 4] \i ->
    Timeseries.insert handle series (t0 + i * 30_000) [("ping_latency_ms", 640)]

  -- 2. Act: evaluate the rule once, at a fixed time
  --    (evaluateTimeseriesRules is the Phase-2 evaluator entry point;
  --     design it to take the evaluation timestamp as an argument!)
  evaluateTimeseriesRules registry handle (t0 + 5 * 30_000)

  -- 3. Assert: exactly one alarm in the store
  events <- readHistoryFiltered registry Nothing 10 emptyAlarmFilter
  pure (length events === 1)
```

Design rule for the evaluator that follows from this: **pass the evaluation
time in explicitly** (like `pruneOnce` and `traceRuleRequest` already do).
Anything that calls `getCurrentTime` internally cannot be tested
deterministically.

Until the evaluator exists, the same pattern already works for testing query
semantics alone: insert known samples, call `execute handle at query`, and
assert on the returned `Value` (thresholds crossing true/false, missing data,
window boundaries).

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
  timeseriesRules:                # Phase 2, not yet implemented
    - ruleId: high-latency
      summary: "latency above threshold"
      severity: warning
      query: "avg_over_time (ping_latency_ms[now - 5m; now]) > 500"
      evaluateEvery: 1s           # keep the test fast
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
