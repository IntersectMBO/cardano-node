# Porting the cardano-parts Grafana alerts to timeseries alarm rules

Status: guide (companion to `alarm-system-concept.md`)

The SRE alert catalogue in
[cardano-parts](https://github.com/input-output-hk/cardano-parts/tree/main/templates/cardano-parts-project/flake/opentofu/grafana/alerts)
defines Grafana-managed Prometheus alert rules for Cardano deployments. This
document shows how the `cardano-node` alerts from that catalogue map onto
timeseries rules in the new alarm system (Phase 2 of the concept doc) — what
translates directly, what changes, and what does not carry over.

The comparison is against the four node-related files:
`cardano-node.nix-import`, `cardano-node-forge.nix-import`,
`cardano-node-network.nix-import`, `cardano-node-quality.nix-import`, and
`cardano-node-divergence.nix-import`.

## Rule anatomy: Grafana vs. alarm system

| Grafana / Prometheus rule | Alarm system `timeseriesRules` entry |
| --- | --- |
| `alert` (name) | `ruleId` |
| `expr` (PromQL) | `query` (`cardano-timeseries-io` language) |
| `for` | `for` |
| `labels.severity = "page"` | `severity:` — richer vocabulary; use `critical` for pages, lower levels become possible |
| `annotations.summary/description` | `summary` (static text; no `{{$labels.instance}}` templating) |
| node identity via `instance` label | series key labels (`node_name`) carried into the alarm's labels |
| evaluation interval (Grafana global) | `evaluateEvery` per rule |

The alarm state machine matches Grafana's semantics: edge-triggered
publication after the expression has been true for the `for` duration, no
resolution events, optional `repeatEvery` reminders.

## Query language correspondence

The `cardano-timeseries-io` language covers almost every PromQL construct the
catalogue uses:

| PromQL | timeseries query language |
| --- | --- |
| instant selector `m` | `m now` — a metric is a *function of time* and must be applied |
| `m[5m]` (range) | `m[now - 5m; now]` (the metric stays unapplied inside a range) |
| `m[360m:1m]` (subquery w/ step) | `m[now - 360m; now : 1m]` |
| `rate(...)`, `increase(...)` | `rate (...)`, `increase (...)` — **per millisecond**, not per second, and without counter-reset handling or window extrapolation |
| `avg_over_time`, `sum_over_time` | same names |
| `quantile_over_time(0.95, v)` | `quantile_over_time 0.95 v` |
| `quantile by(environment) (0.2, v)` | `quantile_by ("environment") 0.2 v` |
| `min`/`max`/`avg`/`abs`/`round` | same names |
| `A unless B` | `unless A B` |
| `m{environment="mainnet"}` | `(m now){"environment" = "mainnet"}` (`=` and `!=` only; label keys are quoted) |
| `expr > k` (alert firing) | plain `>`/`==`/… on an instant vector is a **filter**, exactly like PromQL alert expressions: series failing the comparison drop out of the result, and the rule evaluator treats a series' *presence* as "condition true". Scalar comparisons yield real booleans. |
| `and` / `or` (scalar logic) | `&&` / `||` |
| — | extras: `let`/lambdas, `map`, `filter`, `join`, `to_scalar`, `earliest`, `latest`, `metrics` |

Missing relative to PromQL: regex label matchers (`=~`, `!~`), vector-`or` as
union/fallback, and cross-series binary matching (`on() group_right()`).
Workarounds below.

Two further differences that bit during prototyping: `rate` values are per
millisecond, so PromQL thresholds on rates must be divided by 1000 (`> 0.5`
per second becomes `> 0.0005`; comparisons against `0` are unaffected), and
`rate` over a window that holds only a single sample is an evaluation error
(traced, never published as an alarm). Durations in the rule configuration
(`evaluateEvery`, `for`, `repeatEvery`) are plain integer **seconds**, not
Prometheus duration strings.

## The catalogue, translated

Severity suggestions replace the one-size-fits-all `page`. Metric names are
the new-tracing names as forwarded by the node — verify against a live store
with the `metrics` query before deploying (see caveats).

Durations (`evaluateEvery`, `for`) are plain integer seconds. Instant
selectors are applied to `now`; range windows take the metric unapplied.

```yaml
alarms:
  timeseriesRules:
    # --- cardano-node.nix-import -------------------------------------------
    - ruleId: blockheight-unchanged            # cardano_node_blockheight_unchanged
      summary: "Blockheight unchanged for more than 7 minutes"
      severity: critical
      query: "rate (cardano_node_metrics_blockNum_int[now - 5m; now]) == 0"
      evaluateEvery: 30
      for: 120

    - ruleId: mempool-high                     # cardano_node_mempool_high
      summary: "More than 200 transactions in mempool for over 10 minutes"
      severity: warning
      query: "cardano_node_metrics_txsInMempool_int now > 200"
      evaluateEvery: 30
      for: 600

    - ruleId: mempool-soft-timeouts            # cardano_node_mempool_soft_timeouts_detected
      summary: "Mempool soft timeouts detected in the past hour"
      severity: warning
      query: "increase (cardano_node_metrics_txsMempoolTimeoutSoft_counter[now - 1h; now]) > 2"
      evaluateEvery: 60
      for: 60

    - ruleId: mempool-hard-timeouts            # cardano_node_mempool_hard_timeouts_detected
      summary: "Mempool hard timeouts detected in the past hour"
      severity: error
      query: "increase (cardano_node_metrics_txsMempoolTimeoutHard_counter[now - 1h; now]) > 0"
      evaluateEvery: 60
      for: 60

    # Needs prototyping before use: the bare comparison inside the subquery
    # ((m != 0)[...]) does not elaborate -- a metric must be applied to a
    # time, which inside a subquery requires a lambda. Like the divergence
    # family below, verify against the real interpreter first.
    - ruleId: blockheight-metric-missing       # cardano_node_metric_missing
      summary: "Blockheight metric missing for more than 10 minutes"
      severity: error
      query: >-
        unless (sum_over_time ((cardano_node_metrics_blockNum_int != 0)[now - 360m; now : 1m]) < 350)
               cardano_node_metrics_blockNum_int
      evaluateEvery: 60
      for: 60

    # --- cardano-node-forge.nix-import -------------------------------------
    - ruleId: no-blocks-forged-24h             # cardano_node_forge_blocks_missing
      summary: "No blocks forged in the past 24 hours"
      severity: critical
      query: "increase (cardano_node_metrics_blocksForged_int[now - 24h; now]) == 0"
      evaluateEvery: 300
      for: 60

    - ruleId: forged-not-adopted               # cardano_node_forge_not_adopted_error
      summary: "Failed to adopt one or more forged blocks in the past hour"
      severity: error
      query: "increase (cardano_node_metrics_Forge_didnt_adopt_counter[now - 1h; now]) > 0"
      evaluateEvery: 60
      for: 60

    - ruleId: cannot-forge                     # cardano_node_cannot_forge_new_tracing
      summary: "Failed to forge one or more blocks in the past hour"
      severity: error
      query: "increase (cardano_node_metrics_nodeCannotForge_int[now - 1h; now]) > 0"
      evaluateEvery: 60
      for: 60

    - ruleId: slot-leadership-checks-missed    # too_many_slot_leadership_checks_missed
      summary: "Slot leadership checks missed for more than half of slots"
      severity: critical
      # rate is per millisecond: PromQL's 0.5/s threshold becomes 0.0005/ms.
      query: "rate (cardano_node_metrics_slotsMissed_int[now - 5m; now]) > 0.0005"
      evaluateEvery: 30
      for: 120

    # KES: the richer severity vocabulary replaces three identical "page"
    # alerts with an escalation ladder.
    - ruleId: kes-expiry-10-periods            # cardano_node_KES_expiration_metric_10period_notice
      summary: "Less than 10 KES periods remaining"
      severity: warning
      query: "cardano_node_metrics_remainingKESPeriods_int now <= 10"
      evaluateEvery: 300
      for: 300
    - ruleId: kes-expiry-5-periods
      summary: "Less than 5 KES periods remaining"
      severity: error
      query: "cardano_node_metrics_remainingKESPeriods_int now <= 5"
      evaluateEvery: 300
      for: 300
    - ruleId: kes-expiry-1-period
      summary: "KES expires within 1 period"
      severity: critical
      query: "cardano_node_metrics_remainingKESPeriods_int now <= 1"
      evaluateEvery: 300
      for: 300

    # --- cardano-node-network.nix-import ------------------------------------
    # This one is the concept doc's own example rule.
    - ruleId: high-ping-latency                # high_cardano_ping_latency
      summary: "Average node ping latency above 500 ms"
      severity: warning
      query: "avg_over_time (netdata_statsd_cardano_node_ping_latency_ms_gauge_value_average[now - 5m; now]) > 500"
      evaluateEvery: 30
      for: 3600

    - ruleId: block-adoption-delay             # blocks_adoption_delay_too_high
      summary: "95th-percentile block adoption delay above 4.5 s"
      severity: warning
      query: "avg (quantile_over_time 0.95 (cardano_node_metrics_blockfetchclient_blockdelay_real[now - 6h; now])) >= 4.5"
      evaluateEvery: 300
      for: 60

    - ruleId: block-utilization-high           # blocks_utilization_too_high
      summary: "Average block utilization above 95%"
      severity: warning
      query: "100 * avg (avg_over_time (cardano_node_metrics_blockfetchclient_blocksize_int[now - 6h; now]) / 90112) > 95"
      evaluateEvery: 300
      for: 300

    - ruleId: blockfetch-delay-high            # cardano_blockfetchclient_blockdelay_high
      summary: "Less than 90% of blocks arriving within 5 seconds"
      severity: warning
      query: "cardano_node_metrics_blockfetchclient_blockdelay_cdfFive_real now < 0.90"
      evaluateEvery: 60
      for: 600

    - ruleId: blockfetch-delay-critical        # cardano_blockfetchclient_blockdelay_critical
      summary: "Less than 50% of blocks arriving within 5 seconds"
      severity: critical
      query: "cardano_node_metrics_blockfetchclient_blockdelay_cdfFive_real now < 0.50"
      evaluateEvery: 60
      for: 600

    - ruleId: connection-count-high            # cardano_connection_count_high
      summary: "Incoming connection count above 450 (hard limit 512)"
      severity: warning
      query: "cardano_node_metrics_connectionManager_inboundConns_int now > 450"
      evaluateEvery: 60
      for: 600

    # --- cardano-node-quality.nix-import ------------------------------------
    # Fleet-wide rule: only meaningful when the whole fleet forwards to this
    # tracer (see caveats). The regex exclusion {environment!~"preview"} has
    # no equivalent; use != or per-environment rules.
    - ruleId: chain-density-degraded           # chain_quality_degraded
      summary: "More than 20% of nodes below 70% chain density"
      severity: warning
      query: '100 * quantile_by ("environment") 0.2 ((cardano_node_metrics_density_real now){"environment" != "preview"} * 20) < 70'
      evaluateEvery: 60
      for: 300
```

## Alerts that do not translate one-to-one

**Block divergence (`cardano-node-divergence.nix-import`).** The PromQL uses
cross-series matching (`max(m) - on() group_right() m`) to compare each node
against the fleet maximum. The language has no vector matching, but its
functional extras can express the same idea:

```text
let peak = to_scalar (max (cardano_node_metrics_blockNum_int)) in
map (\x -> abs (peak - x) > 6) cardano_node_metrics_blockNum_int
```

combined with the analogous slot-lag condition via `&&`. This is the one
family where the translation needs prototyping against the real interpreter
before committing to a rule.

**Elevated restarts (`cardano_node_elevated_restarts`).** Uses
`time() - nodeStartTime` inside a subquery. `now` and timestamp arithmetic
exist, and subqueries with a step are supported, so this is expressible in
principle — but timestamp-vs-number typing needs verification. A simpler
first version: alarm when `cardano_node_metrics_nodeStartTime_int` changed
within the window, or leave restart detection to the trace severity rules
(the node logs its startup).

**Old/new tracing metric-name pairs.** Half the catalogue exists twice
(`...Forge_forged_int` vs `...blocksForged_int`) joined by PromQL vector
`or`, because Prometheus may scrape either naming scheme. `cardano-tracer`
receives metrics from the node's own forwarder, i.e. the new-tracing names
only — the `_new_tracing` variants are the ones to port, and the `or`
fallback disappears.

## General caveats

- **Scope.** Grafana alerts run against a central Prometheus that scrapes the
  whole fleet. A `cardano-tracer` store contains only the nodes forwarding to
  that tracer instance. Per-node rules (mempool, KES, forging) translate
  directly; fleet-wide statistics (chain density, divergence) are only
  meaningful when the relevant fleet shares one tracer.
- **Labels.** The store's series label is `node_name` (set by the acceptor),
  not Prometheus's `instance`/`environment`. Rules that filter on
  `environment` need that label to exist in the store, or need the filter
  dropped. Alarm routing dimensions come from the rule's `labels`/scope
  config plus the series key, not from annotation templates.
- **Metric names.** Names pass through `sanitiseMetricName` on ingestion.
  Before writing a rule, list what the store actually holds:
  `GET /timeseries/query?query=metrics`.
- **Delivery.** Grafana pages via its contact points; here, delivery is the
  consumer configuration (today `log`; `webhook`/`email` planned). Porting
  the rules is independent of porting the paging integration.
- **Testing.** Each ported rule should get a Level-1 test as described in
  `timeseries-alarm-testing.md`: insert samples that straddle the threshold
  and the `for` window, evaluate at fixed timestamps, assert exactly one
  alarm. Three rules of this catalogue — `mempool-high`,
  `blockheight-unchanged`, and `high-ping-latency` — already have such
  tests (`Cardano.Tracer.Test.Alarms.TimeseriesTests`), one alarming and
  one quiet dataset each; they are the template for porting the rest.

## Status

The Phase-2 timeseries rule evaluator from the concept doc is implemented
(`Cardano.Tracer.Handlers.Alarms.TimeseriesRules`, driven by
`runTimeseriesEvaluator`). The catalogue above is its acceptance suite —
three rules are verified by the example tests; the queries of the remaining
rules follow the same corrected conventions but should each get their
Level-1 test before being relied on. The two families flagged above
(divergence, `blockheight-metric-missing`) still need prototyping against
the interpreter.
