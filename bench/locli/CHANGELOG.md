# Revision history for locli

## 2.3 -- Jan 2026

* Added support for `typst` reports to the `compare` CLI command: They're now automatically created alongside Org mode reports. Includes `*.ede` templates to generate them; `typst >= 0.14` required for their compilation to PDF.
* Change some report fields' unit and metric descriptions to be more concise.
* Adjust the selection of charts plotted for a report, excluding some less meaningful ones.
* Fuse Welford online mean/variance with merging t-digest into single-pass `CdfAccum` — O(δ) ≈ 3 KB space regardless of input size, eliminating the O(n) memory footprint that caused 16–64 GB usage on large production workloads. Quantiles are approximate (sub-1% rank error at δ=100); mean, stddev, and range remain exact.
* Add `CdfPhase` type parameter to `MachPerf` for single-pass multi-metric fold.
* Add criterion micro-benchmarks (`bench-locli-cdf`) and integration benchmarks (`bench-locli-e2e`).

### Running benchmarks

All commands assume you are inside a nix development shell and in the
repository root:

```bash
nix develop .#default

# List all benchmark names
cabal bench bench-locli-cdf --benchmark-options="--list"

# Full CDF benchmark suite with HTML report (~3 min)
cabal bench bench-locli-cdf \
  --benchmark-options="--output bench-cdf-report.html +RTS -T -RTS"

# Quick smoke test (size 1000 only)
cabal bench bench-locli-cdf \
  --benchmark-options="--match prefix cdf/stdCentiles/1000/ +RTS -T -RTS"

# JSON output for scripted comparison
cabal bench bench-locli-cdf \
  --benchmark-options="--json bench-cdf.json +RTS -T -RTS"

# GC/residency stats (check memory behaviour)
cabal bench bench-locli-cdf \
  --benchmark-options="--match prefix cdf/stdCentiles/100000/ +RTS -T -s -RTS" \
  2>&1 | tee bench-gc-stats.txt

# End-to-end benchmark (requires run data under ./run/current/analysis/)
cabal bench bench-locli-e2e \
  --benchmark-options="--output bench-e2e-report.html +RTS -T -RTS"
```

The `bench-locli-cdf` suite compares the old sort-and-index CDF implementation
(embedded in the benchmark as `cdfOld`) against the new `cdf` at sizes 100, 1K,
10K, and 100K, on both pre-sorted and random input.  It also benchmarks the
streaming `addCdfSample` accumulator and `finaliseCdfAccum` in isolation.

The `bench-locli-e2e` suite measures JSON deserialisation of `ClusterPerf` and
`SummaryOne` from real CI benchmark run data.  It skips gracefully if run data
is absent.

## 2.2 -- May 2025

* New log object `LOLedgerMetrics` (and corresponding DB table) for ledger metrics traces, retaining full backwards compatibility to the former `TraceStartLeadershipCheckPlus`.

## 2.1 -- Feb 2025

* New CLI command `dump-tracefreqs` to write out per-host trace frequencies JSON files
* Introduce `Reducer` type class for `locli-quick` based quick query evaluation
* Sample `Reducer` instance implmentations: data types `TxsInMempool`, `Silence` and `ResourceMeasure`
* Add plotting capabilities (experimental) via a heavily modded fork of package `easyplot-1.0` (module `EasyPlot.hs` and its license)

## 2.0 -- Dec 2024

* New database (DB) persistence backend for log objects using serverless SQLite DBs
* Refactor current file persistence backend into its own module
* New CLI commands `prepare-db` and `unlog-db` to create and read from DB persistence backend respectively
* New sum type `LogObjectSource` to represent input from different backends (file or DB)
* Tweak GC to mitigate high RAM requirements (for perf cluster analyses only)
* New executable `locli-quick` which aims to be a development testbed for (upcoming) DB-backed quick queries

## 1.36 -- Nov 2024

* Add `CHANGELOG.md` for `locli`
* Discern Plutus RIPEMD-160 workload in reports
* Remove unused build-depends
* Remove redundant fields from summary report: Perf analysis start/stop spread, Log text lines per host (NB. incompatible for comparison with `summary.org` files created with prior versions)
* Remove unused CLI commands `list-logobject-keys-legacy` and `list-logobject-keys`
* Remove unused `HostLogs` SHA256 checksums
* Disable missing trace detection (temporarily), as raw data isn't properly evaluated to that end at the moment
