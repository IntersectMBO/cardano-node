# Revision history for locli

## 3.0 -- NEXT

* This version fully drops support for parsing legacy tracing / `iohk-monitoring-framework` generated log objects.
* It also drops capability of parsing log object formats from the development stages of new tracing / `hermod-tracing`.
* Any dependency on the `"kind":` field of log objects is removed, as this field is scheduled for deprecation in `hermod-tracing`.
* Ledger snapshot events are now persisted with their enclosed time (duration).
* Fixed double-counting ledger snapshot events in the machine timeline.
* Fixed namespace drift for several traces, keeping backwards compatibility.
* Fixed ChainSync server header announcements being recorded twice: the falling edge of `ChainSync.ServerHeader.Update` comes without a `risingEdge` field, not with `false`. Block propagation metrics were unaffected (they use the first announcement).
* Fixed a Word64 underflow in the per-slot rejected transactions (timeline column `rejd`): the node can report a negative `rejected` count, which is now clamped to 0 before conversion.
* New test: Parse a fixture file log stream and round-trip it through SQLite.
* Typst replaces Org mode for all report output; Org mode support is removed entirely (no more Emacs / LaTeX dependency for PDFs).
  - The `render-*` commands' `--typst` output: standalone single-table documents (summary, cluster performance, block propagation sections, profiling data), generated from bundled templates.
  - New CLI command `report`: a single-run Typst report, bundling the single-table files next to it.
  - The single-run report includes the Plutus workload calibration as an addendum, like `compare` does, if the run has the generator's budget summary (`plutus-workload-scaling.csv` is written next to the report).
  - `compare` creates its Typst report from `--typst FILE`; the `--ede`, `--template` and `--report` options are gone, as are the dumped template environment and `.ede` files.
  - Removed render formats `--org` and `--org-report`.
  - Data values are escaped in all Typst tables (e.g. `---` no longer renders as an em dash).
* New report sections, on a page of their own after End-to-end propagation (single-run and comparison reports; CLI: `render-clusterperf --snapshots / --tx-processing`):
  - **Ledger Snapshots:** snapshots taken (a count of 0 is highlighted as a warning), write duration, and RTS live data after major GCs during vs. outside snapshot writes.
  - **Transaction Processing:** transactions processed / accepted / rejected, reject ratio, and process / GC / mutator CPU time divided by the transactions processed (in the new unit `ms`). Rejected counts are run totals (processed minus accepted), as the node's per-batch count isn't reliable.
* Reading analysis results is lenient towards CDFs missing from `clusterperf.json` (e.g. written by an older locli): they read as empty, and reports show them as not available (`—`), so older runs can still be compared against.

## 2.4 -- Jul 2026

* Create Plutus workload calibration CSV as part of run comparison, and include as addendum in `typst` report.
* Drop `locli-quick` dependency on `cardano-api`.

## 2.3 -- Jan 2026

* Added support for `typst` reports to the `compare` CLI command: They're now automatically created alongside Org mode reports. Includes `*.ede` templates to generate them; `typst >= 0.14` required for their compilation to PDF.
* Change some report fields' unit and metric descriptions to be more concise.
* Adjust the selection of charts plotted for a report, excluding some less meaningful ones.

## 2.2 -- May 2025

* New log object `LOLedgerMetrics` (and corresponding DB table) for ledger metrics traces, retaining full backwards compatibility to the former `TraceStartLeadershipCheckPlus`.

## 2.1 -- Feb 2025

* New CLI command `dump-tracefreqs` to write out per-host trace frequencies JSON files
* Introduce `Reducer` type class for `locli-quick` based quick query evaluation
* Sample `Reducer` instance implementations: data types `TxsInMempool`, `Silence` and `ResourceMeasure`
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
