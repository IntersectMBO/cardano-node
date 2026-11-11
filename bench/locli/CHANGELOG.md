# Revision history for locli

## 3.0 -- Nov 2026

* This version fully drops support for parsing legacy tracing / `iohk-monitoring-framework` generated log objects.
* It also drops capability of parsing log object formats from the development stages of new tracing / `hermod-tracing`.
* Any dependency on the `"kind":` field of log objects is removed, as this field is scheduled for deprecation in `hermod-tracing`.
* Ledger snapshot events are now persisted with their enclosed time (duration).
* Fixed double-counting ledger snapshot events in the machine timeline.
* Fixed namespace drift for several traces.
* Fixed ChainSync server header announcements being recorded twice: the falling edge of `ChainSync.ServerHeader.Update` comes without a `risingEdge` field, not with `false`. Block propagation metrics were unaffected (they use the first announcement).
* Fixed a Word64 underflow in the per-slot rejected transactions (timeline column `rejd`): the node can report a negative `rejected` count, which is now clamped to 0 before conversion.
* `Data.CDF`: `cdfAverageVal` of a CDF of CDFs is the average over all its inputs' averages; `cdf` is total on empty input; removed the unused `collapseCDF`. Summary values are rounded to 3 decimals.
* Re-rendering a single run from its analysis results (`read-summaries`, no chain rebuilt) keeps the filters, block domain and analysis time of its summary in the table headers.
* Summary table: one row per manifest package (`<version> @ <commit>`), shorter labels ("LO": log object, explained in the Manifest section); the Manifest section and its table share one page.
* Test suite: SQLite round-trip of a real log fixture, reading older analysis results, metric definitions and computation, report rendering, Typst formatting, and every interpreted namespace backed by a real log line.
* Typst replaces Org mode for all report output; Org mode support is removed entirely (no more Emacs / LaTeX dependency for PDFs).
  - The `render-*` commands' `--typst` output: standalone single-table documents (summary, cluster performance, block propagation sections, profiling data), generated from bundled templates.
  - New CLI command `report`: a single-run Typst report, bundling the single-table files next to it.
  - `report` also bundles a variance analysis (`wb a variance`, now its last step): the same structure, with statistics over the runs, and the runs listed in the Manifest.
  - The single-run report includes the Plutus workload calibration as an addendum, like `compare` does, if the run has the generator's budget summary (`plutus-workload-scaling.csv` is written next to the report).
  - `compare` creates its Typst report from `--typst FILE`; the `--ede`, `--template` and `--report` options are gone, as are the dumped template environment and `.ede` files.
  - Removed render formats `--org` and `--org-report`.
  - Data values are escaped in all Typst tables (e.g. `---` no longer renders as an em dash).
  - Comparison tables: the absolute delta (Δ) is formatted like the values it's taken from (no spurious decimals on large counts, no `0.000` for small timings); comparing more runs than a table has rows no longer aborts.
* New report sections, on a page of their own after End-to-end propagation (single-run and comparison reports; CLI: `render-clusterperf --snapshots / --tx-processing`):
  - **Ledger Snapshots:** snapshots taken (a count of 0 is highlighted as a warning), write duration, and RTS live data after major GCs during vs. outside snapshot writes.
  - **Transaction Processing:** transactions processed / accepted / rejected, reject ratio, mempool fill level (per analysed slot), and process / GC / mutator CPU time divided by the transactions processed (in the new unit `ms`). Rejected counts are run totals (processed minus accepted), as the node's per-batch count isn't reliable.
* CPU 85% peaks are attributed to a cause: the epoch transition (at the epoch's first block) and other ones are reported under Resource Usage, those during ledger snapshot writes under Ledger Snapshots. Peak lengths count slots, and a gap left by slot filters ends a peak.
* New summary row **Final UTxO size**, averaged over hosts: constant by benchmark design, so a fractional value indicates a problem. Whole-number summary averages render without decimals.
* `Resources` samples only need CPU time, RSS and thread count; all other fields are optional (e.g. no GC fields from a runtime without GC). Metrics that can't be zero throughout if measured (network and storage I/O, GC) render as not available (`—`) when they are.
* Anomaly control: "Height & slot battles" is split into **Slot battles** (other blocks for a chain block's slot, at any height) and **Height battles** (other blocks at its height, for another slot). Both cover the chain blocks in the analysed slots: slot conditions (e.g. `epoch3+`) apply, block conditions (adoptions, size, unitary chain delta) don't, as contested blocks often fail them.
* Removed the `hash-timeline` command, an early check for blocks without a known forger that only ran with the file storage backend; the chain rebuild still fails on such a block.
* Removed the deprecated file storage backend (the `unlog` command): the SQLite backend (`prepare-db`, `unlog-db`) is the only one. The workbench's `--locli-file` option and `WB_LOCLI_DB` are gone with it.
* Documentation: `doc/locli-internals.typ` (pipeline, stages, distributions, reports, workbench mapping) replaces the outdated yEd flow chart.
* Fixed: a forger's announcement, sending and self-adoption, when missing (e.g. a node without downstream peers), are not available instead of a fixed 0.01 s; the first resource sample no longer counts a node's whole startup as one sample; storage reads that are zero throughout show as 0; the machine timeline shows no block number (`---`) before the first block, instead of 2^64-1.
* Analysis results of an older locli still read: metrics they lack show as `—`, also when compared or combined with newer runs.

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
