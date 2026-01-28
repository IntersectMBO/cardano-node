# Revision history for locli

## 2.3 -- Jan 2026

* Added support for `typst` reports to the `compare` CLI command: They're now automatically created alongside Org mode reports. Includes `*.ede` templates to generate them; `typst >= 0.14` required for their compilation to PDF.
* Change some report fields' unit and metric descriptions to be more concise.
* Adjust the selection of charts plotted for a report, excluding some less meaningful ones.

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
