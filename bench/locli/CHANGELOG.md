# Revision history for locli

## 2.0 -- Dec 2024

* New database (DB) persistence backend for log objects using serverless SQLite DBs
* New CLI commands `prepare-db` and `unlog-db` to create and read from that persistence layer respectively
* Tweak GC to mitigate high RAM requirements
* New executable `locli-quick` which aims to be a development testbed for (upcoming) DB-backed quick queries.

## 1.36 -- Nov 2024

* Add `CHANGELOG.md` for `locli`
* Discern Plutus RIPEMD-160 workload in reports
* Remove unused build-depends
* Remove redundant fields from summary report: Perf analysis start/stop spread, Log text lines per host (NB. incompatible for comparison with `summary.org` files created with prior versions)
* Remove unused CLI commands `list-logobject-keys-legacy` and `list-logobject-keys`
* Remove unused `HostLogs` SHA256 checksums
* Disable missing trace detection (temporarily), as raw data isn't properly evaluated to that end at the moment
