# Revision history for locli

## 1.36 -- Nov 2024

* Add `CHANGELOG.md` for `locli`
* Discern Plutus RIPEMD-160 workload in reports
* Remove unused build-depends
* Remove redundant fields from summary report: Perf analysis start/stop spread, Log text lines per host (NB. incompatible for comparison with `summary.org` files created with prior versions)
* Remove unused CLI commands `list-logobject-keys-legacy` and `list-logobject-keys`
* Remove unused `HostLogs` SHA256 checksums
* Disable missing trace detection (temporarily), as raw data isn't properly evaluated to that end at the moment
