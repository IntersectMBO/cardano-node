# Revision history for trace-dispatcher

## 2.8.0 -- Jan 2025
* Change dependency `ekg` to `ekg-wai`, replacing `snap-server` based web stack with `warp / wai`.
* Add `initForwardingDelayed` which allows for deferred start of forwarding after initialization, instead of tying both together.

## 2.7.0 -- Sep 2024
* Add `docuResultsToMetricsHelptext` for JSON output of metrics docs; required
  by `cardano-node` command `trace-documentation --output-metric-help`

## 2.6.0

* With a metrics prefix that can be set in the configuration (tcMetricsPrefix)
  Metrics gets a type postfix (_int,_real, _counter)

## 2.5.7

* With a prometheus metric with key label pairs. The value will always be "1"

## 2.5.2 -- Dec 2023

* ForHuman Color, Increased Consistency Checks, and Non-empty Inner Workspace Validation

## 2.5.1 -- Dec 2023

* Rewrite of examples as unit tests

## 2.4.1 -- Nov 2023

* Updated to `ouroboros-network-0.10`

## 2.1.0 -- Sep 2023

* Updated to `ouroboros-network-0.9.1.0`

## 2.0.0 -- May 2023

* First version that diverges from caradno-node versioning scheme

* GHC-9.2 support

* Many undocumented changes

## 1.35.4 -- November 2022

* Undocumented changes

## 1.29.0 -- September 2021

* Initial version.
