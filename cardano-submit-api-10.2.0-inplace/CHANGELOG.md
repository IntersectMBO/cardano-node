# Changelog for Cardano-submit-api

## vNext

## 10.2 -- Jan 2026

* Replace the older tracing & metric system — `iohk-monitoring` with `trace-dispatcher`
  * Change prometheus metric type from `gauge` to `counter`
  * Use slightly different prometheus suffix for counters: `counter` instead of `count`

## 10.0 -- Oct 2024

* Bump for Node 10.0

## 3.2.0 -- Nov 2023

** Using `cardano-api-8.33` and `cardano-cli-8.15`

## 8.5.0 -- Oct 2023

* Using `cardano-api-8.25` and `cardano-cli-8.11`
* Add Conway Tx support

## 8.4.0 -- Sep 2023

* Updated dependencies to `cardano-api-8.20`, `ouroboros-network-0.9.1`.

## 8.0.0 -- May 2023

- [Add tx_submit_fail_count metric](https://github.com/intersectmbo/cardano-node/pull/4566)
- [Configurable metrics port in submit-api](https://github.com/intersectmbo/cardano-node/pull/4281)

## 1.35.3 -- July 2022

None

## 1.35.2 -- July 2022 (not released)

None

## 1.35.1 -- July 2022 (not released)

None

## 1.35.0 -- June 2022
- Babbage transactions for submit-api (#3979)
