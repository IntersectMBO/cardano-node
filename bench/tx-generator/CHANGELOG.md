# ChangeLog

## 2.17 -- Jun 2026

* **New remote submission endpoint** — send transactions to a remote endpoint
  instead of the local socket / Node-to-Node protocols, behind a generic,
  backend-agnostic transport interface.
  * Turn it on with the optional `submissionEndpointType` and
    `submissionEndpointURI` keys, which must be set together, e.g.
    `"submissionEndpointType": "Ogmios", "submissionEndpointURI": "ws://127.0.0.1:1337"`.
  * The only endpoint type currently supported is `Ogmios`: each transaction is
    sent over a WebSocket as a JSON-RPC 2.0 `submitTransaction` call.
  * It reroutes **every** phase, but only tx submission: genesis fund import, UTxO splitting, and benchmarking.
  * It is a **functional transport, not a benchmark**: no TPS pacing, no metrics.
    * So it requires `debugMode: true`. The compiler rejects an endpoint config on its own.
  * **A rejected transaction fails the whole run** (the process exits non-zero):
    * Setup phases stop at the first rejection.
    * The benchmark phase finishes the stream, then fails if anything was rejected.
    * Rejections are traced through the normal pipeline, including the detail the endpoint reports.

* **Fix: clean exit for non-benchmark runs.** Scripts that never start the
  Node-to-Node benchmark machinery now exit cleanly instead of crashing at
  shutdown with "AsyncBenchmarkControl absent".
  * Affects `debugMode: true` runs and low-level `json` scripts with no
    `Benchmark` submit phase.

## 2.16 -- Apr 2026

* Added a `--testnet-config-dir` flag to `tx-generator json_highlevel` that auto-discovers connection settings config (socket path, signing key, node config, target nodes) from a `cardano-testnet` output directory.

## 2.15 -- Mar 2025

* A new cabal flag `withplutuslib` is added, enabling import and re-compilation of Plutus scripts from `plutus-scripts-bench` - default: false; use for dev/test of new benchmarks only.
* Port `ProtocolParameters` type and typeclass instances from `cardano-api` into new module `Cardano.Api.Internal`, removing dependency on the deprecated API type.
* A new executable `calibrate-script` which is a tool aimed at auto-calibrating Plutus benchmarking scripts to best fit scaled execution budgets and provides developer-friendly CSV reports of the process.
* Better document how a Plutus script name was resolved by new type `TxGenPlutusResolvedTo`.
* Bump for Node 10.3

## 2.14.2 -- Oct 2024

* Bump for Node 10

## 2.14.1 -- June 2024

* A new NixSvcOptions field is introduced: `_nix_keepalive`
  and it's propagated down to the `kaClient` that does keepalives.
  This makes keepalive timeouts configurable.
* The fast-solo profile is introduced for quick test runs.
* A `CHANGELOG.md` is created for the tx-generator.
