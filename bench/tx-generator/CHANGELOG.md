# ChangeLog

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
