# ChangeLog

## 3.0 -- Nov 2026

* **Transactions are built with the experimental cardano-api**
  (`Cardano.Api.Experimental`) instead of `createTransactionBody` and
  `signShelleyTransaction`.
  * **Only the Conway and Dijkstra eras are supported**, as by the
    experimental API; other eras are rejected.

* **Sub-transactions (Dijkstra onwards).** A new `NestedNtoM` generator: an
  `NtoM` transaction that also carries sub-transactions, each spending its own
  inputs into its own outputs, balanced on its own. The top-level transaction
  pays the fee for the whole transaction and carries the metadata.
  * Turn it on with the optional `sub_txs` key,
    `{ "count": K, "inputs": n, "outputs": m }`, also registered with the NixOS
    service; the compiler then splits enough funds for the sub-transactions.
  * It requires the Dijkstra era, and is not combined with Plutus yet.

* **Script variants and reference scripts.** A Plutus workload can spend
  variants of its script instead of the script itself: scripts with distinct
  hashes that behave and cost the same, so that the ledger's per-block cache
  of decoded scripts doesn't hide the cost of decoding them.
  * Turn it on with the optional `plutus.variants` key,
    `{ "count", "size", "source", "group", "padding" }`, also registered with
    the NixOS service:
    * `size`: bytes per variant, or `null` for as small as can be.
    * `source`: `"reference"` to spend them by reference, `"witness"` to carry
      them in the witnesses. The setup publishes them as reference scripts
      either way, at the address of a new `ReferenceScripts` key, so that the
      two differ in nothing else.
    * `group`: the number of consecutive benchmarking inputs locked at one
      variant; by default as many as a transaction spends.
    * `padding`: what variants are padded with to their size, `"mixed"`
      (default) or `"error-spine"`, the costliest to decode per byte measured.
      The padding is never run.
  * New in benchmarking scripts: the `DefineScriptSet` action, the
    `PublishScripts` generator and the `PayToScriptSet` pay mode.

* **New Plutus type `FixedLoop`**: a loop script run with the loop count its
  redeemer gives, at the execution units of one run (new `MeasuredScript`
  budget), for benchmarks that vary something other than execution.

* **Plutus V4.**
  * Execution units of Plutus V4 scripts, for automatic budgeting and
    `calibrate-script`.
  * New `LoopV4` script (`scripts-fallback/LoopV4.plutus`,
    `data/LoopV4.redeemer.json`), LoopV3's loop as a Plutus V4 script.
  * `calibrate-script` builds its transaction in Dijkstra for protocol version
    12 or later; `data/protocol-parameters-v12.json` is a v12 preview.

* **Exact fee estimates.** They count the key witnesses a signed transaction
  carries, so the estimate equals the ledger's minimum fee.
  * **Fix:** `minFeeRefScriptCostPerByte`, and for Dijkstra the reference
    script cost stride and multiplier, were lost on the way through
    `ProtocolParameters`, which made reference scripts free in every fee
    estimate.

* **New `TraceBenchTxSubWarning` trace** (severity Warning). `NtoM` and
  `NestedNtoM` warn rather than stop when a transaction needs more than the
  configured fee, and `NestedNtoM` when one exceeds `maxTxSize`: the nodes
  will reject those transactions.

* API changes:
  * Outputs are ledger outputs (`Exp.TxOut`), fund witnesses are
    `Exp.AnyWitness`, a fund's value is a `Coin`, and `TxEnvironment`'s fee and
    metadata are a `Coin` and a `TxMetadata`.
  * Outputs are planned as data before a transaction is built: new `Output`,
    `outputFund` and `outputFunds` in `Cardano.TxGenerator.UTxO`, and
    `Destination`, `payTo`, `storeOutputs` and `buildAndStore` in
    `Cardano.Benchmarking.Wallet`. They replace `CreateAndStore`, `ToUTxO`,
    `mangle`, `createAndStore`, `sourceToStoreTransaction` and friends.
  * New `NestedTxGenerator`, `genNestedTx`, `SubTxShape`, `PlutusVariants`,
    `ScriptPadding`, and the `Cardano.TxGenerator.ScriptVariants` module.
  * Removed `mkTxFee`, `mkTxValidityUpperBound` and `liftAnyEra`; the
    metadata functions of `SizedMetadata` take an `Exp.Era` and a
    `TxMetadata`.

## 2.19 -- Oct 2026

* **Support for `extraConfig.initialFunds.file` genesis injections**, where a
  Shelley genesis keeps its initial funds in a separate JSON file referenced
  by `file` plus the Blake2b-256 `hash` of its contents. Previously only the
  legacy top-level `initialFunds` and the inline
  `extraConfig.initialFunds.data` were read. Reading, hashing and parsing go
  through cardano-ledger's `foldInjectionData`, so resolution matches the
  node's.
* API changes:
  * New `genesisWithResolvedInitialFunds` and `getShelleyGenesisDir`.
  * `genesisInitialFunds`, `genesisInitialFundForKey` and
    `genesisSecureInitialFund` fail on file injections; run them inside
    `genesisWithResolvedInitialFunds`.
  * `getGenesis` and `getGenesisPath` renamed to `getShelleyGenesis` and
    `getShelleyGenesisPath`; old names kept as deprecated aliases.

## 2.18 -- Sep 2026

* **New submit mode: dump the transaction stream to disk** instead of
  submitting it to a node.
  * Two new mutually exclusive CLI flags, `--dump-show FILE` (Haskell `Show`
    text) and `--dump-cbor FILE` (raw CBOR, framed as an indefinite-length
    list, one transaction per item), replace the normal submission phase.
  * Wired into the workbench: `start-cluster --dump-show` / `--dump-cbor`
    runs nodes only far enough to serve protocol parameters, then dumps the
    generator's stream to a fixed filename and shuts the cluster down as
    soon as the generator exits — skipping workloads and performance
    analysis, which don't apply to a dump run.

* **Dijkstra era submission support.** The generator can now submit
  transactions built for the Dijkstra era (protocol version 12), including
  over Node-to-Node.

## 2.17 -- Jun 2026

* **New remote submission endpoint** — send transactions to a remote endpoint
  instead of the local socket / Node-to-Node protocols, behind a generic,
  backend-agnostic transport interface.
  * Turn it on with the optional `submissionEndpointProtocol` and
    `submissionEndpointURI` keys, which must be set together, e.g.
    `"submissionEndpointProtocol": "Ogmios", "submissionEndpointURI": "ws://127.0.0.1:1337"`.
  * The only endpoint type currently supported is `Ogmios`: each transaction is
    sent over a WebSocket as a JSON-RPC 2.0 `submitTransaction` call.
  * It reroutes **every** phase, but only tx submission: genesis fund import, UTxO splitting, and benchmarking.
  * It is a **functional transport, not a benchmark**: no TPS pacing, no metrics.
    * The endpoint replaces `targetNodes` as the submission target: set
      `"targetNodes": []` alongside an endpoint. The compiler rejects a config
      that sets both (and, as before, a benchmark with an empty `targetNodes`).
    * Both options are registered with the NixOS service
      (`nix/nixos/tx-generator-service.nix`), which checks this contract at
      evaluation time.
  * **A rejected transaction fails the whole run** (the process exits non-zero):
    * Setup phases stop at the first rejection.
    * The benchmark phase finishes the stream, then fails if anything was rejected.
    * Rejections are traced through the normal pipeline, including the detail the endpoint reports.
  * Endpoint runs trace their operation: connection lifecycle, every
    transaction's outcome (acceptances with the tx id the endpoint reports),
    and a final accepted/rejected tally.

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
