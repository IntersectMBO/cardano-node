# Changelog for cardano-testnet

## 11.2.0 -- 2026-10-27

- Support --prometheus-listen-address and --prometheus-listen-port
  (feature)
  [PR 6723](https://github.com/IntersectMBO/cardano-node/pull/6723)

- Added a new `--enable-tracer` option, which when set starts up `cardano-tracer` and causes all testnet nodes to send tracing data to it
  (feature)
  [PR 6710](https://github.com/IntersectMBO/cardano-node/pull/6710)

- When the network is ready, `cardano-testnet cardano` now writes a machine-readable `manifest.json` into the output directory. The file lists every socket, port, key path, genesis file and wallet address the cluster was started with, so scripts and test harnesses can discover the network without parsing the directory layout. The file is written atomically (it appears only after all readiness checks pass) and is never modified after being written. A strict JSON Schema (`cardano-testnet/schemas/manifest.schema.json`) accompanies the file.
  (feature)
  [PR 6693](https://github.com/IntersectMBO/cardano-node/pull/6693)

- The `cardano-testnet cardano` command gained `--enable-grpc-http` to enable the node gRPC endpoint over HTTP without TLS (h2c), with each node listening by default on 127.0.0.1 on a random free port; the listen address can be customised with `--grpc-listen-address`, and fixed ports can be assigned with `--grpc-listen-port-base` (testnet node *i* then listens on the base port plus *i*-1); the existing `--enable-grpc` option continues to serve gRPC over a unix socket; when gRPC is enabled, each node's gRPC endpoint is printed in the startup summary.
  (feature)
  [PR 6685](https://github.com/IntersectMBO/cardano-node/pull/6685)

- Testnet node configurations are written in the cardano-config format and through cardano-config, with their genesis hashes refreshed whenever the testnet starts and the node's default tracing options written out; removed createConfigJson, createConfigJsonNoHash, getByronGenesisHash, getShelleyGenesisHash, GenesisHashesPolicy and the configPath field of SubmitApiConf, and getStartTime no longer takes a path.
  (breaking)
  [PR 6677](https://github.com/IntersectMBO/cardano-node/pull/6677)

## 11.1.1 -- 2026-08-31

- - `testnetNodes` in `TestnetRuntime` is now `NonEmpty` (a testnet always has at least one node), so consumers no longer need node-count checks before taking the first node.
  - Add a chain-stall watchdog, on by default (`--disable-chain-stall-watchdog` or `runtimeEnableChainStallWatchdog` to opt out): when the chain stops producing blocks forever, every test now fails fast with a message explaining the mechanism, instead of hanging in whatever it was waiting on.
  - Make the timeout for testnet startup depend on the testnet config.
  (compatible, bugfix)
  [PR 6616](https://github.com/IntersectMBO/cardano-node/pull/6616)

- Migrated ping support to `cardano-diffusion:ping`: `Testnet.Ping.pingNode` now returns `Either PingClientException ()`, and the local `PingClientError` type is gone in favour of a re-export of `Cardano.Network.Ping.PingClientException`.
  (breaking)
  [PR 6604](https://github.com/IntersectMBO/cardano-node/pull/6604)

- Migrated RPC transaction and query tests from the old `Cardano.Api` transaction-building API to `Cardano.Api.Experimental`, using `Exp.ConwayEra` as the single era definition point, experimental tx body construction with direct ledger types, and `makeUnsignedTx`/`signTx` for transaction creation.
  (compatible)
  [PR 6577](https://github.com/IntersectMBO/cardano-node/pull/6577)

- - Refactored `NodeOption` from a sum type into a record with a `TestnetNodeOptions` container that enforces at the type level that SPO nodes come first and at least one is present.
  - `readNodeOptionsFromEnv` now validates that node directories are consecutively numbered and that SPOs come before relays.
  (compatible)
  [PR 6563](https://github.com/IntersectMBO/cardano-node/pull/6563)

- - Added `--nodes` flag to specify node roles (SPO/relay) and custom `cardano-node` binaries per node. Example: `--nodes spo,spo:node-bin=/path/to/bin,relay,relay`.
  - Renamed `NodeOptions` to `NodeWithOptions` and `TestnetNodeOptions` to `TestnetNodesWithOptions` (exported from `Testnet.Start.Types` and `Cardano.Testnet`). The new types include a `nodeBin` field for specifying a per-node `cardano-node` binary.
  (feature, compatible)
  [PR 6559](https://github.com/IntersectMBO/cardano-node/pull/6559)

- - Removed `CardanoTestnetOptions` type and `CreateEnvOptions` wrapper (replaced by purpose-specific types).
  - Removed dead fields `cardanoNodeLoggingFormat` and `cardanoOutputDir`.
  - `readNodeOptionsFromEnv`: scans an existing testnet environment directory to classify nodes as SPO or relay.
  - Split `CardanoTestnetOptions` into `TestnetCreationOptions` and `TestnetRuntimeOptions` so each function receives only the fields it uses.
  - `CardanoTestnetCliOptions` is now a sum type (`NoUserProvidedEnv | StartFromEnv`), making `--node-env` and `--num-pool-nodes` structurally mutually exclusive in the CLI parser.
  - Simplified `CardanoTestnetCreateEnvOptions` and `createTestnetEnv` signatures (fewer arguments, genesis options and on-chain params folded into `TestnetCreationOptions`).
  (breaking, feature, compatible)
  [PR 6552](https://github.com/IntersectMBO/cardano-node/pull/6552)

- Replaced `caseShelleyToBabbageOrConwayEraOnwards` and `conwayEraOnwardsConstraints` patterns with `obtainCommonConstraints` and a new `unsafeEraFromSbe` helper that converts `ShelleyBasedEra` to the experimental `Era` witness, simplifying era-dependent code in governance tests and epoch state processing.
  (compatible)
  [PR 6551](https://github.com/IntersectMBO/cardano-node/pull/6551)

- Fixed flaky RPC transaction test that used a stale block height from a prior RPC connection to determine when to query UTxOs after submitting a transaction. Replaced the brittle block-counting wait with `retryUntilM`, which polls the RPC endpoint until the expected UTxOs appear at the destination address.
  (bugfix)
  [PR 6550](https://github.com/IntersectMBO/cardano-node/pull/6550)

- - Added "Supported versions" section to README declaring the single-release policy for cli/node compatibility.
  - `cardano-testnet version` now reports the cardano-api and cardano-cli versions it was built against.
  (feature)
  [PR 6549](https://github.com/IntersectMBO/cardano-node/pull/6549)

- Disable `ExperimentalHardForksEnabled` in the default testnet config. After the experimental-gated `cardanoProtocolVersion` was bumped to 12, Conway-era testnets forged blocks with protocol version 12, which the Conway BBody rule rejects (max protocol version = 11). Dropping the flag makes the node use protocol version 11, which Conway accepts.
  (bugfix)
  [PR 6541](https://github.com/IntersectMBO/cardano-node/pull/6541)

- - Refactored `getEpochStateDetails` to fail immediately when the `foldEpochState` background thread encounters an error, instead of waiting for the full timeout. Previously, if the background thread failed, consumers would wait the entire 15-second timeout before reporting a generic failure. Now the error is captured into the shared `IORef` and surfaced with a meaningful message as soon as a consumer reads it.
  - Increased the epoch state initialisation timeout from 15 to 25 seconds.
  - Simplified `getEpochStateDetails` interface by removing the continuation parameter -- callers now use `fmap` to extract the fields they need.
  (compatible)
  [PR 6525](https://github.com/IntersectMBO/cardano-node/pull/6525)

- Added `--params-file` and `--params-mainnet` flags to `cardano-testnet cardano` subcommand.
  (feature)
  [PR 6467](https://github.com/IntersectMBO/cardano-node/pull/6467)

- - Added `--preserve-timestamps` flag. When set, genesis file timestamps are kept as-is.
  - Timestamps in genesis files are now updated to the current date by default.
  - The `--update-time` flag is now internal (hidden, kept for backward compatibility).
  (feature, compatible)
  [PR 6466](https://github.com/IntersectMBO/cardano-node/pull/6466)

- - Support for running with KES agent via `--use-kes-agent` flag, allowing testing block production with kes-agent.
  - Adapted to `NetworkTopology` type changes: `createTestnetEnv` now creates a concrete topology and allocates random ports to nodes, instead of relying on an abstract topology.
  - Adapted to changes in `TxSubmitResult`.
  - Adapted to latest ledger changes (blockfrost response types to match alonzo genesis ones).
  - Bumped dependencies: `ouroboros-network`, `ouroboros-consensus`, `cardano-api-10.25.*`, `cardano-crypto-class-2.3.*`.
  (feature, compatible)
  [PR 6402](https://github.com/IntersectMBO/cardano-node/pull/6402)

- - Added `--enable-grpc` flag to `cardano-testnet` to enable the gRPC interface (via `cardano-rpc`) when starting a testnet.
  - Added `cardanoEnableRpc` field to `CardanoTestnetOptions` (default `RpcDisabled`).
  - Added `nodeRpcSocketPath` helper to `Testnet.Types` for deriving the gRPC socket path from a node's socket path.
  - Renamed `cardano-testnet` CLI flag `--nodeLoggingFormat` to `--node-logging-format`.
  - Added integration tests for the gRPC interface: `hprop_rpc_query_pparams` verifies protocol parameters and UTxO queries over gRPC, and `hprop_rpc_transaction` verifies transaction submission over gRPC and confirms the transaction lands on-chain.
  (feature, test)
  [PR 6273](https://github.com/IntersectMBO/cardano-node/pull/6273)

## 10.1.0

* [Fix discrepancy in security parameter between Byron and Shelley genesis files](https://github.com/IntersectMBO/cardano-node/pull/6188)
* [Add an option to dump/load configuration sandbox](https://github.com/IntersectMBO/cardano-node/pull/6239)
* [Add flag to support P2P topology](https://github.com/IntersectMBO/cardano-node/pull/6263)
* [Add flag to update time stamps in custom environment](https://github.com/IntersectMBO/cardano-node/pull/6275)
* [Add option to create configuration sandbox with parameters from mainnet](https://github.com/IntersectMBO/cardano-node/pull/6289)

## 10.0.0

* Bump for node 10
* Update `cardano-ping` dependency
* Add `--num-dreps` parameter

## 8.7.0

* Using `cardano-node-8.7.0`, `cardano-api-8.33` and `cardano-cli-8.15`
* Update `ouroboros-network` dependency.

## 8.5.0

* Using `cardano-node-8.5.0`, `cardano-api-8.25` and `cardano-cli-8.11`

## 8.4.0

* Using `cardano-node-8.4.0`, `cardano-api-8.20` and `cardano-cli-8.8`

## 8.1.0

- [Parameterize default node configuration on era](https://github.com/intersectmbo/cardano-node/pull/5211)
