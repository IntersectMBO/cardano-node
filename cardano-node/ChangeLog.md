# Changelog for cardano-node

## 1.5.0 -- January 2020

### node changes

### consensus changes

### ledger changes

### network changes

## 1.4.0 -- January 2020

### node changes
- Move configuration of tracers from CLI to the config file (#474)
- Move support for trace forwarding into a logging plugin (#454)
- Make the launcher scripts able to be used with nix, cabal or stack (#458)
- Fix non-liveview mode in shelley-testnet.sh script (#459)
- Elide repeated log messages (#445)
- Simplify cardano-cli interface (#476)
- Remove unneeded cardano-cli dump-hardcoded-genesis subcommand (#451)
- Remove dependency on cardano-ledger-test (#451)
- Remove message counters from config files (#454)
- Add the mempool size metric to the console live view (#457)
- Update scripts and README

### consensus changes
- Limit forged block size based on current limits from the ledger state (#1400, #1363)
- Add ability to get recent ledger states, for local query protocol (#1440, #1446)
- Refactor block forging code (#786, #1445)
- Fix rare bug in block forging (due to unavoidable race condition) (#1437, #1459)
- Fix a case of dubious async exception handling in chain DB (#1452, #1453)
- Additional tests to better cover EBBs in combination with PBFT consensus. (#1353)
- Various tidying up in the consensus QC tests. (#1401)
- Allow disabling assertions for production builds (#1248)
- Add support to get mempool snapshot size for use in system benchmarks (#1431)
- Adjustments to tracing in block forging for system benchmarks (#1432)

### ledger changes
- Relax the validation rule for on-chain registered software versions to better
  match the legacy implementation. This fixes validation of the testnet.

### network changes
- Significant refactoring of network-mux package (#1247)
- Reduce CPU cost of sending over the mux (approx 10%) (#1420, #1434)
- Simplify IOSim's Async representation (#1394)

## 1.3.0 -- January 2020

### node changes
- Update to latest dependencies (consensus, ledger, logging etc)

### consensus changes
- Add initial support for multiple slot lengths (for hard forks) (#282, #1385)
- Do chain selection based only on the latest header in each chain (#1227)
- Significant performance improvements in serving headers and blocks (#1378)
- Snapshot ledger state on shutdown to avoid long restart times (1388)
- Fix garbage collection of previously applied points in the ledger DB (#1381)
- Fix unnecessary memory retention in the volatile DB indices (#1379)

### ledger changes
- Update to latest version of cardano-ledger-specs

### network changes
- None

## 1.2.0 -- December 2019

### node changes
- Update to latest dependencies (consensus, ledger, logging etc)
- More monitoring counters/statistics, including Prometheus output (#366)
- Remove unused legacy and wallet configuration fields (code and confg files)
- Improve README files
- Hide tracing options from default `--help` command
- Fix flakeyness in logging setup & shutdown
- Stop message counter messages from appearing in log files
- Refactor CLI and config parser code.

### consensus changes
- Improve chain sync serving performance by binary streaming of headers (#1330)
- Much more reliable detection of disk corruption in epoch files (#290, #1253)
- Limit the size of forged blocks (#686)
- Change mempool capacity from number of transactions to size in bytes (#974)
- Set node's default mempool capacity to 2x the mainnet block size
- Avoid logging messages about block forging for nodes that do not forge
- Allow starting before genesis start time by waiting, and log message
- Fix a number of bugs related to EBBs, found by QC tests
- Improved the QC test case generators to cover EBBs better
- Fix a memory retention bug and make thunk detection tests pass
- Use file locks for the chain DB (#1266)
- Get the slot length from the genesis file (#1345)

### ledger changes
- Remove support for HD addresses (not needed by the ledger, just wallets)
- Remove unnecessary SafeSigner abstraction
- Remove unnecessary EncryptedSigningKey
- Remove dependency on scrypt
- Add tests for isRedeemAddress, improve address encoding/decoding

### #network changes
- Added initial peer-to-peer governor with QC tests. Not yet used.

## 1.1.0 -- December 2019

### node changes
- Updated to latest consensus and network versions
- Script to connect to mainnet using deployed mainnet relays
- CI integration test for mixed cluster of old cardano-sl nodes and new nodes
- Improved CI "chairman" integration test
- Improved CLI and config file handling
- Adjusted log severity levels for many trace messages
- Better default RTS flags
- New --validate-db flag to revalidate all on-disk database files
- Updated README instructions

### consensus changes
- Adjusted the dividing line between ledger and consensus for block production
  code for clearer structure and so features are tested in the right place.
- Progress on refactoring needed to support the hard fork protocol combinator.
- Serve blocks as binary blobs without deserialising for improved performance.
- Check checksums when reading blocks to detect disk corruption.
- Finish feature to support accepting blocks from the near "future", once the
  local time catches up. This gives a degree of lenience for clock skew, while
  still respecting the Ouroboros rule of "blocks from the future" being invalid.
- Added more extensive QuickCheck tests for BFT consensus.
- Fixed bugs identified by QuickCheck state machine tests.
- Improvements to the API of the IO simulator.
- Trace the reason for a known block being invalid when rejecting a header.
- Add additional trace points.

### network changes
- Simplified API to network layer used by consensus and node clients.
- Documented wire format of the local transaction submission protocol.
- Added infrastructure to support size and time limits in mini-protocol driver.

## 1.0.0 -- November 2019

- Complete rewrite compared to previous cardano-sl series.

- New modular design. The cardano-node is the top level for the node and
  aggregates the other components from other packages: consensus, ledger and
  networking, with configuration, CLI, logging and monitoring.

- The node no longer incorporates wallet or explorer functionality. The wallet
  backend and explorer backend are separate components that run in separate
  external processes that communicate with the node via local IPC.
