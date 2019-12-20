# Changelog for cardano-node

## Unreleased changes

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
