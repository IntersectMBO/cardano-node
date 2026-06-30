### Added

- Added `--nodes` flag to specify node roles (SPO/relay) and custom `cardano-node` binaries per node.
  Example: `--nodes spo,spo:node-bin=/path/to/bin,relay,relay`.

### Changed

- Renamed `NodeOptions` to `NodeWithOptions` and `TestnetNodeOptions` to `TestnetNodesWithOptions`
  (exported from `Testnet.Start.Types` and `Cardano.Testnet`). The new types include a `nodeBin` field for
  specifying a per-node `cardano-node` binary.
