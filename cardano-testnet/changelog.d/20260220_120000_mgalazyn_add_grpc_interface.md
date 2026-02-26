### Added

- Added `--enable-grpc` flag to `cardano-testnet` to enable the gRPC interface (via `cardano-rpc`) when starting a testnet.
- Added `cardanoEnableRpc` field to `CardanoTestnetOptions` (default `False`).
- Added `nodeRpcSocketPath` helper to `Testnet.Types` for deriving the gRPC socket path from a node's socket path.

### Tests

- Added integration tests for the gRPC interface: `hprop_rpc_query_pparams` verifies protocol parameters and UTxO queries over gRPC, and `hprop_rpc_transaction` verifies transaction submission over gRPC and confirms the transaction lands on-chain.
