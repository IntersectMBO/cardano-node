### Fixed

- Fixed flaky RPC transaction test that used a stale block height from a prior RPC connection to determine when to query UTxOs after submitting a transaction. Replaced the brittle block-counting wait with `retryUntilM`, which polls the RPC endpoint until the expected UTxOs appear at the destination address.
