## Fixed

- Add a chain-stall watchdog, on by default (`--disable-chain-stall-watchdog` or `runtimeEnableChainStallWatchdog` to opt out): when the chain stops producing blocks forever, every test now fails fast with a message explaining the mechanism, instead of hanging in whatever it was waiting on.
- Make the timeout for testnet startup depend on the testnet config.

## Changed

- `testnetNodes` in `TestnetRuntime` is now `NonEmpty` (a testnet always has at least one node), so consumers no longer need node-count checks before taking the first node.
