
### Tests

- Added integration tests for `transaction build` with `--withdrawal`, covering both key-witnessed and Plutus V3 script-witnessed withdrawals. The tests verify that the auto-balancer correctly sums withdrawals into consumed value when the output exceeds the input.
