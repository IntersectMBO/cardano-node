# Transaction Generator

The transaction generator stresses a cluster of Cardano nodes with a simmulated work load of transaction.
It works in two phases:

* Phase 1: Secure a genesis fund and prepare a set of initial UTxOs via the local protocol.
* Phase 2: Connect to a set of nodes via the node-to-node protocol and offer/transmit a number of transaction at a certain rate limit.

## Running the tx-generator
Either provide the configuration via a number of CLI arguments or run a JSON based benchmarking script.
An example script is in `bench/tx-generator/test/script.json`.

```
Usage: tx-generator COMMAND

Available commands:
  cliArguments             tx-generator with CLI arguments
  eraTransition            tx-generator demo era transition
  json                     tx-generator run JsonScript
```

Current maintainers: Marc and Serge
