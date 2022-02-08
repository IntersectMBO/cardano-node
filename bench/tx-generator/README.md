# Transaction Generator

The transaction generator stresses a cluster of Cardano nodes with a simmulated work load of transaction.

## Running the tx-generator
The tx-generator executes JSON based tx-generator scripts.
An example script is in `bench/tx-generator/test/script.json`.

The benchmarking infrastructure runs the tx-generator as an systemd service.
This service is defined as a NIXOS service definition.
The service definition has a set of "high-level" configuration options
and generates the corresponding "low-level" tx-generator script based on that options.

```
Usage:
tx-generator json FILE
```

Current maintainers: Marc and Serge

## Hacking the tx-generator
See Internals.md