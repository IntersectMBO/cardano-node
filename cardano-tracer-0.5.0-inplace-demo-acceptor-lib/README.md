# Cardano Tracer

> Attention: systemd is enabled by default on Linux. It can be
> disabled manually with a cabal flag: `-f -systemd` when building on
> systems without it.

`cardano-tracer` is a service for logging and monitoring over Cardano nodes. After it is connected to the node, it periodically asks the node for different information, receives it, and handles it.

For more details please [read the documentation](https://github.com/intersectmbo/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md).

## Developers

Performance and Tracing team is responsible for this service. The primary developer is [Baldur Blöndal](https://github.com/Icelandjack).

## Feedback

Your bug reports and/or Feature Requests are welcome! Feel free to open a ticket in [this repository](https://github.com/intersectmbo/cardano-node/issues) with label `tracing`.
