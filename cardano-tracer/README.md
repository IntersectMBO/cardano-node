# Cardano Tracer

> Attention: systemd is enabled by default on Linux. It can be
> disabled manually with a cabal flag: `-f -systemd` when building on
> systems without it.

`cardano-tracer` is a service for logging and monitoring over Cardano nodes. After it is connected to the node, it periodically asks the node for different information, receives it, and handles it.

For more details please [read the documentation](https://github.com/intersectmbo/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md).

## RTView

> Attention: RTView is hidden behind a build flag. Enable with this cabal flag: `-f +rtview`.

RTView is a real-time monitoring tool for Cardano nodes (RTView is an abbreviation for "Real Time View"), it is a part of `cardano-tracer` service. RTView provides an interactive web page where you can see different kinds of information about connected nodes (something like Grafana).

For more details please [read its documentation](https://github.com/intersectmbo/cardano-node/blob/master/cardano-tracer/docs/cardano-rtview.md).

RTView is not feature complete and is thus disabled by default. Being
an experimental/optional component of `cardano-tracer` we will still
guarantee it remains buildable and usable in its current state.

## Developers

Performance and Tracing team is responsible for this service. The primary developer is [Baldur Blöndal](https://github.com/Icelandjack).

## Feedback

Your bug reports and/or Feature Requests are welcome! Feel free to open a ticket in [this repository](https://github.com/intersectmbo/cardano-node/issues) with labels `RTView` and `tracing`.
