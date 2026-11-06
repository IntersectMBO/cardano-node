# trace-forward

This library specifies two protocols allowing to forward different information from the node to external applications (for example, `cardano-tracer` or `cardano-wallet`). These protocols are built upon [`typed-protocols`](https://github.com/input-output-hk/typed-protocols).

The first one allows forwarding `TraceObject`s from the node to external applications. You can think of `TraceObject` as a log item, which will be saved in log files.

The second one allows forwarding `DataPoint`s, arbitrary structured data that provides `ToJSON` instance.

Please note that the node doesn't use this library directly. Instead, `trace-dispatcher` library is using it to forward mentioned data via different tracers.

# Demo

These protocols are `typed-protocols`-based, so they can be `Mux`-ed for use via the same network connection. The corresponding demo is provided by `cardano-tracer` project.

## Developers

Benchmarking team is responsible for this library. The primary developer is [@jutaro](https://github.com/jutaro).
