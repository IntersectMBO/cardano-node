# datapoint-forward

`datapoint-forward` is a library allowing to forward different structured data from one process to another one. It is built upon [`typed-protocols`](https://github.com/input-output-hk/ouroboros-network/tree/master/typed-protocols).

The `trace-dispatcher` library is using `datapoint-forward` to forward different structured data from the node to exernal acceptors (for example, `cardano-tracer`). The example of such a data is node's basic info. Please note that each forwarded type should provide `ToJSON` and `FromJSON` instances, because it will be "packed" to `DataPoint` and then forwarded to the acceptor by request.

## Developers

Benchmarking team is responsible for this library. The primary developer is [@denisshevchenko](https://github.com/denisshevchenko).
