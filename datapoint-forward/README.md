# datapoint-forward

`datapoint-forward` is a library allowing to forward different structured data from one process to another one. It is built upon [`typed-protocols`](https://github.com/input-output-hk/ouroboros-network/tree/master/typed-protocols). Please note that forwarded type should provide `ToJSON` instance.

The `trace-dispatcher` is using `datapoint-forward` to forward different structured data from the node to exernal acceptors (for example, `cardano-tracer`). The example of such a data is node's basic info.

## Developers

Benchmarking team is responsible for this library. The primary developer is [@denisshevchenko](https://github.com/denisshevchenko).
