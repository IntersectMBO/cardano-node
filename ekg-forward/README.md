# ekg-forward

[![GitHub CI](https://github.com/input-output-hk/ekg-forward/workflows/CI/badge.svg)](https://github.com/input-output-hk/ekg-forward/actions)

## What Is It

`ekg-forward` is a library allowing to forward [EKG system metrics](https://hackage.haskell.org/package/ekg-core) from one process to another one.

## Main Use Case

You have one Haskell application that collects its EKG system metrics (both predefined and custom) and another Haskell application that needs to receive those metrics. You can think of the first application as a Forwarder and the second one as an Acceptor.

## Motivation

There is `ekg` [package](https://hackage.haskell.org/package/ekg) that already lets you remotely monitor a running Haskell process over HTTP. But there are three main differences between `ekg` and `ekg-forward`:

1. `ekg` provides HTTP server for monitoring, `ekg-forward` is a lightweight library without HTTP and REST API.
2. `ekg-forward` is based on Haskell typed protocol, which provides type-level guarantees of correctness.
3. `ekg-forward`'s network layer uses `ouroboros-network-framework` [package](https://github.com/IntersectMBO/ouroboros-network/) which supports both network sockets and local pipes for connection.

## How To Use It

You can find demo programs in the `demo` directory.

## Limitations

Please note that **not all** EKG metrics are supported in the current release:

1. [Gauge](https://hackage.haskell.org/package/ekg-core-0.1.1.7/docs/System-Metrics-Gauge.html) - supported
2. [Label](https://hackage.haskell.org/package/ekg-core-0.1.1.7/docs/System-Metrics-Label.html) - supported
3. [Counter](https://hackage.haskell.org/package/ekg-core-0.1.1.7/docs/System-Metrics-Counter.html) - supported
4. [Distribution](https://hackage.haskell.org/package/ekg-core-0.1.1.7/docs/System-Metrics-Distribution.html) - **not** supported
