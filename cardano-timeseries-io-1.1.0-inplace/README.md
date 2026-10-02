# Cardano Timeseries I/O

## What it does

The primary goal of the project is to serve as a standalone library for `cardano-tracer`, providing realtime metric storage & query.

The library exposes the following components:
- An interface of metric stores together with multiple implementations.
- A low-level unambiguous language & its interpreter against a metric store — for querying metrics.
- A high-level user-facing language & its elaborator to the low-level one.

On top of the library the project provides a simple CLI for reading a store off disk & executing a query against it, optionally interactively.

## CLI Syntax

```
Usage: cardano-timeseries-io FILE
                             ((-q|--query QUERY) | (-f|--file FILE) |
                               (-i|--interactive))

  Run a query against a metric store

Available options:
  -q,--query QUERY         Execute the query
  -f,--file FILE           Execute the file
  -i,--interactive         Enter REPL
  -h,--help                Show this help text
```

## Build flags
  - _profiling_ for enabling GHC profiling support.
