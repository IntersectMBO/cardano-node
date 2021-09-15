# cardano-tracer

`cardano-tracer` is an application for logging and monitoring over `cardano-node`. After it is connected to the node, it periodically asks the node for different information, receives it, and handles it.

# Contents

1. [Introduction](#Introduction)
   1. [Motivation](#Motivation)
   3. [Overview](#Overview)
2. [Build and run](#Build-and-run)
3. [Configuration](#Configuration)
   1. [Distributed Scenario](#Distributed-scenario)
   1. [Local Scenario](#Local-scenario)
   2. [Requests](#Requests)
   3. [Logging](#Logging)
   4. [Logs rotation](#Logs-rotation)
   5. [Prometheus](#Prometheus)

# Introduction

## Motivation

Previously, `cardano-node` handled all the logging by itself. Moreover, it provided monitoring tools as well, for example, a web page for EKG metrics. `cardano-tracer` is an attempt to _move_ all logging/monitoring-related stuff from the node to a separate application. As a result, the node will be smaller, faster, and simpler.

## Overview

You can think of `cardano-node` as a **producer** of logging/monitoring information, and `cardano-tracer` as a **consumer** of this information. And after the network connection between them is established, `cardano-tracer` periodically asks for logging/monitoring information, and `cardano-node` replies with it.

Currently, this information is presented in two items:

1. Trace object, which contains arbitrary information from the node. For more details, please see `Cardano.Logging.Types.TraceObject` from `trace-dispatcher` library.
2. EKG metric, which contains some system metric. Please [read the documentation](https://hackage.haskell.org/package/ekg-core) for more info.

Please note that `cardano-tracer` can work as an aggregator as well: _one_ `cardano-tracer` process can receive the information from _multiple_ `cardano-node` processes. In this case, received logging information will be stored in subdirectories: each subdirectory will contain all the items received from the particular node.

# Build and run

Please make sure you have [Nix installed](https://nixos.org/guides/install-nix.html).

First of all, go to Nix shell using the following command (from the root of `cardano-node` repository):

```
nix-shell
```

Now build `cardano-tracer` using the following command:

```
cabal build cardano-tracer
```

Then you can run `cardano-tracer` using the following command:

```
cabal exec -- cardano-tracer --config /path/to/your/config.json
```

Please see below an explanation about the configuration file.

# Configuration

The way how to configure `cardano-tracer` is depending on your requirements. There are two basic scenarios:

1. **Distributed** scenario, when `cardano-tracer` is working on one machine, and your nodes are working on another machine(s).
2. **Local** scenario, when `cardano-tracer` and your nodes are working on the same machine.

Distributed scenario is for real-world case: for example, you have a cluster from `N` nodes working on `N` different AWS-instances, and you want to collect all the logging/monitoring information from these nodes using one single `cardano-tracer` process working on your machine. So, by default you should consider using distributed scenario.

Local scenario is for testing case: for example, you want to try your new infrastructure from scratch, so you run `N` nodes and one `cardano-tracer` process on your laptop.

## Distributed Scenario

This is an example of the cluster from 3 nodes and one tracer:

```
machine A                 machine B                 machine C
+----------------------+  +----------------------+  +----------------------+
| cardano-node         |  | cardano-node         |  | cardano-node         |
+----------------------+  +----------------------+  +----------------------+
                       ^             ^              ^
                        \            |             /
                         \           |            /
                          v          v           v
                          +----------------------+
                          | cardano-tracer       |
                          +----------------------+
                          machine D
```

The minimalistic configuration file for `cardano-tracer` in such a scenario would be:

```
{
  "network": {
    "tag": "AcceptAt",
    "contents": "/tmp/cardano-tracer.sock"
  },
  "logging": [
    {
      "logRoot": "/tmp/cardano-tracer-logs",
      "logMode": "FileMode",
      "logFormat": "ForMachine"
    }
  ]
}
```

The `network` field specifies the way how `cardano-tracer` will be connected to your nodes. Here you see `AcceptAt` tag, which means that `cardano-tracer` works as a server: it _accepts_ network connections by listening the local Unix socket `/tmp/cardano-tracer.sock`. But if `cardano-tracer` _accepts_ the connections - who should _initiate_ them? The node cannot do that, because it listens its local Unix socket too. To do that, we use SSH forwarding. Please note that `cardano-tracer` **does not** support connection via IP-address and port, to avoid unauthorized connections, that's why we need `ssh` for distributed scenario.

It can be shown like this:


```
machine A                 machine B                 machine C
+----------------------+  +----------------------+  +----------------------+
| cardano-node         |  | cardano-node         |  | cardano-node         |
|             \        |  |             \        |  |             \        |
|              v       |  |              v       |  |              v       |
|         local socket |  |         local socket |  |         local socket |
+----------------------+  +----------------------+  +----------------------+
                       ^             ^               ^
                        \            |              /
                        SSH         SSH           SSH
                          \          |            /
                           v         v           v
                          +----------------------+
                          | local socket         |
                          |      ^               |
                          |       \              |
                          |        \             |
                          |       cardano-tracer |
                          +----------------------+
                          machine D
```

In this case, `ssh` connects each node with the same `cardano-tracer`. The idea of SSH forwarding is simple: we connect not the processes directly, but their network endpoints instead. You can think of it as a network channel from the local socket on one machine to the local socket on another machine. So, to connect `cardano-node` working on machine `A` with `cardano-tracer` working on machine `D`, run this command on machine `A`:

```
ssh -nNT -L /tmp/cardano-tracer.sock:/tmp/cardano-node.sock -o "ExitOnForwardFailure yes" john@109.75.33.121
```

where:

- `/tmp/cardano-tracer.sock` is a path to the local Unix socket on machine `A`,
- `/tmp/cardano-node.sock` is a path to the local Unix socket on machine `D`,
- `john` is a user you use to login on machine `D`,
- `109.75.33.121` is an IP-adress of machine `D`.

Run the same command on machines `B` and `C` to connect corresponding nodes with the same `cardano-tracer` working on machine `D`.

## Local Scenario

As was mentioned above, local scenario is for testing, when your nodes and `cardano-tracer` are working on the same machine. In this case all these processes can see the same local sockets directly, so we don't need `ssh`. The configuration file for the local cluster from 3 nodes will look like this this:

```
{
  "network": {
    "tag": "ConnectTo",
    "contents": [
      "/tmp/cardano-node-1.sock"
      "/tmp/cardano-node-2.sock"
      "/tmp/cardano-node-3.sock"
    ]
  },
  "logging": [
    {
      "logRoot": "/tmp/cardano-tracer-logs",
      "logMode": "FileMode",
      "logFormat": "ForMachine"
    }
  ]
}
```

As you see, the tag is `ConnectTo` now, which means that `cardano-tracer` works as a client: it _establishes_ network connections with the node via the local Unix sockets `/tmp/cardano-node-*.sock`. Please make sure your local nodes are listening corresponding sockets.

Let's explore other fields of the configuration file.

## Requests

The optional field `loRequestNum` specifies the number of log items that will be requested from the node. In this example, `loRequestNum` is `10`, it means that `cardano-tracer` will periodically ask 10 log item in one request. It is useful to reduce the network traffic: it is possible to ask 50 log items in one request or ask them in 50 requests one at a time. Please note that if `loRequestNum` is bigger than the real number of log items in the node, all these items will be returned immediately. For example, if `cardano-tracer` asks 50 log items but the node has only 40 log items _in this moment of time_, these 40 items will be returned, there is no waiting for missing 10 items.

The optional field `ekgRequestFreq` specifies the period of how often EKG metrics will be requested, in seconds. In this example, `ekgRequestFreq` is `1`, which means that `cardano-tracer` will ask for new EKG metrics every second. Please note that there is no limit as `loRequestNum`, so every request returns _all_ the metrics the node has _in this moment of time_.

## Logging

Logging is one of the most important features of `cardano-tracer`. The field `logging` describes logging parameters:

```
"logging": [
  {
    "logRoot": "/tmp/cardano-tracer-logs",
    "logMode": "FileMode",
    "logFormat": "ForMachine"
  }
]
```

The field `logRoot` specifies the path to the root directory. This directory will contain all the subdirectories with the log files inside. Please remember that each subdirectory corresponds to the particular node. If the root directory does not exist, it will be created.

The field `logMode` specifies logging mode. There are two possible modes: `FileMode` and `JournalMode`. `FileMode` is for storing logs to the files, `JournalMode` is for storing them in `systemd`'s journal.

The field `logFormat` specifies the format of logs. There are two possible modes: `ForMachine` and `ForHuman`. `ForMachine` is for JSON format, `ForHuman` is for human-friendly text format.

Please note that `logging` field accepts the list, you can specify more than one logging section. For example, for both log formats:

```
"logging": [
  {
    "logRoot": "/tmp/cardano-tracer-logs-json",
    "logMode": "FileMode",
    "logFormat": "ForMachine"
  },
  {
    "logRoot": "/tmp/cardano-tracer-logs-text",
    "logMode": "FileMode",
    "logFormat": "ForHuman"
  }
]
```

## Logs rotation

An optional field `rotation` describes parameters for log rotation. Please note that if you skip this field:

```
"rotation": null
```

all log items will be stored in one file, usually, it is not what you want. These are rotation parameters:

```
"rotation": {
  "rpKeepFilesNum": 1,
  "rpLogLimitBytes": 50000,
  "rpMaxAgeHours": 1
}
```

The field `rpKeepFilesNum` specifies the number of the log files that will be kept. In this example, `rpKeepFilesNum` is `1`, which means that 1 _last_ log file will always be kept.

The field `rpLogLimitBytes` specifies the maximum size of the log file, in bytes. In this example, `rpLogLimitBytes` is `50000`, which means that once the size of the current log file is 50 KB, the new log file will be created.

The field `rpMaxAgeHours` specifies the lifetime of the log file, in hours. In this example, `rpMaxAgeHours` is `1`, which means that each log file will be kept for 1 hour only. After that, the log file is treated as outdated and will be deleted. Please note that N _last_ log files (specified by `rpKeepFilesNum`) will be kept even if they are outdated.

## Prometheus

The optional field `hasPrometheus` specifies the host and port where the page with EKG metrics will be available. In this example, the web page will be available at the local port `12798`:

```
"hasPrometheus": [
  "127.0.0.1",
  12798
]
```

Please note that if you skip this field:

```
"hasPrometheus": null
```

the web page will not be available.

After you open `http://127.0.0.1:12798` in your browser you will see:

1. the warning message if there are no connected nodes,
2. the list of identifiers of connected nodes.

Each identifier is a hyperlink to the page where you will see the list of EKG metrics received from the corresponding node.
