# Cardano Tracer

`cardano-tracer` is a service for logging and monitoring over Cardano nodes. After it is connected to the node, it periodically asks the node for different information, receives it, and handles it.

# Contents

1. [Introduction](#Introduction)
   1. [Motivation](#Motivation)
   3. [Overview](#Overview)
2. [Build and run](#Build-and-run)
3. [Configuration](#Configuration)
   1. [Distributed Scenario](#Distributed-scenario)
   2. [Local Scenario](#Local-scenario)
   3. [Network Magic](#Network-magic)
   4. [Requests](#Requests)
   5. [Logging](#Logging)
   6. [Logs Rotation](#Logs-rotation)
   7. [Prometheus](#Prometheus)
   8. [EKG Monitoring](#EKG-monitoring)
   9. [Verbosity](#Verbosity)

# Introduction

## Motivation

Previously, the node handled all the logging by itself. Moreover, it provided monitoring tools as well: two web-servers, for Prometheus and for EKG monitoring page. `cardano-tracer` is a result of _moving_ all the logging/monitoring-related stuff from the node to a separate service. As a result, the node became smaller, faster, and simpler.

## Overview

You can think of Cardano node as a **producer** of logging/monitoring information, and `cardano-tracer` as a **consumer** of this information. After the network connection between them is established, `cardano-tracer` periodically asks for such an information, and the node replies with it.

There are 3 kinds of such an information:

1. Trace object, which contains different logging data. `cardano-tracer` constantly asks for new trace objects each `N` seconds, receives them and stores them in the log files and/or in Linux `systemd`'s journal.
2. EKG metric, which contains some system metric. Please [read EKG documentation](https://hackage.haskell.org/package/ekg-core) for more info. `cardano-tracer` constantly asks for new EKG metrics each `N` seconds, receives them and displays them using monitoring tools.
3. Data points, which contains arbitrary information about the node. Please note that `cardano-tracer` asks for new data points only by _explicit_ request when it needs it, there is no constant asking.

Please note that `cardano-tracer` can work as an aggregator as well: _one_ `cardano-tracer` process can receive the information from _multiple_ nodes.

# Build and run

Please make sure you have [Nix installed](https://nixos.org/guides/install-nix.html).

First of all, go to Nix shell using the following command (from the root of `cardano-node` repository):

```
nix-shell
```

Now build and install `cardano-tracer` using the following command:

```
cabal build cardano-tracer && cabal install cardano-tracer --installdir=PATH_TO_DIR --overwrite-policy=always
```

where `PATH_TO_DIR` is a path to a directory where `cardano-tracer` will be copied after building.

Then you can go to `PATH_TO_DIR` and run `cardano-tracer` using the following command:

```
./cardano-tracer --config PATH_TO_CONFIG
```

where `PATH_TO_CONFIG` is a path to your configuration file, please see below an explanation about it. You can find an example of the configuration file in `configuration` subdirectory.

# Configuration

The way how to configure `cardano-tracer` depends on your requirements. There are two basic scenarios:

1. **Distributed** scenario, when `cardano-tracer` is working on one machine, and your nodes are working on another machine(s).
2. **Local** scenario, when `cardano-tracer` and your nodes are working on the same machine.

Distributed scenario is for real-life case: for example, you have `N` nodes working on `N` different AWS-instances, and you want to collect all the logging/monitoring information from these nodes using one `cardano-tracer` process working on your machine.

Local scenario is for testing case: for example, you want to try your new infrastructure from scratch, so you run `N` nodes and one `cardano-tracer` process on your machine.

**IMPORTANT NOTICE**: Please note that `cardano-tracer` **does not** support connection via IP-address and port, to avoid unauthorized connections. The **only** way to establish connection with the node is a local socket (Unix sockets or Windows named pipes).

## Distributed Scenario

This is an example with 3 nodes and one `cardano-tracer`:

```
machine A            machine B            machine C
+-----------------+  +-----------------+  +-----------------+
| node 1          |  | node 2          |  | node 3          |
+-----------------+  +-----------------+  +-----------------+
                ^             ^             ^
                 \            |            /
                  \           |           /
                   v          v          v
                   +---------------------+
                   | cardano-tracer      |
                   +---------------------+
                   machine D
```

The minimalistic configuration file for `cardano-tracer` would be:

```
{
  "networkMagic": 764824073,
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

The `network` field specifies the way how `cardano-tracer` will be connected to your nodes. Here you see `AcceptAt` tag, which means that `cardano-tracer` works as a server: it _accepts_ network connections by listening the local socket `/tmp/cardano-tracer.sock`. Your nodes work as clients: they _initiate_ network connections using their local sockets. It can be shown like this:

```
machine A            machine B            machine C
+-----------------+  +-----------------+  +-----------------+
| node 1          |  | node 2          |  | node 3          |
|      \          |  |      \          |  |      \          |
|       v         |  |       v         |  |       v         |
|    local socket |  |    local socket |  |    local socket |
+-----------------+  +-----------------+  +-----------------+





                   +---------------------+
                   | local socket        |
                   |      ^              |
                   |       \             |
                   |      cardano-tracer |
                   +---------------------+
                   machine D
```

To establish the real network connections between your machines, you need SSH forwarding:

```
machine A            machine B            machine C
+-----------------+  +-----------------+  +-----------------+
| node 1          |  | node 2          |  | node 3          |
|      \          |  |      \          |  |      \          |
|       v         |  |       v         |  |       v         |
|    local socket |  |    local socket |  |    local socket |
+-----------------+  +-----------------+  +-----------------+
                ^             ^             ^
                 \            |            /
                 SSH         SSH          SSH
                   \          |          /
                    v         v         v
                   +---------------------+
                   | local socket        |
                   |      ^              |
                   |       \             |
                   |      cardano-tracer |
                   +---------------------+
                   machine D
```

The idea of SSH forwarding is simple: we do connect not the processes directly, but their network endpoints instead. You can think of it as a network channel from the local socket on one machine to the local socket on another machine:

```
machine A                                      machine D
+----------------------------+                 +------------------------------------+
| node 1 ---> local socket <-|---SSH channel---|-> local socket <--- cardano-tracer |
+----------------------------+                 +------------------------------------+
```

So neither your nodes nor `cardano-tracer` know anything about SSH, they only know about their local sockets. But because of SSH forwarding mechanism they work together from different machines. And since you already have your SSH credentials, the connection between your nodes and `cardano-tracer` will be secure.

So, to connect `cardano-node` working on machine `A` with `cardano-tracer` working on machine `D`, run this command on machine `A`:

```
ssh -nNT -L /tmp/cardano-tracer.sock:/tmp/cardano-node.sock -o "ExitOnForwardFailure yes" john@109.75.33.121
```

where:

- `/tmp/cardano-tracer.sock` is a path to the local socket on machine `A`,
- `/tmp/cardano-node.sock` is a path to the local socket on machine `D`,
- `john` is a user you use to login on machine `D`,
- `109.75.33.121` is an IP-adress of machine `D`.

Run the same command on machines `B` and `C` to connect corresponding nodes with the same `cardano-tracer` working on machine `D`.

## Local Scenario

As was mentioned above, local scenario is for testing, when your nodes and `cardano-tracer` are working on the same machine. In this case all these processes can see the same local sockets directly, so we don't need `ssh`. The configuration file for 3 local nodes would look like this:

```
{
  "networkMagic": 764824073,
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

As you see, it is the same configuration file: the `cardano-tracer` works as a server: it _accepts_ network connections by listening the local socket `/tmp/cardano-tracer.sock`. Your local nodes work as clients: they _initiate_ network connections using the _same_ local socket `/tmp/cardano-tracer.sock`.

There is another way to connect `cardano-tracer` to your nodes: the `cardano-tracer` can work as _initiator_, this is an example of configuration file:

```
{
  "networkMagic": 764824073,
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

As you see, the tag in `network` field is `ConnectTo` now, which means that `cardano-tracer` works as a client: it _establishes_ network connections with your local nodes via the local sockets `/tmp/cardano-node-*.sock`. In this case each socket is used by a particular node.

Please use `ConnectTo`-based scenario only if you really need it. Otherwise, it is **highly recommended** to use `AcceptAt`-based scenario.

## Network Magic

The field `networkMagic` specifies the value of network magic. It is an integer constant from the genesis file, the node uses this value for the network handshake with peers. Since `cardano-tracer` should be connected to the node, it needs that network magic.

The value from the example above, `764824073`, is taken from the Shelley genesis file for Mainnet. Please take this value from the genesis file your nodes are launched with.

## Requests

The optional field `loRequestNum` specifies the number of log items that will be requested from the node. For example, if `loRequestNum` is `10`, `cardano-tracer` will constantly ask 10 log items in one request. This value is useful for reducing the network traffic: it is possible to ask 50 log items in one request or ask them in 50 requests one at a time. Please note that if `loRequestNum` is bigger than the real number of log items in the node, all these items will be returned immediately. For example, if `cardano-tracer` asks 50 log items but the node has only 40 log items _in this moment of time_, these 40 items will be returned, there is no waiting for additional 10 items.

The optional field `ekgRequestFreq` specifies the period of how often EKG metrics will be requested, in seconds. For example, if `ekgRequestFreq` is `1`, `cardano-tracer` will ask for new EKG metrics every second. Please note that there is no limit as `loRequestNum`, so every request returns _all_ the metrics the node has _in this moment of time_.

There are default values for `loRequestNum` and `ekgRequestFreq`, so if you are not sure - please remove these fields from your configuration file to use default values.

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

This is an example of log structure:

```
/rootDir
   /subdirForNode0
      node-2021-11-25T10-06-52.json
      node.json -> /rootDir/subdirForNode0/node-2021-11-25T10-06-52.json
```

In this example, `subdirForNode0` is a subdirectory containing log files with items received from the node `0`. And `node-2021-11-25T10-06-52.json` is the _current_ log: it means that currently `cardano-tracer` is writing items in this log file, via symbolic link `node.json`.

The field `logMode` specifies logging mode. There are two possible modes: `FileMode` and `JournalMode`. `FileMode` is for storing logs to the files, `JournalMode` is for storing them in `systemd`'s journal. Please note that if you choose `JournalMode`, the field `logRoot` will be ignored.

The field `logFormat` specifies the format of logs. There are two possible modes: `ForMachine` and `ForHuman`. `ForMachine` is for JSON format, `ForHuman` is for human-friendly text format.

Please note that `logging` field accepts the list, so you can specify more than one logging section. For example, for both log formats:

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

In this case log items will be written in JSON format (in `.json`-files) as well as in text format (in `.log`-files).

## Logs Rotation

An optional field `rotation` describes parameters for log rotation. Please note that if you skip this field, all the log items will be stored in one single file, and usually it's not what you want. These are rotation parameters:

```
"rotation": {
  "rpFrequencySecs": 30,
  "rpKeepFilesNum": 3,
  "rpLogLimitBytes": 50000,
  "rpMaxAgeHours": 1
}
```

The field `rpFrequencySecs` specifies rotation period, in seconds. In this example, `rpFrequencySecs` is `30`, which means that rotation check will be performed every 30 seconds.

The field `rpLogLimitBytes` specifies the maximum size of the log file, in bytes. In this example, `rpLogLimitBytes` is `50000`, which means that once the size of the current log file is 50 KB, the new log file will be created.

The field `rpKeepFilesNum` specifies the number of the log files that will be kept. In this example, `rpKeepFilesNum` is `3`, which means that 3 _last_ log files will always be kept.

The field `rpMaxAgeHours` specifies the lifetime of the log file, in hours. In this example, `rpMaxAgeHours` is `1`, which means that each log file will be kept for 1 hour only. After that, the log file is treated as outdated and will be deleted. Please note that N _last_ log files (specified by `rpKeepFilesNum`) will be kept even if they are outdated.

## Prometheus

The optional field `hasPrometheus` specifies the host and port of the web page with metrics. For example:

```
"hasPrometheus": {
  "epHost": "127.0.0.1",
  "epPort": 3000
}
```

Here the web page is available at `http://127.0.0.1:3000`. Please note that if you skip this field, the web page will not be available.

After you open `http://127.0.0.1:3000` in your browser, you will see the list of identifiers of connected nodes (or the warning message, if there are no connected nodes yet), for example:

```
* tmp-cardano-tracer.sock@0
* tmp-cardano-tracer.sock@1
* tmp-cardano-tracer.sock@2
```

Each identifier is a hyperlink to the page where you will see the **current** list of metrics received from the corresponding node, in such a format:

```
rts_gc_par_tot_bytes_copied 0
rts_gc_num_gcs 2
rts_gc_max_bytes_slop 15880
rts_gc_num_bytes_usage_samples 1
rts_gc_wall_ms 4005
...
rts_gc_par_max_bytes_copied 0
rts_gc_mutator_cpu_ms 57
rts_gc_mutator_wall_ms 4004
rts_gc_gc_cpu_ms 1
rts_gc_cumulative_bytes_used 184824
```

## EKG Monitoring

The optional field `hasEKG` specifies the hosts and ports of two web pages:

1. the list of identifiers of connected nodes,
2. EKG monitoring page.

For example:

```
"hasEKG": [
  {
    "epHost": "127.0.0.1",
    "epPort": 3100
  },
  {
    "epHost": "127.0.0.1",
    "epPort": 3101
  }
]
```

The page with the list of identifiers of connected nodes will be available at `http://127.0.0.1:3100`, for example:

```
* tmp-cardano-tracer.sock@0
* tmp-cardano-tracer.sock@1
* tmp-cardano-tracer.sock@2
```

Each identifier is a hyperlink, after clicking to it you will be redirected to `http://127.0.0.1:3101` where you will see EKG monitoring page for corresponding node.

## Verbosity

The optional field `verbosity` specifies the verbosity level for the `cardano-tracer` itself. There are 3 levels:

1. `Minimum` - `cardano-tracer` will work as silently as possible.
2. `ErrorsOnly` - messages about problems will be shown in standard output.
3. `Maximum` - all the messages will be shown in standard output. **Caution**: the number of messages can be huge.

Please note that if you skip this field, `ErrorsOnly` verbosity will be used by default.
