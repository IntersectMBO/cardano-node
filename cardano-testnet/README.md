# cardano-testnet

`cardano-testnet` starts a local Cardano cluster: it generates the genesis
files, keys and configuration for a set of nodes, starts the nodes, and keeps
the cluster running until stopped. It is useful for testing custom cluster
configurations locally.

## Getting cardano-testnet

`cardano-testnet` ships with [cardano-node releases](https://github.com/IntersectMBO/cardano-node/releases):
release archives contain the `cardano-testnet` binary alongside `cardano-node`
and `cardano-cli`. Alternatively, you can:

- Run it with nix, bundled with matching `cardano-node` and `cardano-cli`
  binaries:

  ```bash
  nix run github:IntersectMBO/cardano-node#cardano-testnet -- cardano
  ```

- Build it from source. We refer to
  [the instructions](https://developers.cardano.org/docs/operators/node/installing-cardano-node/)
  for building cardano-node from source. Once you are done with these
  instructions, run the following to build cardano-testnet:

  ```bash
  cabal build cardano-testnet
  ```

Unless you use the self-contained `nix run` invocation above, define two
environment variables pointing to your `cardano-node` and `cardano-cli`
executables:

```shell
export CARDANO_CLI=path to your executable
export CARDANO_NODE=path to your executable
```

`cardano-testnet` resolves these executables through the environment variables
(not through `PATH`), and they should come from the same release as
`cardano-testnet` itself — see [Supported versions](#supported-versions) below.

## Top-level commands

`cardano-testnet` has the following commands:

```text
Usage: cardano-testnet (cardano | create-env | version | help)

Available options:
  -h,--help                Show this help text

Available commands:
  cardano                  Start a testnet and keep it running until stopped
  create-env               Create a sandbox for Cardano testnet
  version                  Show cardano-testnet version
  help                     Show cardano-testnet help
```

## Options for launching a local cluster

To launch a local cluster, you should use the `cardano-testnet cardano` command, whose API is as follows:

```text
Usage: cardano-testnet cardano
  [ --node-env FILEPATH [--preserve-timestamps]
  | [--nodes SPEC[,SPEC...] | --num-pool-nodes COUNT]
    [--max-lovelace-supply WORD64]
    [--num-dreps NUMBER]
    [--testnet-magic INT]
    [--epoch-length SLOTS]
    [--slot-length SECONDS]
    [--active-slots-coeff DOUBLE]
    [--params-file FILEPATH | --params-mainnet]
    [--output-dir DIRECTORY]
  ]
  [--enable-new-epoch-state-logging]
  [ --enable-grpc
  | --enable-grpc-http [--grpc-listen-address IP] [--grpc-listen-port-base PORT]
  ]
  [--use-kes-agent]
  [--disable-chain-stall-watchdog]

  Start a testnet and keep it running until stopped

Available options:
  --node-env FILEPATH      Path to the node's environment (which is generated
                           otherwise). You can generate a default environment
                           with the 'create-env' command, then modify it and
                           pass it with this argument.
  --preserve-timestamps    Do not update the time stamps in genesis files to
                           current date.
  --nodes SPEC[,SPEC...]   Comma-separated node specifications. SPO nodes must
                           come before relay nodes. Each spec is a role (spo or
                           relay) optionally followed by :node-bin=<path>. If
                           the path contains commas, colons, double quotes, or
                           backslashes, wrap it in double quotes and escape any
                           literal double quotes as \" and backslashes as \\
                           within. To prevent bash from consuming the double
                           quotes, enclose the whole argument in single quotes.
                           Examples: --nodes
                           spo,spo:node-bin=/path/to/bin,relay,relay | --nodes
                           'spo:node-bin="/path,with:commas",relay'
  --num-pool-nodes COUNT   Number of pool nodes. Note this uses a default node
                           configuration for all nodes.
  --max-lovelace-supply WORD64
                           Max lovelace supply that your testnet starts with.
                           (default: 100000020000000)
  --num-dreps NUMBER       Number of delegate representatives (DReps) to
                           generate. (default: 3)
  --testnet-magic INT      Specify a testnet magic id. (default: 42)
  --epoch-length SLOTS     Epoch length, in number of slots. (default: 500)
  --slot-length SECONDS    Slot length. (default: 0.1)
  --active-slots-coeff DOUBLE
                           Active slots coefficient. (default: 5.0e-2)
  --params-file FILEPATH   File containing custom on-chain parameters in
                           Blockfrost format:
                           https://docs.blockfrost.io/#tag/cardano--epochs/GET/epochs/latest/parameters
  --params-mainnet         Use mainnet on-chain parameters
  --output-dir DIRECTORY   Directory where to store files, sockets, and so on.
                           It is created if it doesn't exist. If unset, a
                           temporary directory is used.
  --enable-new-epoch-state-logging
                           Enable new epoch state logging to
                           logs/ledger-epoch-state.log
  --enable-grpc            [EXPERIMENTAL] Enable gRPC endpoint on all of testnet
                           nodes. The listening socket file will be the same
                           directory as node's N2C socket. Listens on a unix
                           socket.
  --enable-grpc-http       [EXPERIMENTAL] Enable gRPC endpoint over HTTP without
                           TLS (h2c) on all of testnet nodes. Listens on
                           127.0.0.1 on a random port per node unless
                           --grpc-listen-address/--grpc-listen-port-base are
                           given.
  --grpc-listen-address IP IP address the gRPC HTTP endpoints of all nodes
                           listen on. Requires --enable-grpc-http.
                           (default: 127.0.0.1)
  --grpc-listen-port-base PORT
                           Base port for gRPC HTTP listeners: testnet node i
                           listens on PORT+i-1. Random free ports are used when
                           omitted. Requires --enable-grpc-http.
  --use-kes-agent          Get Praos block forging credentials from kes-agent
                           via the default socket path
  --disable-chain-stall-watchdog
                           Disable the background watchdog that makes the run
                           fail fast, with a diagnosis, when the chain stops
                           producing blocks forever.
  -h,--help                Show this help text
```

We now go over these different options as there are many interactions between them.

### Using a custom node configuration file and custom genesis files

cardano-testnet has two behaviors, depending on whether you want to use defaults or not. You can either:

1. Default the node configuration file, the genesis files (Byron, Shelley, Alonzo, Conway, and Dijkstra), and the topology file. In this case, don't specify `--node-env`: `cardano-testnet` will take care of generating all these files.
2. Pass a pre-generated node sandbox environment using `--node-env`. This environment should be generated using the `cardano-testnet create-env` sub-command. In that case, `cardano-testnet` will not generate any configuration file. The specifics of the `create-env` sub-command are detailed below.

### Specifying nodes

There are two mutually exclusive ways to specify the nodes in the cluster:

1. `--num-pool-nodes COUNT` simply specifies the number of SPO nodes. All nodes use the default `cardano-node` binary (from your `CARDANO_NODE` environment variable).
2. `--nodes SPEC[,SPEC...]` allows fine-grained control. Each spec is a role (`spo` or `relay`), optionally followed by `:node-bin=<path>` to specify a custom `cardano-node` binary for that node. SPO nodes must come before relay nodes in the list. For example:
   - `--nodes spo,spo,relay` starts two SPO nodes and one relay, all using the default binary.
   - `--nodes spo,spo:node-bin=/path/to/other/bin,relay` starts two SPO nodes (the second using a custom binary) and one relay.

If neither option is given, the default is one SPO node and two relay nodes.

### The `cardano-testnet` sandbox environment

`cardano-testnet` stores all its keys (SPO, dreps, etc.), configuration files, and node data in a single directory.

If you don't specify `--output-dir`, cardano-testnet will use a directory named `testnet` in the current working directory, creating it if it doesn't exist.
If you specify `--output-dir`, cardano-testnet will use the specified directory to store the keys and the nodes' data. In both cases we recommend using a fresh directory every time, otherwise there is a risk that one run poisons the other.
In addition, using your own directory makes it easier to inspect the logs after the testnet has finished, or while it is running.

The structure of the directory of a running testnet is as follows (using bash pseudo-syntax to avoid enumerations; this is for the default cluster of one SPO node and two relays — pool, DRep, and delegator counts follow the options described above):

```text
├── byron-gen-command
│   └── genesis-keys.000.key
├── delegate-keys
│   ├── delegate{1,2,3}
│   │   ├── kes.{skey,vkey}
│   │   ├── key.{skey,vkey}
│   │   ├── opcert.{cert,counter}
│   │   └── vrf.{skey,vkey}
│   └── README.md
├── drep-keys
│   ├── drep{1,2,3}
│   │   └── drep.{skey,vkey}
│   └── README.md
├── genesis-keys
│   ├── genesis{1,2,3}
│   │   └── key.{skey,vkey}
│   └── README.md
├── logs
│   └── node{1,2,3}
│       ├── node.pid
│       ├── stderr.log
│       └── stdout.log
├── node-data
│   ├── node{1,2,3}
│   │   ├── db
│   │   │   └── <node database files>
│   │   ├── port
│   │   └── topology.json
├── pools-keys
│   ├── pool1
│   │   ├── byron-delegate.key
│   │   ├── byron-delegation.cert
│   │   ├── cold.{skey,vkey}
│   │   ├── kes.{skey,vkey}
│   │   ├── opcert.{cert,counter}
│   │   ├── staking-reward.{skey,vkey}
│   │   └── vrf.{skey,vkey}
│   └── README.md
├── socket
│   ├── node{1,2,3}
│   │   └── sock
├── stake-delegators
│   ├── delegator{1,2,3}
│   │   ├── payment.{skey,vkey}
│   │   └── staking.{skey,vkey}
├── utxo-keys
│   ├── utxo{1,2,3}
│   │   └── utxo.{addr,skey,vkey}
│   └── README.md
├── {alonzo,byron,conway,dijkstra,shelley}-genesis.json
├── genesis-input.dijkstra.json
├── configuration.yaml
└── current-stake-pools.json
```

We draw the reader's attention to two things:

1. The nodes' logs are located in `logs/node1/`, `logs/node2/`, etc. Those are useful for debugging.
2. The genesis files are at the root of the output directory, and the topology files are in the node subdirectories. This will be relevant for the next section.

### Creating your own sandbox environment

`cardano-testnet` provides the option to create a sandbox environment as described above, without launching the node network itself. This allows the modification of configuration files, Genesis or otherwise. The API of the command is as follows:

```text
Usage: cardano-testnet create-env
  [--nodes SPEC[,SPEC...] | --num-pool-nodes COUNT]
  [--max-lovelace-supply WORD64]
  [--num-dreps NUMBER]
  [--testnet-magic INT]
  [--epoch-length SLOTS]
  [--slot-length SECONDS]
  [--active-slots-coeff DOUBLE]
  [--params-file FILEPATH | --params-mainnet]
  --output DIRECTORY

  Create a sandbox for Cardano testnet

Available options:
  --nodes SPEC[,SPEC...]   Comma-separated node specifications. SPO nodes must
                           come before relay nodes. Each spec is a role (spo or
                           relay) optionally followed by :node-bin=<path>. If
                           the path contains commas, colons, double quotes, or
                           backslashes, wrap it in double quotes and escape any
                           literal double quotes as \" and backslashes as \\
                           within. To prevent bash from consuming the double
                           quotes, enclose the whole argument in single quotes.
                           Examples: --nodes
                           spo,spo:node-bin=/path/to/bin,relay,relay | --nodes
                           'spo:node-bin="/path,with:commas",relay'
  --num-pool-nodes COUNT   Number of pool nodes. Note this uses a default node
                           configuration for all nodes.
  --max-lovelace-supply WORD64
                           Max lovelace supply that your testnet starts with.
                           (default: 100000020000000)
  --num-dreps NUMBER       Number of delegate representatives (DReps) to
                           generate. (default: 3)
  --testnet-magic INT      Specify a testnet magic id. (default: 42)
  --epoch-length SLOTS     Epoch length, in number of slots. (default: 500)
  --slot-length SECONDS    Slot length. (default: 0.1)
  --active-slots-coeff DOUBLE
                           Active slots coefficient. (default: 5.0e-2)
  --params-file FILEPATH   File containing custom on-chain parameters in
                           Blockfrost format:
                           https://docs.blockfrost.io/#tag/cardano--epochs/GET/epochs/latest/parameters
  --params-mainnet         Use mainnet on-chain parameters
  --output DIRECTORY       Directory where to create the sandbox environment.
  -h,--help                Show this help text
```

If you want to run a testnet with custom parameters, we suggest the following workflow:

```bash
rm -rf env # Ensure there is no existing sandbox environment
cardano-testnet create-env --output env # Creates the sandbox environment in env/

# Modify the configuration files as you see fit

cardano-testnet cardano --node-env env # Run the testnet on the custom environment
```

Notes:
1. By default, `cardano-testnet cardano` updates the time stamps in genesis files to the current date. If you don't want this behavior (e.g., you have carefully set custom timestamps), use `--preserve-timestamps`.
2. `create-env` generates the node configuration file without genesis file hashes, precisely so that the genesis files can be modified without having to recompute hashes every time.
3. The following configuration files are safe for modifications:
    - `env/configuration.yaml`: high-level configuration options for the `cardano-node` binary
    - `env/{byron,shelley,alonzo,conway,dijkstra}-genesis.json`: the genesis files for the testnet chain.
    - `env/node-data/node{1,2,3}/topology.json`: topology files for individual nodes.

### Getting on-chain parameters from mainnet

By default, the `create-env` sub-command generates on-chain parameters at their initial value. If you want your test network to use a different set of on-chain parameters without specifying them by hand, you can use one of the following options:
1. Use `--params-file` to specify a set of on-chain parameters. The file must be formatted as a JSON response from this [Blockfrost endpoint](https://docs.blockfrost.io/#tag/cardano--epochs/GET/epochs/latest/parameters).
2. Use `--params-mainnet` to get on-chain parameters similar to the current state of mainnet. Those parameters are provisioned from [this file](https://raw.githubusercontent.com/input-output-hk/cardano-parameters/refs/heads/main/mainnet/parameters.json), which is updated every epoch (i.e. every five days).

These options are also available directly on the `cardano-testnet cardano` command when not using `--node-env`.

### Era-specific flags: Shelley

There are four flags that control values specified in the Shelley genesis file:

```text
  --max-lovelace-supply WORD64
                           Max lovelace supply that your testnet starts with.
                           (default: 100000020000000)
  --epoch-length SLOTS     Epoch length, in number of slots. (default: 500)
  --slot-length SECONDS    Slot length. (default: 0.1)
  --active-slots-coeff DOUBLE
                           Active slots coefficient. (default: 5.0e-2)
```

Note that all of these flags are ignored when a sandbox environment is provided via `--node-env`.

### Era-specific flags: Conway

There is one flag that control values that appear in the Conway genesis file and that's the number of dreps:

```text
  --num-dreps NUMBER       Number of delegate representatives (DReps) to
                           generate. (default: 3)
```

Like the Shelley flags, this flag is ignored if a sandbox environment is provided via `--node-env`.

### gRPC endpoints (experimental)

Every node of the testnet can expose the node's experimental gRPC interface:

- `--enable-grpc` makes each node listen on a Unix socket, located in the same directory as the node's node-to-client socket.
- `--enable-grpc-http` makes each node listen over HTTP without TLS (h2c) instead. By default each node listens on `127.0.0.1` on a random free port; use `--grpc-listen-address` to change the IP address, and `--grpc-listen-port-base` to assign deterministic ports (node `i` listens on `PORT + i - 1`).

When gRPC is enabled, the endpoint of each node is printed on startup, right after the `Testnet started` line.

### Understanding the output of `cardano-testnet`

When you run `cardano-testnet` (here with `--output-dir /tmp/testnet`), you will see output similar to this:

```shell
Creating environment: /tmp/testnet/
Starting testnet in environment: /tmp/testnet/
Testnet started
Waiting for shutdown (Ctrl+C)
```

When using `--node-env` with a pre-existing environment, the `Creating environment` line is omitted.

Once the line `Testnet started` appears, the testnet is running and building blocks.

To interact with the testnet using [cardano-cli](https://github.com/IntersectMBO/cardano-cli), set the following environment variables (adjusting paths to match your `--output-dir`):

```shell
export CARDANO_NODE_SOCKET_PATH=/tmp/testnet/socket/node1/sock
export CARDANO_NODE_NETWORK_ID=42
```

In order to shutdown the testnet, you can press `Ctrl+C` and `cardano-testnet` will kill all the nodes that were spawned when it was started.

## Supported versions

cardano-testnet is designed to run the cardano-cli and cardano-node that ship
in the same release. Mixing binaries across releases is unsupported; use cli
and node from the same release tarball as this cardano-testnet.

The cardano-api and cardano-cli versions this build was compiled against are
printed by `cardano-testnet version`.
