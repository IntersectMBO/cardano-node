# Scripts

 This document describes how to use a given script and its purpose.

## Development scripts

  Scripts in this directory are not strictly intended for end users,
  they rather serve as developer-oriented proving grounds for automation.

  As such, they break often, but also, by virtue of their existence, they provide
  some insight into how the executables provided by this repository can be used.

### Index

- <span><b>chairman.sh</b></span> <br/>  Run the Chairman consensus checker against a cluster started
                     by `shelley-testnet*.sh` family of scripts
- <span><b>cluster-log.sh</b></span> <br/> Visualise blockchain from logs emitted by `cluster-test.sh`
- <span><b>cluster-test.sh</b></span> <br/>- Run a CI-oriented cluster (consisting of Byron Rewrite
         nodes, a proxy and Byron Legacy nodes) in a VM, while capturing the logs
- <span><b>generator.sh</b></span> <br/> Run the transaction generator against a cluster started
                     by `shelley-testnet` family of scripts
- <span><b>genesis.sh</b></span> <br/> Generate a new genesis in the `configuration/` folder
- <span><b>get-default-key-address.sh</b></span> <br/> Given a signing key, print its associated address
- <span><b>issue-genesis-utxo-expenditure.sh</b></span> <br/> Write a file with a transaction
         spending a genesis UTxO entry, given a key owning it
- <span><b>issue-utxo-expenditure.sh</b></span> <br/> Write a file with a transaction spending a
         normal UTxO entry, given a key owning it
- <span><b>mainnet.sh</b></span> <br/> Run a node against Cardano Mainnet
- <span><b>shelley-testnet2.sh</b></span> <br/> Start a dev cluster with 3 nodes,
         with neat curses-based UI (run from tmux)
- <span><b>shelley-testnet-dns.sh</b></span> <br/> Start a dev cluster with 3 nodes (run from tmux)
- <span><b>shelley-testnet.sh</b></span> <br/> Start a dev cluster with 3 nodes, with
         basic logging (run from tmux)
- <span><b>start-node.sh</b></span> <br/> Basic individual node startup script
- <span><b>start-wallet.sh</b></span> <br/> Start a basic wallet client
- <span><b>submit-tx.sh</b></span> <br/> Submit a transaction file made by `issue-*-expenditure.sh`
                      family of scripts


# Shelley Testnet

The `./scripts/shelley-testnet.sh` script starts up three nodes that are
connected via TCP sockets to each other and produce blocks according to the
algorithm selected (e.g. "BFT").  The blocks are shared among the nodes and
after verification integrated into a nodes ledger.  The user can submit
transactions to a node which includes them in its local mempool, and eventually
in the next block it will create.


```

 +---------+         +---------+
 |         | <-----> |         |
 | node 0  |         | node 1  |
 |         | <-+ +-> |         |
 +---------+   | |   +---------+
               v v

            +---------+
            |         |
            | node 2  |
            |         |
            +---------+


```

## Startup testnet

Add the next two lines to your $HOME/.tmux.conf file:
```
set-window-option -g mouse on
set -g default-terminal "tmux-256color"
```

1.) Generate the necessary genesis files by running

    `./scripts/genesis.sh`

2.) create a `tmux` session

`tmux new-session -s Demo`

3.) run the demo script in this new session

`./scripts/shelley-testnet.sh`


## Run chairman

#### Purpose:

Connect with all the core nodes and store the forks from a common prefix.  If any of them is longer than the security parameter (k) it will throw an exception.

#### Usage:

1) Run `./scripts/shelley-testnet2.sh`.

2) Run `./scripts/chairman.sh` in a separate terminal.


## Connect to mainnet

Run `./scripts/mainnet.sh`

This script connects to several IOHK nodes on mainnet.

## Submit a tx to the testnet

Following the instructions above, the window of the terminal will be split into four panes.
Three of the panes showing the nodes running and a shell to enter commands for transaction submission, e.g.

```
./scripts/submit-tx.sh generated-tx-file
```
The `submit-tx.sh` script by default sends the transaction to node with node id 0.

See cardano-cli's Transactions section for instructions on how to generate a tx.

## Startup testnet with dns

You can run:

`./scripts/shelley-testnet-dns.sh`

instead of `shelley-testnet.sh`.
It requires that the addresses `local.iohk.io` and `local6.iohk.io` resolve to `127.0.0.1` and `::1` respectively.

You can use [unbound](https://github.com/NLnetLabs/unbound) dns server. You can use the following `/etc/unbound/unbound.conf` file:
```
server:
  verbosity: 1
  local-data: "local.iohk.io A 127.0.0.1"
  local-data: "local6.iohk.io AAAA ::1"
```

## Run transaction generator

https://github.com/input-output-hk/iohk-monitoring-framework/wiki/Transaction-Generator:-Usage-Guide#launch-a-cluster

## CI cluster

#### Prerequisites

Prerequisites include having the Nix package manager installed and suitably
configured.

Configuration consists of ensuring `/etc/nix/nix.conf` have:

1. The IOHK Hydra binary cache enabled:

   - https://hydra.iohk.io must be in either of the `substituters` or the
     `binary-caches` space-separated list options
   - <b><span>hydra.iohk.io</span>:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ</b> must be
     present in the `public-keys` space-separated list option

2. Configuration allowing KVM to be used by Nix:

   `kvm` and `nixos-test` must be in the `system-features` space-separated list option

   The effective configuration can be observed as follows:

    `$ nix show-config | grep system-features`<br/>
    `system-features = benchmark big-parallel kvm nixos-test`

#### Running

 To launch the CI cluster, just execute the script:

`./scripts/cluster-test.sh`

It'll create a log file for the entire cluster's output in a file named:

`cluster.EPOCHTIME.COMMIT-ID.log`.

#### Logs

Logs for individual machines can be extracted using the `cat` and `less`
subcommands of `scripts/cluster-log.sh`:

`scripts/cluster-log.sh --ip 10.1.0.2 cluster.1573270530.4158764bcaed9e6b.log less`

#### Charting

The logs can interpret to plot a chart of the produced blockchain (including forks),
either from the standpoint of a single node, as specified by its IP address:

`scripts/cluster-log.sh --ip 10.1.0.2 cluster.1573270530.4158764bcaed9e6b.log png`

..or for the entire cluster:

`scripts/cluster-log.sh --cluster 10.1.0.%s 1 cluster.1573270530.4158764bcaed9e6b.log png`

Both commands will produce PNG files for the relevant node's chart & launch
the Eye of Gnome (eog) program to view that.

The number of blocks to show is by default limited to 25.

In the charts:

  - blue-colored blocks are made by Byron Rewrite nodes,
  - green-colored blocks are made by Byron Legacy nodes,
  - arrow labels have the public key of the block signer & its BFT id.
