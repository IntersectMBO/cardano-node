[![Build Status](https://badge.buildkite.com/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a.svg)](https://buildkite.com/input-output-hk/cardano-node)

# cardano-node

Integration of the [ledger](https://github.com/input-output-hk/cardano-ledger), [consensus](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus), [networking](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network) and [node shell](https://github.com/input-output-hk/cardano-shell) repositories.
[Logging](https://github.com/input-output-hk/iohk-monitoring-framework) is provided as a [feature](https://github.com/input-output-hk/cardano-shell/blob/master/app/Cardano/Shell/Features/Logging.hs) by the node shell to the other packages.

# `genesis-tool`

A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

Detailed documentation in its attendant [README](app/genesis-tool/README.md).

# Node demonstration

The demonstration starts up three nodes that are connected to each other and produce blocks according to the algorithm selected (e.g. "BFT").
The blocks are shared among the nodes and after verification integrated into a nodes ledger.
The user can submit transactions to a node which includes them in its local mempool, and eventually in the next block it will create.


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

## Startup of the demonstration

Add the next two lines to your $HOME/.tmux.conf file:

set-window-option -g mouse on
set -g default-terminal "tmux-256color"

In a first terminal start the central logging process:

    `./scripts/trace-acceptor.sh`

In a second terminal:

1.) create a `tmux` session

    `tmux new-session -s Demo`

2.) run the demo script in this new session

    `./scripts/demo.sh`

The window of the terminal will be split into four panes showing the three nodes running and a shell to enter commands for transaction submission, e.g.

```
./scripts/submit-tx.sh --real-pbft -n 2 --address a --amount 99 --txin ababa --txix 0

```
The above command will prepare a transaction of amount 99 to address _a_ and sends the transaction for validation and integration into a block to node _2_.
Increment the last argument '--txix' to send in a new transaction.


3.) or you can run

    `./scripts/demo-dns.sh`

    instead of `demo.sh`.  It requires that the addresses `local.iohk.io` and
    `local6.iohk.io` resolve to `127.0.0.1` and `::1` respectively.  You can
    use [unbound](https://github.com/NLnetLabs/unbound) dns server.  You can
    use the following `/etc/unbound/unbound.conf` file:
    ```
    server:
      verbosity: 1
      local-data: "local.iohk.io A 127.0.0.1"
      local-data: "local6.iohk.io AAAA ::1"
    ```




# development

run *ghcid* with: `ghcid -c "cabal new-repl exe:cardano-node --reorder-goals"`

