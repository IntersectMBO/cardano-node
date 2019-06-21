[![Build Status](https://badge.buildkite.com/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a.svg)](https://buildkite.com/input-output-hk/cardano-node)

# cardano-node

Integration of the [ledger](https://github.com/input-output-hk/cardano-ledger), [consensus](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus), [networking](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network) and [node shell](https://github.com/input-output-hk/cardano-shell) repositories.
[Logging](https://github.com/input-output-hk/iohk-monitoring-framework) is provided as a [feature](https://github.com/input-output-hk/cardano-shell/blob/master/app/Cardano/Shell/Features/Logging.hs) by the node shell to the other packages.


# demonstration

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

## startup of the demonstration

Write the following commands in a terminal:

1.) enter into a `nix-shell`
2.) create a `tmux` session
    `tmux new-session -s Demo`
3.) run the demo script in this new session
    `./scripts/demo.sh`

The window of the terminal will be split into four panes showing the three nodes running and a shell to enter commands for transaction submission, e.g.

```
./scripts/submit-tx.sh --bft -n 2 --address a --amount 99

```
The above command will prepare a transaction of amount 99 to address _a_ and sends the transaction for validation and integration into a block to node _2_.


