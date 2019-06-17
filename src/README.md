
## Demo playground

Little playground to try in the wild the chain-following protocol where
consumers and producers run on named pipes and the communication happens out-of-process.

## Running the demo

You have to specify your network topology in a json file. There is an
example inside `demo-playground` called `simple-topology.json`. By default,
there are 3 core nodes and they all follow each other.
For example, to setup a minimal example, runs in three separate terminal (in this order):

```
./node-scripts/start-node.sh --bft -n 0
./node-scripts/start-node.sh --bft -n 1
./node-scripts/start-node.sh --bft -n 2
```

You will see that the three nodes syncs with each other and agree on a common chain.

## Submitting transactions

To submit transactions, first spin up the node you want to target, then type:

```
./demo-playground/submit-tx.sh -n 2 --address a --amount 1000
```

This would send the transaction to the node 2. The node would then add the Tx
to its mempool and incorporate it into a block when it's elected leader.
You can chain transactions once you know the hash, which is printed after you
submit the transaction:

```
> ./demo-playground/submit-tx.sh -n 2 --address a --amount 1000
Up to date
The Id for this transaction is: 6f6e1118
```
