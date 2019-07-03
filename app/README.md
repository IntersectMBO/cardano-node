
## Running the node

You have to specify your network topology in a json file. There is an
example inside `configuration` called `simple-topology.json`. By default,
there are 3 core nodes and they all follow each other.
For example, to setup a minimal example, run in three separate terminal (in this order):

```
./scripts/start-node.sh --bft -n 0 --host 127.0.0.1 --port 3000
./scripts/start-node.sh --bft -n 1 --host 127.0.0.1 --port 3001
./scripts/start-node.sh --bft -n 2 --host 127.0.0.1 --port 3002
```

You will see that the three nodes syncs with each other and agree on a common chain.

## Submitting transactions

To submit transactions, first spin up the node you want to target, then type:

```
./scripts/submit-tx.sh --bft -n 2 --address a --amount 1000
```

This would send the transaction to the node 2. The node would then add the Tx
to its mempool and incorporate it into a block when it's elected leader.
You can chain transactions once you know the hash, which is printed after you
submit the transaction:

```
> ./scripts/submit-tx.sh --bft -n 2 --address a --amount 1000
Up to date
The Id for this transaction is: 6f6e1118
```

## Spawning trace acceptor

To spawn the trace acceptor:

```
./scripts/trace-acceptor.sh
```

This would spawn an acceptor which gathers all the messages being sent from the
nodes.
