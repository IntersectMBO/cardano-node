
## Running the node

You have to specify your network topology in a json file. There is an
example inside `configuration` called `simple-topology.json`. By default,
there are 3 core nodes and they all follow each other.
For example, to setup a minimal example, run in three separate terminal (in this order):

```
./scripts/start-node.sh --bft -n 0 --log-config=./configuration/log-configuration-ekg.yaml
./scripts/start-node.sh --bft -n 1 --log-config=./configuration/log-configuration.yaml
./scripts/start-node.sh --bft -n 2 --log-config=./configuration/log-configuration.yaml
```

You will see that the three nodes syncs with each other and agree on a common chain.

## Submitting transactions

To submit transactions, first spin up the node you want to target, then type:

```
./scripts/submit-tx.sh -n 2 --address a --amount 1000 --log-config=./configuration/log-configuration.yaml
```

This would send the transaction to the node 2. The node would then add the Tx
to its mempool and incorporate it into a block when it's elected leader.
You can chain transactions once you know the hash, which is printed after you
submit the transaction:

```
> ./scripts/submit-tx.sh -n 2 --address a --amount 1000 --log-config=./configuration/log-configuration.yaml
Up to date
The Id for this transaction is: 6f6e1118
```
