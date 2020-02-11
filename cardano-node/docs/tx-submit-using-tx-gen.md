# Submit Transactions to WebAPI using Transaction Generator

This document demonstrates how to submit transactions to `cardano-tx-submit-webapi` using [transaction generator](https://github.com/input-output-hk/cardano-node/blob/master/cardano-node/src/Cardano/CLI/Tx/Generation.hs).

## Prepare Local Cluster to Start

Go to `cardano-node` repository and remove nodes' local databases if needed:

```
$ rm -rf db/
```

Also remove an old genesis files:

```
$ rm -rf configuration/genesis/*
```

Now create the new genesis:

```
$ ./scripts/genesis.sh
```

Please see an output, you should see something like this:

```
...
genesis created with hash = b8f44e852982a88aaf352684bc507fd337a15117f01f43816abfb2487a67d348
...
```

## Run Local Cluster

Go to `tmux` and launch the local cluster (Shelley in Live View mode):

```
$ tmux
$ ./scripts/shelley-testnet-live.sh
```

You will see 3 `tmux` panes with the nodes. Please see the `block number` value: if it increases, the cluster is working correctly.

## Run Tx Submit WebAPI

Go to `cardano-explorer` repository and create configuration for `cardano-tx-submit-webapi`:

```
$ ./scripts/gen-tx-submit-config.sh --require-magic --genesis-hash GEN_HASH --output config.yaml
```

where `GEN_HASH` is the hash of genesis you've got from the output of `./scripts/genesis.sh` script. As a result, you will see the file `config.yaml` in the current directory.

Run `cardano-tx-submit-webapi`:

```
$ cabal new-run cardano-tx-submit-webapi -- --config config.yaml --genesis-file /path-to/cardano-node/configuration/genesis/genesis.json --socket-path /path-to/cardano-node/socket/0 --port 8101
```

Possible output is this:

```
[cardano-tx-submit:Info:9] [2020-02-11 08:05:34.82 UTC] Running tx-submit node
[cardano-tx-submit:Info:9] [2020-02-11 08:05:34.82 UTC] NetworkMagic: RequiresMagic 459045235
[cardano-tx-submit:Info:9] [2020-02-11 08:05:34.82 UTC] localInitiatorNetworkApplication: connecting to node via "/path-to/cardano-node/socket/0"
[cardano-tx-submit:Info:10] [2020-02-11 08:05:34.82 UTC] Running tx-submit web server on http://localhost:8101/
```

It means that `cardano-tx-submit-webapi` is listening to the port `8101` and ready to receive transactions.

## Run transaction Generator

Go to `cardano-node` repository again and run transaction generator. For example, if you have built it using `stack`:

```
$ stack exec -- cardano-cli generate-txs --config /path-to/tx-gen-log-config.yaml --signing-key /path-to/cardano-node/configuration/genesis/delegate-keys.000.key --delegation-certificate /path-to/cardano-node/configuration/genesis/delegation-cert.000.json --genesis-file /path-to/cardano-node/configuration/genesis/genesis.json --socket-path /path-to/cardano-node/socket/0 --num-of-txs 100 --add-tx-size 0 --inputs-per-tx 1 --outputs-per-tx 1 --tx-fee 10000000 --tps 2 --sig-key /path-to/cardano-node/configuration/genesis/delegate-keys.000.key --sig-key /path-to/cardano-node/configuration/genesis/delegate-keys.001.key --sig-key /path-to/cardano-node/configuration/genesis/delegate-keys.002.key --submit-to-api "http://localhost:8101/api/submit/tx" --target-node '("127.0.0.1",3000)'
```

In this example 100 transactions will be generated. Please note that this number **does not** include genesis transaction and splitting transactions, so technically **103** tranactions will be generated: 1 genesis transaction + 2 splitting transactions + 100 main transactions. You can see it in the output of `cardano-tx-submit-webapi`:


```
[cardano-tx-submit:Info:22] [2020-02-11 08:10:25.30 UTC] txSubmitPost: received 249 bytes   <- genesis transaction
[cardano-tx-submit:Info:22] [2020-02-11 08:10:25.30 UTC] Success
[cardano-tx-submit:Info:24] [2020-02-11 08:10:25.30 UTC] txSubmitPost: received 3849 bytes  <- splitting transaction #1
[cardano-tx-submit:Info:24] [2020-02-11 08:10:25.30 UTC] Success
[cardano-tx-submit:Info:25] [2020-02-11 08:10:25.31 UTC] txSubmitPost: received 2646 bytes  <- splitting transaction #2
[cardano-tx-submit:Info:25] [2020-02-11 08:10:25.31 UTC] Success
[cardano-tx-submit:Info:26] [2020-02-11 08:10:45.33 UTC] txSubmitPost: received 307 bytes   <- main transaction #1
[cardano-tx-submit:Info:26] [2020-02-11 08:10:45.33 UTC] Success
...
[cardano-tx-submit:Info:141] [2020-02-11 08:11:35.82 UTC] txSubmitPost: received 308 bytes  <- main transaction #100
[cardano-tx-submit:Info:141] [2020-02-11 08:11:35.83 UTC] Success
```

Please note that an argument `--submit-to-api` specifies the full API endpoint, for example, `"http://localhost:8101/api/submit/tx"`. In this case, an argument `--target-node '("127.0.0.1",3000)'` will be **ignored**, because all 103 transactions will be sent to specified API endpoint (not to the node as usual).
