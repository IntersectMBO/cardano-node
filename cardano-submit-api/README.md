# Testing cardano-submit-api

Setting this up for testing and for actual use on a real network.


### Pre-requisites

You will need a cardano network with payment address and keys.  This may be on `mainnet`, an official
testnet or a testnet that you've set up yourself.  We will assume these files are in a directory
called `playground`:

* `user-1-payment.addr` - User 1 payment address.  This address must have sufficient funds.
* `user-1-payment.skey` - User 1 signing key.
* `user-2-payment.addr` - User 2 payment address.
* `magic.flag` - The network magic flag.  This will be `--testnet-magic <magic>` or `--mainnet`
  depending on the network you are using.
* `node.socket` - The socket file for your network, or a symlink to that socket file.  If you
  have a node running, you can find the socket file by running the command
  `ps aux | grep cardano-node` and looking for the `--socket-path` option.

### Install and run the cardano-submit-api

Install the `cardano-submit-api`:

```bash
cardano-node $ cabal install cardano-submit-api --overwrite-policy=always
```

The `cardano-cli` lives in its own repository and is consumed here as a dependency, so take it from
the flake rather than building it from this tree:

```bash
cardano-node $ nix run .#cardano-cli -- --version
```

Pick a tracing configuration.  Submit-api accepts either YAML or JSON, so any of these work as-is
with no conversion:

* `cardano-submit-api/config/tx-submit-mainnet-config.yaml` in this repository.
* `share/<network>/submit-api-config.json` from a release tarball.
* `cardanoLib.defaultSubmitApiConfig` from iohk-nix.

Then run the `cardano-submit-api` against your network:

```bash
playground $ cardano-submit-api \
  --config submit-api-config.yaml \
  --socket-path node.socket \
  --port 8090 \
  $(cat magic.flag)
[2026-08-04 23:08:42.77Z][host:TxSubmitApi.Metrics.Started](Info,8) Starting metrics server on port 8081
[2026-08-04 23:08:42.77Z][host:TxSubmitApi.Endpoint.ListeningOnPort](Info,7) Web API listening on port 127.0.0.1:8090
```

Prometheus metrics are served on `/` of the metrics port, which defaults to `8081` and is set with
`--metrics-port`.  Note that `--listen-address` governs only the web API; the metrics server always
binds all interfaces.

```bash
playground $ curl -s http://localhost:8081/
# TYPE cardano_submit_api_metrics_tx_submit_counter counter
cardano_submit_api_metrics_tx_submit_counter  0
# TYPE cardano_submit_api_metrics_tx_submit_fail_counter counter
cardano_submit_api_metrics_tx_submit_fail_counter  0
```

The metric name prefix comes from `TraceOptionMetricsPrefix` in the tracing config; remove that key
for unprefixed names.

### Build and submit a transaction

In another terminal, find out how much ADA is in your user 1 payment address:

```bash
playground $ CARDANO_NODE_SOCKET_PATH=node.socket cardano-cli query utxo \
  --address $(cat user-1-payment.addr) \
  $(cat magic.flag)
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8a3d63d4d95f669ef62570f2936ad50d2cfad399e04808ca21474e70b11987ee     0        97640000 lovelace
```

Save that data into environment variables for future use.  For example:

```bash
playground $ txhash=8a3d63d4d95f669ef62570f2936ad50d2cfad399e04808ca21474e70b11987ee
playground $ balance=97640000
```

Find out how much will remain after paying `1000000 lovelace` to the target account:

```bash
playground $ remaining=$(echo "$balance - 1000000 - 180000" | bc)
```

Build a raw transaction.  The era is selected by the command group, so use `latest` for the current
era, or name an era explicitly such as `conway`:

```bash
playground $ cardano-cli latest transaction build-raw \
  --tx-in "$txhash#0" \
  --tx-out "$(cat user-1-payment.addr)+$remaining" \
  --tx-out "$(cat user-2-payment.addr)+1000000" \
  --invalid-hereafter "21168607" \
  --fee 180000 \
  --out-file tx.raw
```

Sign the transaction:

```bash
playground $ cardano-cli latest transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file user-1-payment.skey \
  $(cat magic.flag) \
  --out-file tx.signed
```

Extract the CBOR binary from the signed transaction:

```bash
playground $ xxd -r -p <<< "$(jq -r .cborHex tx.signed)" > tx.signed.cbor
```

Submit the signed transaction using curl:

```bash
playground $ curl --header "Content-Type: application/cbor" -X POST http://localhost:8090/api/submit/tx --data-binary @tx.signed.cbor
"8a3d63d4d95f669ef62570f2936ad50d2cfad399e04808ca21474e70b11987ee"%
```

The string returned is the new transaction hash.

You can check your user 2 payment address has received the funds by querying the following:

```bash
playground $ CARDANO_NODE_SOCKET_PATH=node.socket cardano-cli query utxo \
  --address $(cat user-2-payment.addr) \
  $(cat magic.flag)
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
24e12cf8937db7fc95a39ca7780a5a1cb425ee53321d730254d661cc96be572f     1        1000000 lovelace
8a3d63d4d95f669ef62570f2936ad50d2cfad399e04808ca21474e70b11987ee     1        1000000 lovelace
```

Additionally, the `cardano-submit-api` will print out a record of the transaction on its `stdout`,
and increment the `tx_submit_counter` metric:

```
[2026-08-04 23:09:11.24Z][host:TxSubmitApi.Endpoint.SubmittedTransaction](Info,7) txSubmitPost: successfully submitted transaction 8a3d63d4d95f669e
```

A failed submission traces `TxSubmitApi.Endpoint.FailedToSubmitTransaction` instead and increments
`tx_submit_fail_counter`.  Both traces are `Info` severity, so a tracing config with a root severity
above `Info` suppresses them.

### Error case tests

`test/run.sh` is a rough smoke check of the endpoint's rejection paths, run against a submit-api
already listening on `http://localhost:8090`:

```bash
cardano-node $ ./cardano-submit-api/test/run.sh
```

It posts a hex-encoded body, a base64 body, an empty body, a body with the wrong content type, and a
well-formed but invalid transaction, asserting on substrings of the responses.  Two caveats:

* The hex and base64 cases both just fail to deserialise, so they assert the same thing and do not
  really distinguish their inputs.
* The invalid transaction case expects `ApplyTxError`, which only comes back when a node is actually
  reachable on the socket; without one you get a connection error instead and that assertion fails.

Treat a clean run as a smoke signal rather than a conformance test.
