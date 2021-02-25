# Retiring a Stake Pool

To retire a pool we need to:

* Create a **deregistration certificate** and
* Submit the certificate to the blockchain with a **transaction**

The deregistration certificate contains the _epoch_ in which we want to retire the pool. This epoch must be _after_ the current epoch and _not later than_ `eMax` epochs in the future, where `eMax` is a protocol parameter.

So we first need to figure out the current epoch. The number of _slots per epoch_ is recorded in the genesis file, and we can get it with

    cat mainnet-shelley-genesis.json | grep epoch
    > "epochLength": 432000,

So one epoch lasts for 432000 slots. We get the current slot by querying the tip:

    export CARDANO_NODE_SOCKET_PATH=relay-db/node-socket
    cardano-cli query tip --mainnet

    {
        "blockNo": 5361172,
        "headerHash": "cdd3d9075d22a52f0a8e5d92a6c76e6982a2a1036fd4ce8732f70d89bb778a67",
        "slotNo": 22141376
    }


This gives us

    expr 22141376 / 432000
    > 51

So we are currently in epoch 51.

We can look up `eMax` by querying the current protocol parameters:

    cardano-cli query protocol-parameters \
    --mainnet \
    --allegra-era \
    --out-file protocol.json

    cat protocol.json | grep eMax
    > "eMax": 18,

This means the earliest epoch for retirement is 52 (one in the future), and the latest is 69 (current epoch plus `eMax`).

So for example, we can decide to retire in epoch 53.

#### Create deregistration certificate

**WARNING** This involves the __cold keys__. Take the necessary precautions to not expose your cold keys to the internet.

Create the deregistration certificate and save it as `pool-deregistration.cert`:

    cardano-cli stake-pool deregistration-certificate \
    --cold-verification-key-file cold.vkey \
    --epoch 53 \
    --out-file pool-deregistration.cert

#### Draft the transaction

    cardano-cli transaction build-raw \
    --tx-in <TxHash>#<TxIx> \
    --tx-out $(cat payment.addr)+0 \
    --invalid-hereafter 0 \
    --fee 0 \
    --out-file tx.draft \
    --certificate-file pool-deregistration.cert

#### Calculate the fees:

    cardano-cli transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 2 \
    --byron-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

For example:

    > 171309

We query our address for a suitable UTxO to use as input:

    cardano-cli query utxo \
    --address $(cat payment.addr) \
    --mainnet \
    --allegra-era



           TxHash             TxIx      Amount
    ------------------------------------------------
    9db6cf...                    0      999999267766 lovelace

We calculate our change:

    expr 999999267766 - 171309
    > 999999096457

#### Build, sign and submit the transaction:

Build the raw transaction:

    cardano-cli transaction build-raw \
    --tx-in 9db6cf...#0 \
    --tx-out $(cat payment.addr)+999999096457 \
    --invalid-hereafter 860000 \
    --fee 171309 \
    --out-file tx.raw \
    --certificate-file pool-deregistration.cert

**Sign it with both the payment signing key and the cold signing key
(the first signature is necessary because we are spending funds from `paymant.addr`,
the second because the certificate needs to be signed by the pool owner):**

    cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --signing-key-file cold.skey \
    --mainnet \
    --out-file tx.signed

And submit to the blockchain:

    cardano-cli transaction submit \
    --tx-file tx.signed \
    --mainnet

The pool will retire at the end of epoch 52.

If we change our mind, we can create and submit a new registration certificate before epoch 53, which will then overrule the deregistration certificate.
