# Retiring a Stake Pool


We assume that we have a registered stake pool with cold keys `cold.vkey` and `cold.skey`, a payment address with sufficient funds
(for the transaction fees) `payment.addr` and an associated payment signing key `payment.skey`.

To retire our pool we need to:

* Create a _deregistration certificate_ and
* Submit the certificate to the blockchain with a transaction

The deregistration certificate contains the _epoch_ in which we want to retire the pool. This epoch must be _after_ the current epoch and _not later than_ `eMax` epochs in the future, where `eMax` is a protocol parameter.

So we first need to figure out the current epoch. The number of _slots per epoch_ is recorded in the genesis file, and we can get it with

    cat shelley_testnet-genesis.json | grep epoch
    > "epochLength": 21600,

So one epoch lasts for 21600 slots. We get the current slot by querying the tip:

    export CARDANO_NODE_SOCKET_PATH=relay-db/node-socket
    cardano-cli shelley query tip --testnet-magic 42

    > Tip (SlotNo {unSlotNo = 856232}) ...

This gives us

    expr 856232 / 21600
    > 39

So we are currently in epoch 39.

We can look up `eMax` by querying the current protocol parameters:

    cardano-cli shelley query protocol-parameters \
    --testnet-magic 42 \
    --out-file protocol.json

    cat protocol.json | grep eMax
    > "eMax": 100,

This means the earliest epoch for retirement is 40 (one in the future), and the latest is 139 (current epoch plus `eMax`).  

So for example, we can decide to retire in epoch 41.

#### Create deregistration certificate

**WARNING** This involves the __cold keys__. Take the necessary precautions to not to expose your cold keys to the internet.

Create the deregistration certificate and save it as `pool.deregistration`:

    cardano-cli shelley stake-pool deregistration-certificate \
    --cold-verification-key-file cold.vkey \
    --epoch 41 \
    --out-file pool.deregistration

#### Draft the transaction

    cardano-cli shelley transaction build-raw \
    --tx-in <UTXO>#<TxIx> \
    --tx-out $(cat payment.addr)+0 \
    --ttl 0 \
    --fee 0 \
    --out-file tx.draft \
    --certificate-file pool.deregistration

#### Calculate the fees:

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --testnet-magic 42 \
    --protocol-params-file protocol.json

For example:

    > 171309

We query our address for a suitable UTxO to use as input:

    cardano-cli shelley query utxo \
    --address $(cat payment.addr) \
    --testnet-magic 42



           TxHash             TxIx        Lovelace
    ------------------------------------------------
    9db6cf...                    0      999999267766

We calculate our change:

    expr 999999267766 - 171309
    > 999999096457

#### Build, sign and submit the transaction:

Build the raw transaction:

    cardano-cli shelley transaction build-raw \
        --tx-in 9db6cf...#0 \
        --tx-out $(cat payment.addr)+999999096457 \
        --ttl 860000 \
        --fee 171309 \
        --out-file tx.raw \
        --certificate-file pool.deregistration

**Sign it with both the payment signing key and the cold signing key
(the first signature is necessary because we are spending funds from `paymant.addr`,
the second because the certificate needs to be signed by the pool owner):**

    cardano-cli shelley transaction sign \
        --tx-body-file tx.raw \
        --signing-key-file payment.skey \
        --signing-key-file cold.skey \
        --testnet-magic 42 \
        --out-file tx.signed

And submit to the blockchain:

    cardano-cli shelley transaction submit \
        --tx-file tx.signed \
        --testnet-magic 42

The pool will retire at the end of epoch 40.

If we change our mind, we can create and submit a new registration certificate before epoch 41, which will then overrule the deregistration certificate.
