# Create a simple transaction

You will need a new __payment address__ (payment2.addr) so that you can send funds to it. First, get a new payment key pair for that:

    cardano-cli shelley address key-gen \
    --verification-key-file payment2.vkey \
    --signing-key-file payment2.skey

This has created two new keys: __payment2.vkey__ and __payment2.skey__

To generate __payment2.addr__ we can use the same stake key pair that we already have:

    cardano-cli shelley address build \
    --payment-verification-key-file payment2.vkey \
    --stake-verification-key-file stake.vkey \
    --out-file payment2.addr \
    --testnet-magic 42

Let's send 100 ada from `payment.addr` to `payment2.addr`

Creating a transaction is a process that requires various steps:

* Get the protocol parameters
* Define the time-to-live (TTL) for the transaction
* Calculate the fee
* Build the transaction
* Sign the transaction
* Submit the transaction

### Get protocol parameters
Get the protocol parameters and save them to `protocol.json` with:

       cardano-cli shelley query protocol-parameters \
       --testnet-magic 42 \
       --out-file protocol.json

### Define the TTL (time to Live) for the transaction

We need the CURRENT TIP of the blockchain, this is, the height of the last block produced. We are looking for the value of `unSlotNo`

    cardano-cli shelley query tip --testnet-magic 42

    > Tip (SlotNo {unSlotNo = 795346}) (ShelleyHash {unShelleyHash = HashHeader {unHashHeader =        6a28a1c9fac321d7f4c8df8de68ee17f1967695460b5b422c93e6faaeeaf5cf2}}) (BlockNo {unBlockNo = 33088})

So at this moment the tip is on block 795346.

To build the transaction we need to specify the __TTL (Time to live)__, this is the block height limit for our transaction to be included in a block, if it is not in a block by that slot the transaction will be cancelled.

From `protocol.json` we know that we have 1 slot per second. Lets say that it will take us 10 minutes to build the transaction, and that we want to give it another 10 minutes window to be included in a block.  So we need 20 minutes or 1200 slots. So we add 1200 to the current tip: 795346 + 1200 = 796546. So our TTL is 796546.

### Calculate the fee
The transaction needs one (1) input: a valid UTXO from `payment.addr`, and two (2) outputs: The receiving address __payment2.addr__ and an address to send the change back, in this case we use __payment.addr__

       cardano-cli shelley transaction calculate-min-fee \
       --tx-in-count 1 \
       --tx-out-count 2 \
       --ttl 796546 \
       --testnet-magic 42 \
       --signing-key-file payment.skey \
       --protocol-params-file protocol.json

       > 167965

(The `--testnet-magic 42` identifies the Shelley Testnet. Other testnets will use other numbers, and mainnet uses `--mainnet` instead.)

So we need to pay __167965 lovelace__ fee to create this transaction.

Assuming we want to send 100 ADA to __payment2.addr__  spending a __UTXO__ containing 1,000,000 ada (1,000,000,000,000 lovelace), now we need to calculate how much is the __change__ to send back to __payment.addr__

    expr 1000000000000 - 100000000 - 167965

    > 999899832035

We need the transaction hash and index of the __UTXO__ we want to spend:

    cardano-cli shelley query utxo \
        --address $(cat payment.addr) \
        --testnet-magic 42

    >                            TxHash                                 TxIx        Lovelace
    > ----------------------------------------------------------------------------------------
    > 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99     4     1000000000000


### Build the transaction

We write the transaction in a file, we will name it `tx.raw`.

Note that for `--tx-in` we use the following syntax: `TxId#TxIx` where `TxId` is the transaction hash and `TxIx` is the index
and for `--tx-out` we use: `TxOut+Lovelace` where `TxOut` is the hex encoded address followed by the amount in `Lovelace`.

    cardano-cli shelley transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+100000000 \
    --tx-out $(cat payment.addr)+999899832035 \
    --ttl 796546 \
    --fee 167965 \
    --out-file tx.raw

### Sign the transaction
Sign the transaction with the signing key __payment.skey__ and save the signed transaction in __tx.signed__

    cardano-cli shelley transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --testnet-magic 42 \
    --out-file tx.signed

### Submit the transaction
Make sure that your node is running and set CARDANO_NODE_SOCKET_PATH variable to:

    export CARDANO_NODE_SOCKET_PATH=~/cardano-node/relay/db/node.socket

and submit the transaction:

    cardano-cli shelley transaction submit \
            --tx-file tx.signed \
            --testnet-magic 42

### Check the balances

We must give it some time to get incorporated into the blockchain, but eventually, we will see the effect:

        cardano-cli shelley query utxo \
            --address $(cat payment.addr) \
            --testnet-magic 42

        >                            TxHash                                 TxIx        Lovelace
        > ----------------------------------------------------------------------------------------
        > b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     1      999899832035

        cardano-cli shelley query utxo \
            --address $(cat payment2.addr) \
            --testnet-magic 42

        >                            TxHash                                 TxIx        Lovelace
        > ----------------------------------------------------------------------------------------
        > b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     0         100000000
