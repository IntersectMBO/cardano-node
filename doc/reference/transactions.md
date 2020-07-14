# Create a simple transaction

Creating a transaction is a process that requires various steps:

* Get the protocol parameters
* Determine the time-to-live (TTL) for the transaction
* Get origin utxo's balance and calculate the change to send back to origin address
* Build the transaction
* Calculate the fee
* Rebuild the transaction including the fees
* Sign the transaction
* Submit the transaction

Make sure that your node is running and set CARDANO_NODE_SOCKET_PATH variable to:

    export CARDANO_NODE_SOCKET_PATH=~/cardano-node/relay/db/node.socket

### Get protocol parameters

Get the protocol parameters and save them to `protocol.json` with:

    cardano-cli shelley query protocol-parameters \
    --testnet-magic 42 \
    --out-file protocol.json

The `--testnet-magic 42` identifies the Shelley Testnet. Other testnets will use other numbers, and mainnet uses `--mainnet` instead.

    cat protocol.json
    {
    "poolDeposit": 500000000,
    "protocolVersion": {
        "minor": 0,
        "major": 0
    },
    "minUTxOValue": 0,
    "decentralisationParam": 1,
    "maxTxSize": 16384,
    "minPoolCost": 0,
    "minFeeA": 44,
    "maxBlockBodySize": 65536,
    "minFeeB": 155381,
    "eMax": 18,
    "extraEntropy": {
        "tag": "NeutralNonce"
    },
    "maxBlockHeaderSize": 1100,
    "keyDeposit": 400000,
    "nOpt": 250,
    "rho": 2.2e-3,
    "tau": 5.0e-2,
    "a0": 0.3
    }

### Determine the TTL (time to Live) for the transaction

First, get the __current tip__ of the blockchain, this is, the height of the last slot. We are looking for the value of `unSlotNo`

    cardano-cli shelley query tip --testnet-magic 42

    > Tip (SlotNo {unSlotNo = 123456}) (ShelleyHash {unShelleyHash = HashHeader {unHashHeader =        6a28a1c9fac321d7f4c8df8de68ee17f1967695460b5b422c93e6faaeeaf5cf2}}) (BlockNo {unBlockNo = 23456})

So at this moment the tip is on block 123456

To build the transaction we need to specify the __TTL (Time to live)__, this is the slot height limit for our transaction to be included in a block, if it is not in a block by that slot the transaction will be cancelled.

### Get origin utxo balance and calculate the change to send back to origin address

From `protocol.json` we know that we have 1 slot per second. Lets say that it will take us 5 minutes to build the transaction, and that we want to give it another 5 minutes window to be included in a block.  So we need 10 minutes or 600 slots. So we add 600 to the current tip:  123456 + 600 = 124056. So our TTL is 124056.

We need the __transaction hash__ and __index__ of the UTXO we want to spend:

  cardano-cli shelley query utxo \
      --address $(cat payment.addr) \
      --testnet-magic 42

  >                            TxHash                                 TxIx        Lovelace
  > ----------------------------------------------------------------------------------------
  > 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99     4     1000000000


  Now we need to calculate how much is the __change__ to send back to __payment.addr__

  Assuming we want to send 100 ADA to __payment2.addr__ spending a __UTXO__ containing 1000 ada (1,000,000,000 lovelace),

    expr 1000000000 - 100000000 - 167965

    > 899832035

### Build the transaction

We write the transaction in a file, name it `tx.raw`.

Note that for `--tx-in` we use the following syntax: `TxId#TxIx` where `TxId` is the transaction hash and `TxIx` is the index
and for `--tx-out` we use: `TxOut+Lovelace` where `TxOut` is the hex encoded address followed by the amount in `Lovelace`.

    cardano-cli shelley transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+100000000 \
    --tx-out $(cat payment.addr)+899832035 \
    --ttl 1234567 \
    --fee 0 \
    --out-file tx.raw

### Calculate the fee
A simple transaction needs one (1) input: a valid UTXO from __payment.addr__, and two (2) outputs: The receiving address __payment2.addr__ and an address to send the change back, __payment.addr__.

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.raw \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --testnet-magic 42 \
    --protocol-params-file protocol.json

     > 167965

So we need to pay __167965 lovelace__ fee to create this transaction.

### Rebuild the transaction including the fees

    cardano-cli shelley transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+100000000 \
    --tx-out $(cat payment.addr)+999899832035 \
    --ttl 1234567 \
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
