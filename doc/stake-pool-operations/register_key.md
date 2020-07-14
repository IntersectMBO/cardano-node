
# Register stake address in the blockchain

Before, we created our payment keys and address, which allow us to control our funds; we also created our stake keys and stake address, which allow us to participate in the protocol by delegating our stake or by creating a stake pool.  

We need to _register_ our __stake key__ in the blockchain. To achieve this, we:

* Create a registration certificate
* Get protocol parameters
* Determine the Time-to-Live (TTL)
* Calculate change to send back to origin address
* Build raw transaction
* Calculate the fees   
* Submit the certificate to the blockchain with a transaction

Make sure that your node is running and set CARDANO_NODE_SOCKET_PATH environment variable to the path to the node.socket

### Create a _registration certificate_:

    cardano-cli shelley stake-address registration-certificate \
    --stake-verification-key-file stake.vkey \
    --out-file stake.cert

Once the certificate has been created, we must include it in a transaction to post it to the blockchain.

### Get protocol parameters

Get the protocol parameters and save them to `protocol.json` with:

       cardano-cli shelley query protocol-parameters \
       --testnet-magic 42 \
       --out-file protocol.json

### Determine the TTL

We need the __current tip__ of the blockchain, this is, the height of the last slot. We are looking for the value of `unSlotNo`

    cardano-cli shelley query tip --testnet-magic 42

    > Tip (SlotNo {unSlotNo = 795346}) (ShelleyHash {unShelleyHash = HashHeader {unHashHeader =        6a28a1c9fac321d7f4c8df8de68ee17f1967695460b5b422c93e6faaeeaf5cf2}}) (BlockNo {unBlockNo = 33088})

So at this moment the tip is on block 795346.

To build the transaction we need to specify the __TTL (Time to live)__, this is the slot height limit for our transaction to be included in a block, if it is not in a block by that slot the transaction will be cancelled.

From `protocol.json` we know that we have 1 slot per second. Lets say that it will take us 10 minutes to build the transaction, and that we want to give it another 10 minutes window to be included in a block.  So we need 20 minutes or 1200 slots. So we add 1200 to the current tip: 795346 + 1200 = 796546. So our TTL is 796546.


### Build a raw transaction


**Get transaction hash and index**

We need the transaction hash and index of the __UTXO__ we want to spend:

    cardano-cli shelley query utxo \
        --address $(cat payment.addr) \
        --testnet-magic 42

    >                            TxHash                                 TxIx        Lovelace
    > ----------------------------------------------------------------------------------------
    > 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99     4     1000000000000


    cardano-cli shelley transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment.addr)+0 \
    --ttl 987654 \
    --fee 0 \
    --out-file tx.raw \
    --certificate-file stake.cert

### Calculate fees and deposit

This transaction has only 1 input (the UTXO used to pay the transaction fee) and 1 output (our payment.addr to which we are sending the change). This transaction has to be signed by both the payment signing key `payment.skey` and the stake signing key `stake.skey`; and includes the certificate `stake.cert`:

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.raw \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --testnet-magic 42 \
    --protocol-params-file protocol.json \
    --witness-count 1 \
    --byron-witness-count 0

    > 173377

**Caclulate the change**

In this transaction we have to not only pay transaction fees, but also include a _deposit_ (which we will get back when we deregister the key) as stated in the protocol parameters:

The deposit amount can be found in the `protocol.json` under `keyDeposit`:

        ...
        "keyDeposit": 400000,
        ...

Query the UTXO:

    cardano-cli shelley query utxo \
        --address $(cat payment.addr) \
        --testnet-magic 42

    >                            TxHash                                 TxIx        Lovelace
    > ----------------------------------------------------------------------------------------
    > 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99     4      1000000000


So we have 1000 ada, calculate the change to send back to `payment.addr`:

    expr 1000000000 - 173377  - 400000

    > 99999426623


### Rebuild transaction including fees and deposit

cardano-cli shelley transaction build-raw \
--tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
--tx-out $(cat payment.addr)+99999426623 \
--ttl 987654\
--fee 173377 \
--out-file tx.raw

### Submit the certificate with a transaction:

Sign it:

    cardano-cli shelley transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --signing-key-file stake.skey \
    --testnet-magic 42 \
    --out-file tx.signed

And submit it:

    cardano-cli shelley transaction submit \
    --tx-file tx.signed \
    --testnet-magic 42

Your stake key is now registered in the blockchain. To confirm, query the utxo again:

    cardano-cli shelley query utxo \
    --address $(cat payment.addr) \
    --testnet-magic 42

                          TxHash                                      TxIx        Lovelace
    ----------------------------------------------------------------------------------------
    4cb9baf37fb1a6320150b14fcbadc24daed2b4cb9baf0f9e48b352d720c5c99     0       99999426623  
