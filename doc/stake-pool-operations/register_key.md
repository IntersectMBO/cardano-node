# Register stake address in the blockchain

Stake address needs to be registered in the blockchain to be useful. Registering keys requires:

* Create a registration certificate.
* Submit the certificate to the blockchain with a transaction.

#### Create a _registration certificate_:

    cardano-cli shelley stake-address registration-certificate \
    --stake-verification-key-file stake.vkey \
    --out-file stake.cert

#### Draft transaction

For the transaction draft, --tx.out, --ttl and --fee can be set to zero.

    cardano-cli shelley transaction build-raw \
    --tx-in b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee#1 \
    --tx-out $(cat paymentwithstake.addr)+0 \
    --ttl 0 \
    --fee 0 \
    --out-file tx.raw \
    --certificate-file stake.cert

#### Calculate fees

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.raw \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --testnet-magic 42 \
    --protocol-params-file protocol.json

The output is the transaction fee in lovelace:

    > 171485

Registering the stake address, not only pay transaction fees, but also includes a _deposit_ (which you get back when deregister the key) as stated in the protocol parameters:

The deposit amount can be found in the `protocol.json` under `keyDeposit`, for example in Shelley Tesntet:

    ...
    "keyDeposit": 400000,
    ...

Query the UTXO of the address that pays for the transaction and deposit:

    cardano-cli shelley query utxo \
        --address $(cat payment.addr) \
        --testnet-magic 42

    >                            TxHash                                 TxIx        Lovelace
    > ----------------------------------------------------------------------------------------
    > b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     1      1000000000

#### Calculate the change to send back to payment address after including the deposit

    expr 1000000000 - 171485 - 400000

    > 999428515

#### Submit the certificate with a transaction:

Build the transaction, this time include  --ttl and --fee  

    cardano-cli shelley transaction build-raw \
    --tx-in b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee#1 \
    --tx-out $(cat paymentwithstake.addr)+999428515 \
    --ttl 987654 \
    --fee 171485 \
    --out-file tx.raw \
    --certificate-file stake.cert

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

Your stake key is now registered in the blockchain.

**Note. `--testnet magic 42` identifies the testnets, for mainnet, use `--mainnet` instead.
