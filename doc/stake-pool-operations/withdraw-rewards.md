## Withdrawing rewards


### Check the balance of the rewards address:

    cardano-cli shelley query stake-address-info \
    --mainnet \
    --address $(cat stake.addr)

    [
        {
            "address": "stake_test1urxx5h9mhey1234566ek6uxqgtlkpvteqjqgaleddevpggfggxw",
            "delegation": "pool1ljq292q5xwz8t3ehmaw3c5p9xpr123456dnrxd40004ns6dy8v0",
            "rewardAccountBalance": 550000000
        }
    ]


### Query the payment address balance

You'll withdraw rewards into a payment.addr wich will pay for the transaction fees.

    cardano-cli shelley query utxo --mainnet --address $(cat payment.addr)

                               TxHash                                 TxIx        Lovelace
    ----------------------------------------------------------------------------------------
    a82f8d2a85cde39118a894306ad7a85ba40af221406064a56bdd9b3c61153527     1         194054070



### Draft the withdraw transaction to transfer the rewards to a payment.addr

    cardano-cli shelley transaction build raw \
    --tx-in a82f8d2a85cde39118a894306ad7a85ba40af221406064a56bdd9b3c61153527#1
    --tx-out $(cat payment.addr)+0
    --withdrawal $(cat stake.addr)+550000000
    --ttl 0
    --fee 0
    --out-file withdraw_rewards.raw

### Calculate transaction fees

    cardano-cli shelley transaction calculate-min-fee \
    --mainnet \
    --tx-body-file withdraw_rewards.raw  \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --protocol-params-file protocol.json

   > 171089

### Build the raw transaction.

    expr 194054070 - 171089
    193882981

    cardano-cli shelley transaction build raw \
    --tx-in $(cat payment.addr)#1
    --tx-out $(cat payment.addr)+193882981
    --withdrawal $(cat stake.addr)+550000000
    --ttl 12345678
    --fee 171089
    --out-file withdraw_rewards.raw    

### Sign and submit the transactions

    cardano-cli shelley transaction sign \
    --mainnet \
    --tx-body-file withdraw_rewards.raw  \
    --signing-key-file payment.skey \
    --signing-key-file stake.skey \
    --out-file withdraw_rewards.signed

    cardano-cli shelley transaction submit \
    --mainnet \
    --tx-file withdraw_rewards.signed
