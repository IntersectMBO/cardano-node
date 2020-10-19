## 報酬を引き落とす


### 報酬アドレスの残高を確認する

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


### 支払いアドレスの残高を問い合わせる

トランザクション手数料を支払うpayment.addrに報酬を引き落とします

    cardano-cli shelley query utxo --mainnet --address $(cat payment.addr)

                               TxHash                                 TxIx        Lovelace
    ----------------------------------------------------------------------------------------
    a82f8d2a85cde39118a894306ad7a85ba40af221406064a56bdd9b3c61153527     1         194054070



### 報酬をpayment.addrに移すための引き落としトランザクションのドラフトを作成する

    cardano-cli shelley transaction build raw \
    --tx-in a82f8d2a85cde39118a894306ad7a85ba40af221406064a56bdd9b3c61153527#1
    --tx-out $(cat payment.addr)+0
    --withdrawal $(cat stake.addr)+550000000
    --ttl 0
    --fee 0
    --out-file withdraw_rewards.raw

### トランザクション手数料を計算する

    cardano-cli shelley transaction calculate-min-fee \
    --mainnet \
    --tx-body-file withdraw_rewards.raw  \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --protocol-params-file protocol.json

   > 171089

### rawトランザクションを構築する

    expr 194054070 - 171089
    193882981

    cardano-cli shelley transaction build raw \
    --tx-in $(cat payment.addr)#1
    --tx-out $(cat payment.addr)+193882981
    --withdrawal $(cat stake.addr)+550000000
    --ttl 12345678
    --fee 171089
    --out-file withdraw_rewards.raw    

### トランザクションに署名して送信する

    cardano-cli shelley transaction sign \
    --mainnet \
    --tx-body-file withdraw_rewards.raw  \
    --signing-key-file payment.skey \
    --signing-key-file stake.skey \
    --out-file withdraw_rewards.signed

    cardano-cli shelley transaction submit \
    --mainnet \
    --tx-file withdraw_rewards.signed
