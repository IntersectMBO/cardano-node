# ステークアドレスをブロックチェーンに登録する

ステークアドレスを使用可能にするためにはブロックチェーンに登録する必要があります。鍵の登録には以下が必要です

* 登録証明書を作成する
* ブロックチェーンに証明書をトランザクションで送信する

#### 登録証明書を作成する

    cardano-cli shelley stake-address registration-certificate \
    --stake-verification-key-file stake.vkey \
    --out-file stake.cert

#### トランザクションのドラフトを作成する

トランザクションドラフトでは、–tx.out、–ttl、–feeはゼロに設定できます。

    cardano-cli shelley transaction build-raw \
    --tx-in b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee#1 \
    --tx-out $(cat payment.addr)+0 \
    --ttl 0 \
    --fee 0 \
    --out-file tx.raw \
    --certificate-file stake.cert

#### 手数料を計算する

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.raw \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

アウトプットはLavelace単位のトランザクション手数料です

    > 171485

ステークアドレスの登録には、トランザクション手数料の支払いだけでなく、プロトコルパラメーターの章で触れたデポジット（鍵の登録解除時に返金）が発生します。

デポジット額は`keyDeposit`の`protocol.json`にあります。以下はShelleyテストネットの場合です

    ...
    "keyDeposit": 2000000,
    ...

トランザクション手数料とデポジットを支払うのUTXOのアドレスを問い合わせます

    cardano-cli shelley query utxo \
        --address $(cat payment.addr) \
        --mainnet

    >                            TxHash                                 TxIx        Lovelace
    > ----------------------------------------------------------------------------------------
    > b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     1      1000000000

#### デポジットを含めたうえで支払いアドレスに返金するつり銭を計算する

    expr 1000000000 - 171485 - 2000000

    > 999428515

#### 証明書をトランザクションで送信する

トランザクションを構築します。今回は–ttlと–feeを含めます  

    cardano-cli shelley transaction build-raw \
    --tx-in b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee#1 \
    --tx-out $(cat payment.addr)+997828515 \
    --ttl 987654 \
    --fee 171485 \
    --out-file tx.raw \
    --certificate-file stake.cert

署名します

    cardano-cli shelley transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --signing-key-file stake.skey \
    --mainnet \
    --out-file tx.signed

送信します

    cardano-cli shelley transaction submit \
    --tx-file tx.signed \
    --mainnet

ステーク鍵がブロックチェーンに登録されました。

**注意：**`--mainnet`はCardanoメインネットを特定するものです。テストネットの場合は`--testnet-magic 1097911063`を使用してください。
