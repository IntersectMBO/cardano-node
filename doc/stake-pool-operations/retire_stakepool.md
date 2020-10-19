# ステークプールを終了する

ステークプールを終了するには以下が必要です

* **登録抹消証明書** を作成する
* ブロックチェーンに証明書を **トランザクション** で送信する

登録抹消証明書にはプールを終了するエポックが含まれます。このエポックは現行のエポックの後であり、`eMax`エポックよりも前である必要があります。`eMax`はプロトコルパラメーターです。

まず、現行のエポックを確認します。ジェネシスファイルに記録されているエポックごとのスロット数を確認します

    cat mainnet-shelley-genesis.json | grep epoch
    > "epochLength": 21600,

エポックのスロット数は21600です。チップを問い合わせることで現行のスロットを割り出します

    export CARDANO_NODE_SOCKET_PATH=relay-db/node-socket
    cardano-cli shelley query tip --mainnet

    > Tip (SlotNo {unSlotNo = 856232}) ...

これで以下が得られます

    expr 856232 / 21600
    > 39

現在エポック39であることがわかりました。

現在のプロトコルパラメーターを問い合わせることにより`eMax`をルックアップできます

    cardano-cli shelley query protocol-parameters \
    --mainnet \
    --out-file protocol.json

    cat protocol.json | grep eMax
    > "eMax": 100,

ここから、終了可能なエポックは最速で40（次）から遅くとも139まで（現行のエポック+`eMax`）となります。 

したがって、例えば、エポック41で終了すると決定することができます。

#### 登録抹消証明書を作成する

**警告：** これには __コールドキー__ が必要です。コールドキーをインターネットに晒さないように十分に注意してください。

登録抹消証明書を作成して、`pool.deregistration`として保存します

    cardano-cli shelley stake-pool deregistration-certificate \
    --cold-verification-key-file cold.vkey \
    --epoch 41 \
    --out-file pool.deregistration

#### トランザクションのドラフトを作成する

    cardano-cli shelley transaction build-raw \
    --tx-in <UTXO>#<TxIx> \
    --tx-out $(cat payment.addr)+0 \
    --ttl 0 \
    --fee 0 \
    --out-file tx.draft \
    --certificate-file pool.deregistration

#### 手数料を計算する

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

例

    > 171309

インプット用に適切なUTXOのアドレスを問い合わせます

    cardano-cli shelley query utxo \
    --address $(cat payment.addr) \
    --mainnet



           TxHash             TxIx        Lovelace
    ------------------------------------------------
    9db6cf...                    0      999999267766

つり銭を計算します

    expr 999999267766 - 171309
    > 999999096457

#### トランザクションを構築し、署名して送信する

rawトランザクションを構築します

    cardano-cli shelley transaction build-raw \
    --tx-in 9db6cf...#0 \
    --tx-out $(cat payment.addr)+999999096457 \
    --ttl 860000 \
    --fee 171309 \
    --out-file tx.raw \
    --certificate-file pool.deregistration

**支払い署名鍵とコールド署名鍵の両方で署名します（支払い署名鍵は`paymant.addr`
の資金を使用するため、コールド署名鍵は証明書にプールオーナーの署名が必要なため）**

    cardano-cli shelley transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --signing-key-file cold.skey \
    --mainnet \
    --out-file tx.signed

ブロックチェーンに送信します

    cardano-cli shelley transaction submit \
    --tx-file tx.signed \
    --mainnet

プールはエポック40の終わりに終了します。

変更する場合は、エポック41の前に新しい登録証明書を送信すると、登録抹消証明書に優先されます。
