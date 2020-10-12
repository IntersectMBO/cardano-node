# シンプルなトランザクションを作成する

トランザクションを作成するにはさまざまなステップを踏む必要があります

* プロトコルパラメーターを入手する
* 手数料を計算する
* トランザクションのTTL（time-to-live）を定義する
* トランザクションを構築する
* トランザクションに署名する
* トランザクションを送信する

#### プロトコルパラメーター入手する

プロトコルパラメーターを入手して`protocol.json`に保存します

```
cardano-cli shelley query protocol-parameters \
  --mainnet \
  --out-file protocol.json
```

#### トランザクションハッシュと使用する**UTXO**のインデックスを入手する

```
cardano-cli shelley query utxo \
  --address $(cat payment.addr) \
  --mainnet
```

```
                            TxHash                                 TxIx        Lovelace
----------------------------------------------------------------------------------------
4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99     4         20000000
```

#### トランザクションのドラフトを作成する

トランザクションのドラフトを作成してtx.draftに保存します

`--tx-in`には`TxId#TxIx`のシンタックスを使用します。`TxId`がトランザクションハッシュで`TxIx`がインデックスです。`--tx-out`には`TxOut+Lovelace`を使用します。`TxOut`はHEXエンコードアドレスで、`Lovelace`単位の額が続きます。トランザクションドラフトでは–tx-out、–ttl、–feeはゼロに設定できます。

    cardano-cli shelley transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+0 \
    --tx-out $(cat payment.addr)+0 \
    --ttl 0 \
    --fee 0 \
    --out-file tx.draft

#### 手数料を計算する

シンプルなトランザクションには、`payment.addr`から有効なUTXOアウトプットが1つ、さらに次の2つのアウトプットが必要です

* Output1：トランザクションを入金するアドレス
* Output2：トランザクションのつり銭を入金するアドレス（新しいUTXO）

手数料を計算するには、ドラフトトランザクションに含める必要があります

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

    > 167965

#### payment.addrに返金するつり銭を計算する
全額Lovelace単位にする必要があります

    expr <UTXO BALANCE> - <AMOUNT TO SEND> - <TRANSACTION FEE>

例えば、20ADAを含んでいるUTXOから10ADAを送金した場合、手数料を支払った後で`payment.addr`に返金されるのは9.832035 ADAになります  

    expr 20000000 - 10000000 - 167965

    > 9832035

#### トランザクションのTTL（time to Live）を設定する

トランザクションを構築するためには、**TTL（Time to live）** を特定する必要があります。これは、トランザクションがブロックに含まれるためのスロットの高さの上限です。そのスロットでブロックに含まれない場合、トランザクションはキャンセルされます。従って、TTL = slotNo + N slotsとなります。Nはトランザクションをブロックに含められるよう幅を持たせるために追加するスロットの量です。

ブロックチェーンのチップを問い合わせます

    cardano-cli shelley query tip --mainnet

`unSlotNo`の値を探します

    {
        "blockNo": 16829,
        "headerHash": "3e6f59b10d605e7f59ba8383cb0ddcd42480ddcc0a85d41bad1e4648eb5465ad",
        "slotNo": 369200
    }

TTLの計算例：369215 + 200スロット = 369400

#### トランザクションを構築する

トランザクションをファイルに書き込みます。ファイル名を`tx.raw`とします。

    cardano-cli shelley transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+10000000 \
    --tx-out $(cat payment.addr)+9832035 \
    --ttl 369400 \
    --fee 167965 \
    --out-file tx.raw

#### トランザクションに署名する

トランザクションに署名鍵**payment.skey**で署名し、署名したトランザクションを**tx.signed**に保存します

    cardano-cli shelley transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --mainnet \
    --out-file tx.signed

#### トランザクションを送信する

    cardano-cli shelley transaction submit \
    --tx-file tx.signed \
    --mainnet

#### 残高を確認する

ブロックチェーンに統合されるまでしばらくかかりますが、やがて結果が現れます

    cardano-cli shelley query utxo \
        --address $(cat payment.addr) \
        --mainnet

    >                            TxHash                                 TxIx        Lovelace
    > ----------------------------------------------------------------------------------------
    > b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     1         9832035

    cardano-cli shelley query utxo \
        --address $(cat payment2.addr) \
        --mainnet

    >                            TxHash                                 TxIx        Lovelace
    > ----------------------------------------------------------------------------------------
    > b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     0         10000000


**注意：**`--mainnet`はCardanoメインネットを特定するものです。テストネットの場合は`--testnet-magic 1097911063`を使用してください。 
