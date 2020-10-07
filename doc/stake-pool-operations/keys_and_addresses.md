# 鍵とアドレスを生成する

Cardano Shelley期には、すべてのステークホルダーは2セットの鍵とアドレスを持ちます

* 支払い鍵とアドレス：トランザクションの送受信を行う
* ステーク鍵とアドレス：プロトコルへの参加、ステークプールの作成、委任、報酬の受け取りをコントロールする

#### 支払い鍵ペア

支払い鍵ペアを生成します

```
cardano-cli shelley address key-gen \
--verification-key-file payment.vkey \
--signing-key-file payment.skey
```
2つのファイル（`payment.vkey`および`payment.skey`）が作成されます。1つには公開検証鍵もう1つには秘密署名鍵が入っています。

#### レガシーキー

Byron期の支払い鍵を生成します

支払い鍵ファイルは以下のフォーマットを使用します
```json
{
    "type": "PaymentSigningKeyByron_ed25519_bip32",
    "description": "Payment Signing Key",
    "cborHex": "hex-here"
}
```

`hex-here`が`0x5880 | xprv | pub | chaincode`として生成されます

#### ステーク鍵ペア
ステーク鍵ペアを生成します

```
cardano-cli shelley stake-address key-gen \
--verification-key-file stake.vkey \
--signing-key-file stake.skey
```
#### 支払いアドレス
`payment.vkey`と`stake.vkey`の両検証鍵を含んでアドレスを構築する際、生成された`payment address`はこのステーク鍵とアドレスに関連付けられています

```
cardano-cli shelley address build \
--payment-verification-key-file payment.vkey \
--stake-verification-key-file stake.vkey \
--out-file payment.addr \
--mainnet
```
#### ステークアドレス

`stake address`を生成します

```
cardano-cli shelley stake-address build \
--stake-verification-key-file stake.vkey \
--out-file stake.addr \
--mainnet
```
このアドレスは、支払いを受信することは __できません__ が、プロトコルに参加することによって得られる報酬を受け取ることができます。


#### アドレスの残高照会

> 注：自分のノードが現在のBlock Height（ブロックの高さ）と同期していることを確認してください。これは、 [explorer.cardano.org](https://explorer.cardano.org)で確認できます。同期していない場合、Byron期を参照するエラーが生じる場合があります。

任意のアドレスの残高を照会するためには、ノードを実行し、環境変数`CARDANO_NODE_SOCKET_PATH`をnode.socketへのパスに設定します

```
cardano-cli shelley query utxo \
--address $(cat payment.addr) \
--mainnet
```
```
                            TxHash                                 TxIx        Lovelace
--------------------------------------------------------------------------------------------
```

**注意：**`--mainnet`はCardanoメインネットを特定するものです。テストネットの場合は`--testnet-magic 1097911063`を使用してください。 
