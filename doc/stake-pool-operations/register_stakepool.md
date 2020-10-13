# メタデータ付きステークプールを登録する

次にアクセスできることを確認してください

| ファイル | コンテンツ |
| :--- | :--- |
| `payment.vkey` | 支払い検証鍵 |
| `payment.skey` | 支払い署名鍵 |
| `stake.vkey` | ステーキング検証鍵 |
| `stake.skey` | ステーキング署名鍵 |
| `stake.addr` | 登録済みステークアドレス |
| `payment.addr` | `stake`にリンクされた資金入りアドレス |
| `cold.vkey` | コールド検証鍵 |
| `cold.skey` | コールド署名鍵 |
| `cold.counter` | 発行番号 |
| `node.cert` | 運営証明書 |
| `kes.vkey` | KES検証鍵 |
| `kes.skey` | KES署名鍵 |
| `vrf.vkey` | VRF検証鍵 |
| `vrf.skey` | VRF署名鍵 |

ステークプールの登録には以下が必要です

* メタデータのJSONファイルを作成し、自分の管理するノードとURLに保存する
* JSONファイルのハッシュを取得する
* ステークプール登録証明書を生成する
* 出資の委任証明書を作成する
* 証明書をブロックチェーンに送信する

**警告：** **ステークプール登録証明書** と **委任証明書** を生成するには **コールドキー** が必要です。したがって、これをメインネットで行う場合、インターネットにコールドキーを晒すリスクを避けるための適切な安全策として、これらの証明書はローカルマシンで作成するという方法もあります。

#### プールのメタデータが入ったJSONファイルを作成する

      {
      "name": "TestPool",
      "description": "The pool that tests all the pools",
      "ticker": "TEST",
      "homepage": "https://teststakepool.com"
      }


自分が管理するURLにファイルを保存します。例： [https://teststakepool.com/poolMetadata.json](https://git.io/JJWdJ) githubではGISTを使用できます。URLは65文字以下に設定してください。

#### メタデータJSONファイルのハッシュを取得する

JSONがスキーマ要件を満たしているか検証し、満たしている場合はファイルのハッシュを取得します

    cardano-cli shelley stake-pool metadata-hash --pool-metadata-file pool_Metadata.json

    >6bf124f217d0e5a0a8adb1dbd8540e1334280d49ab861127868339f43b3948af


#### ステークプール登録証明書を生成する

    cardano-cli shelley stake-pool registration-certificate \
    --cold-verification-key-file cold.vkey \
    --vrf-verification-key-file vrf.vkey \
    --pool-pledge <AMOUNT TO PLEDGE IN LOVELACE> \
    --pool-cost <POOL COST PER EPOCH IN LOVELACE> \
    --pool-margin <POOL COST PER EPOCH IN PERCENTAGE> \
    --pool-reward-account-verification-key-file stake.vkey \
    --pool-owner-stake-verification-key-file stake.vkey \
    --mainnet \
    --pool-relay-ipv4 <RELAY NODE PUBLIC IP> \
    --pool-relay-port <RELAY NODE PORT> \
    --metadata-url https://git.io/JJWdJ \
    --metadata-hash <POOL METADATA HASH> \
    --out-file pool-registration.cert


| パラメーター | 説明 |
| :--- | :--- |
| stake-pool-verification-key-file | コールドキー検証 |
| vrf-verification-key-file | VRSキー検証 |
| pool-pledge | 出資額（Lovelace） |
| pool-cost | エポックごとの運営コスト（Lovelace） |
| pool-margin | オペレーターマージン |
| pool-reward-account-verification-key-file | 報酬用ステーク鍵検証 |
| pool-owner-staking-verification-key-file | プールオーナー用ステーク鍵検証 |
| out-file | 証明書を書く出力ファイル |
| pool-relay-port | ポート |
| pool-relay-ipv4 | リレーノードIPアドレス |
| metadata-url | 自分のJSONファイルのURL |
| metadata-hash | プールのJSONメタデータファイルのハッシュ |

**報酬用に別の鍵を使用することや、出資を共有する複数のオーナーがいる場合に複数のオーナー鍵を提供することができます。**

**pool-registration.cert** 例


    type: StakePoolCertificateShelley
    title: Free form text
    cbor-hex:
    18b58a03582062d632e7ee8a83769bc108e3e42a674d8cb242d7375fc2d97db9b4dd6eded6fd5820
    48aa7b2c8deb8f6d2318e3bf3df885e22d5d63788153e7f4040c33ecae15d3e61b0000005d21dba0
    001b000000012a05f200d81e820001820058203a4e813b6340dc790f772b3d433ce1c371d5c5f5de
    46f1a68bdf8113f50e779d8158203a4e813b6340dc790f772b3d433ce1c371d5c5f5de46f1a68bdf
    8113f50e779d80f6   

#### 出資の委任証明書を生成する

出資を証明するために、委任証明書を作成します

    cardano-cli shelley stake-address delegation-certificate \
    --stake-verification-key-file stake.vkey \
    --cold-verification-key-file cold.vkey \
    --out-file delegation.cert

これで、`stake.vkey`に関するすべてのステークアドレスからの資金をコールドキー`cold.vkey`に紐づくプールに委任する委任証明書が作成されます。最初のステップで複数のプールオーナー用に複数のステーク鍵が生成された場合は、各自の委任証明書が必要となります。

#### プール証明書と委任証明書をブロックチェーンに送信する

`pool registration certificate`と`delegation certificates`を1つないしは複数のトランザクションに含んでブロックチェーンに送信します。複数の証明書を１つのトランザクションに入れることができます。証明書は順番に適用されます。

#### トランザクションのドラフトを作成する

    cardano-cli shelley transaction build-raw \
    --tx-in <UTXO>#<TxIx> \
    --tx-out $(cat payment.addr)+0 \
    --ttl 0 \
    --fee 0 \
    --out-file tx.draft \
    --certificate-file pool-registration.cert \
    --certificate-file delegation.cert

#### 手数料を計算する

    cardano-cli shelley transaction calculate-min-fee \
    --tx-body-file tx.raw \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --mainnet \
    --witness-count 1 \
    --byron-witness-count 0 \
    --protocol-params-file protocol.json

例

    > 184685

ステークプールの登録にはデポジットが必要です。額は`protocol.json`特定されています。Shelleyテストネット例

"poolDeposit": 500000000

#### .–tx-outの釣銭を計算する
すべてLovelace単位です

    expr <UTxO BALANCE> - <poolDeposit> - <FEE>

#### トランザクションを構築する

    cardano-cli shelley transaction build-raw \
    --tx-in <UTXO>#<TxIx> \
    --tx-out $(cat payment.addr)+<CHANGE IN LOVELACE> \
    --ttl <TTL> \
    --fee <FEE> \
    --out-file tx.raw \
    --certificate-file pool-registration.cert \
    --certificate-file delegation.cert

#### トランザクションに署名する

    cardano-cli shelley transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --signing-key-file stake.skey \
    --signing-key-file cold.skey \
    --mainnet \
    --out-file tx.signed

#### トランザクションを送信する

    cardano-cli shelley transaction submit \
    --tx-file tx.signed \
    --mainnet


#### ステークプールが登録されているか確認する

プールIDを取得します

    cardano-cli shelley stake-pool id --verification-key-file cold.vkey

ネットワーク台帳ステータスに自分のプールIDがあるか確認します

    cardano-cli shelley query ledger-state --mainnet | grep publicKey | grep <poolId>
