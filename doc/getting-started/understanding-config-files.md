# 設定ファイルとその使い方を理解する

#### topology.jsonファイル

ネットワーク内のどのノードとやり取りするかを自分のノードに指示するためのものです。最小バージョンはこのように見えます

```json
{
  "Producers": [
    {
      "addr": "x.x.x.x",
      "port": 3001,
      "valency": 1
    }
  ]
}
```
* これは、自分のノードが`port 3001`上のIP`x.x.x.x`と通信することを意味します

* `valency`はノードが持つべき接続数をノードに伝えます。これはDNSアドレスにのみ作用します。DNSアドレスが与えられた場合、valencyはアクティブ（ホット）接続で維持する解決したIPアドレス数を管理し、IPアドレスはvalencyをブール値として使用し、`0`の場合に対象アドレスを無視します

__ブロックを生成する__ ノードは必ず自分の __リレーノード__ と __のみ__ 交信し、そのリレーノードはネットワークの他のリレーノードと通信する必要があります。IPアドレスとピアのポートはTelegramチャネルにあります。


#### genesis.jsonファイル

ジェネシスファイルは`cardano-cli`が`genesis.spec.json`ファイルを読み込んで生成します。これは、本ドキュメントの範囲外となりますが、これは以下の設定に使用するため重要です

<<<<<<< HEAD
* `genDelegs`：委任を生成するためのジェネシスキーからのマッピング
* `initialFunds`：初期アドレスからそのアドレスにおける初期値をマッピング
* `maxLovelaceSupply`：ブロックチェーンのLovelace総量  
* `systemStart`：スロットゼロの時刻

`genesis.json`ファイルは以下のように見えます

```json
{
  "activeSlotsCoeff": 0.05,
  "protocolParams": {
    "protocolVersion": {
      "minor": 0,
      "major": 2
    },
    "decentralisationParam": 1,
    "eMax": 18,
    "extraEntropy": {
      "tag": "NeutralNonce"
    },
    "maxTxSize": 16384,
    "maxBlockBodySize": 65536,
    "maxBlockHeaderSize": 1100,
    "minFeeA": 44,
    "minFeeB": 155381,
    "minUTxOValue": 1000000,
    "poolDeposit": 500000000,
    "minPoolCost": 340000000,
    "keyDeposit": 2000000,
    "nOpt": 150,
    "rho": 0.003,
    "tau": 0.20,
    "a0": 0.3
  },
  "genDelegs": {
    "ad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950c": {
      "delegate": "d9e5c76ad5ee778960804094a389f0b546b5c2b140a62f8ec43ea54d",
      "vrf": "64fa87e8b29a5b7bfbd6795677e3e878c505bc4a3649485d366b50abadec92d7"
    },
    "b9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497": {
      "delegate": "855d6fc1e54274e331e34478eeac8d060b0b90c1f9e8a2b01167c048",
      "vrf": "66d5167a1f426bd1adcc8bbf4b88c280d38c148d135cb41e3f5a39f948ad7fcc"
    },
    "60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1": {
      "delegate": "7f72a1826ae3b279782ab2bc582d0d2958de65bd86b2c4f82d8ba956",
      "vrf": "c0546d9aa5740afd569d3c2d9c412595cd60822bb6d9a4e8ce6c43d12bd0f674"
    },
    "f7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757": {
      "delegate": "69ae12f9e45c0c9122356c8e624b1fbbed6c22a2e3b4358cf0cb5011",
      "vrf": "6394a632af51a32768a6f12dac3485d9c0712d0b54e3f389f355385762a478f2"
    },
    "162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82": {
      "delegate": "4485708022839a7b9b8b639a939c85ec0ed6999b5b6dc651b03c43f6",
      "vrf": "aba81e764b71006c515986bf7b37a72fbb5554f78e6775f08e384dbd572a4b32"
    },
    "2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6": {
      "delegate": "6535db26347283990a252313a7903a45e3526ec25ddba381c071b25b",
      "vrf": "fcaca997b8105bd860876348fc2c6e68b13607f9bbd23515cd2193b555d267af"
    },
    "268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819b": {
      "delegate": "1d4f2e1fda43070d71bb22a5522f86943c7c18aeb4fa47a362c27e23",
      "vrf": "63ef48bc5355f3e7973100c371d6a095251c80ceb40559f4750aa7014a6fb6db"
    }
  },
  "updateQuorum": 5,
  "networkId": "Mainnet",
  "initialFunds": {},
  "maxLovelaceSupply": 45000000000000000,
  "networkMagic": 764824073,
  "epochLength": 432000,
  "systemStart": "2017-09-23T21:44:51Z",
  "slotsPerKESPeriod": 129600,
  "slotLength": 1,
  "maxKESEvolutions": 62,
  "securityParam": 2160
}

各パラメーターの簡単な説明です。詳細は [spec](https://github.com/input-output-hk/cardano-ledger-specs/tree/master/shelley/chain-and-ledger/executable-spec)を参照してください
```


| パラメーター | 意味 |
|----------| --------- |
<<<<<<< HEAD
| activeSlotsCoeff | ブロックを生成するスロットの比率 |
| poolDeposit | プール登録デポジットの額 |
| protocolVersion| 承認されたプロトコルバージョン |
| decentralisationParam | 連合型ノードが生成したブロックの割合 |
| maxTxSize | 最大トランザクションサイズ |
| minPoolCost | ステークコストがこの値未満のステークプールは登録/再登録不可 |
| minFeeA | 最低手数料計算の一次因子 |
| maxBlockBodySize | 最大ブロックボディサイズ |
| minFeeB | 最低手数料計算の定数係数 |
| eMax | プール終了のエポック境界 |
| extraEntropy | エクストラエントロピー |
| maxBlockHeaderSize | 最大ブロックヘッダーサイズ |
| keyDeposit | 鍵登録デポジットの額 |
| nOpt | 望ましいプール数 |
| rho | 通貨量拡大 |
| tau | トレジャリー拡大 |
| a0 | プールの出資影響力 |
| networkMagic | テストネットの特定 |
| systemStart | スロット0の時刻 |
| genDelegs | 委任を生成するためのジェネシスキーからのマッピング |
| updateQuorum | プロトコルのパラメーター更新に関する投票に必要なクォーラムを決定 |
| initialFunds | アドレスを値にマッピング |
| maxLovelaceSupply | システム内のLovelace総量、報酬計算に使用 |
| networkMagic | テストネットの特定 |
| epochLength | 1エポック内のスロット数 |
| staking | 初期委任 |
| slotsPerKESPeriod | KES期間内のスロット数 |
| slotLength | 秒単位のスロットの長さ |
| maxKESEvolutions | プールオペレーターが新たな運営証明書を作成する必要が生じるまでにKESキーが変化できる最大回数 |
| securityParam | セキュリティパラメーターk |


#### config.jsonファイル

下に示されているのは、ダウンロードしたデフォルトの`config.json`ファイルです。

このファイルには __4__ つのセクションがあり、これにより、ノードが実行することや情報をどのように表示するかを完全にコントロールできます。

__注意：config.jsonファイルの作成方法により、実際のファイルに表示されるフィールドの順序は異なります（一貫性がない）が、ここではより構造的に表示しています__

#### 基本のノード設定

最初のセクションは、基本のノード設定パラメーターに関連します。必ず`TPraos`をプロトコルとし、正しいパスを`mainnet-shelley-genesis.json`ファイル、テストネットでの使用を`RequiresMagic`にしてください。
この例ではSimpleViewを使用しています。これは、アウトプットを`stdout`に送信します。他に、装飾的な表示を生成するためにターミナルマルチプレクサを使用する`LiveView`のオプションもあります。このトピックは後でカバーします

	  "Protocol": "TPraos",
	  "GenesisFile": "mainnet-shelley-genesis.json",
	  "RequiresNetworkMagic": "RequiresMagic",

#### パラメーターの更新

このプロトコルバージョン番号は、ブロック生成ノードにより、プロトコル更新への同意および同期を行うシステムの一部として使用されます。ここでは何も変更する必要はありません

	  "ApplicationName": "cardano-sl",
	  "ApplicationVersion": 0,
	  "LastKnownBlockVersion-Alt": 0,
	  "LastKnownBlockVersion-Major": 2,
	  "LastKnownBlockVersion-Minor": 0,


#### 追跡

`Tracers`は、ロギングの際にどの情報を収集するかをノードに指示するものです。必要な情報のタイプと量に応じてスイッチのようにON/OFFできます。ここで提供されるのはかなり大まかなコントロールですが、望まない追跡アウトプットをフィルタリングするには比較的効率的です。

ノードは`SimpleView`または`LiveView`のいずれかで実行できます。`SimpleView`は単に標準アウトプットを使用し、オプションとしてログアウトプットを使用します。`LiveView`はさまざまなノードメトリクスのライブビュー付きテキストコンソールです。

`TurnOnLogging`：ロギング全般を有効化または無効化します

`TurnOnLogMetrics`：メモリやCPU使用量などさまざまなOSメトリクスを有効化します。こうしたメトリクスはログや監視バックエンドに送信できます

`setupBackends`、`defaultBackends`、`hasEKG`、`hasPrometheus：システムはロギングや監視用にさまざまなバックエンドをサポートします。このセッティングは、設定に使用可能なバックエンドをリストアップしています。ロギングバックエンドの名称は`Katip`です。
EKGまたはPrometheus（プロメテウス）監視インターフェイスを使用したい場合は、EKGバックエンドも可能です

`setupScribes`および`defaultScribes`：Katipロギングバックエンドの場合、アウトプット（スクライブ）をセットアップする必要があります。以下のタイプのスクライブが使用できます

* FileSK：ファイル用
* StdoutSK/StderrSK：stdout/stderr用
* JournalSK：systemdのジャーナルシステム
* DevNullSK
* スクライブアウトプットのフォーマットはScTextまたはScJsonです

`rotation`：katipスクライブのファイルローテーション初期設定です（特定のスクライブ用に上記setupScribesで上書きされていない場合）

```json
"TurnOnLogging": true,
"TurnOnLogMetrics": true,
"TracingVerbosity": "NormalVerbosity",
"minSeverity": "Debug",
"TraceBlockFetchClient": false,
"TraceBlockFetchDecisions": false,
"TraceBlockFetchProtocol": false,
"TraceBlockFetchProtocolSerialised": false,
"TraceBlockFetchServer": false,
"TraceBlockchainTime": false,
"TraceChainDb": true,
"TraceChainSyncBlockServer": false,
"TraceChainSyncClient": false,
"TraceChainSyncHeaderServer": false,
"TraceChainSyncProtocol": false,
"TraceDNSResolver": true,
"TraceDNSSubscription": true,
"TraceErrorPolicy": true,
"TraceForge": true,
"TraceHandshake": false,
"TraceIpSubscription": true,
"TraceLocalChainSyncProtocol": false,
"TraceLocalErrorPolicy": true,
"TraceLocalHandshake": false,
"TraceLocalTxSubmissionProtocol": false,
"TraceLocalTxSubmissionServer": false,
"TraceMempool": true,
"TraceMux": false,
"TraceTxInbound": false,
"TraceTxOutbound": false,
"TraceTxSubmissionProtocol": false,
"setupBackends": [
  "KatipBK"
],
"defaultBackends": [
  "KatipBK"
],
"hasEKG": 12788,
"hasPrometheus": [
  "127.0.0.1",
  12798
],
"setupScribes": [
  {
    "scFormat": "ScText",
    "scKind": "StdoutSK",
    "scName": "stdout",
    "scRotation": null
  }
],
"defaultScribes": [
  [
    "StdoutSK",
    "stdout"
  ]
],
"rotation": {
  "rpKeepFilesNum": 10,
  "rpLogLimitBytes": 5000000,
  "rpMaxAgeHours": 24
  },
```

#### 詳細なロギングコントロール

`options`：追跡アウトプットのフィルタリングに関して、より詳細なコントロールも可能です。また、追跡アウトプットを特定のバックエンドにマッチさせたりルートさせたりすることもできます。これは上記の大まかなコントロールに比べて効率は下がりますが、コントロールの精度は高まります。

`mapBackends`：特定の名称に一致するメトリクスを指定のバックエンドに転送し、上記defaultBackendsを上書きします。これは**上書き**であり拡張ではないため、ここで一致するものはデフォルトのバックエンドへは行かず、明確にリストに上がっているバックエンドに送信されることに注意してください

`mapSubtrace`： このセクションはより表現的です。現在これに関するドキュメンテーションに取り組んでいます

```json
	  "options": {
	    "mapBackends": {
	      "cardano.node-metrics": [
	        "EKGViewBK"
	      ],
	      "cardano.node.BlockFetchDecision.peers": [
	        "EKGViewBK"
	      ],
	      "cardano.node.ChainDB.metrics": [
	        "EKGViewBK"
	      ],
	      "cardano.node.metrics": [
	        "EKGViewBK"
	      ]
	    },
	    "mapSubtrace": {
	      "benchmark": {
	        "contents": [
	          "GhcRtsStats",
	          "MonotonicClock"
	        ],
	        "subtrace": "ObservableTrace"
	      },
	      "#ekgview": {
	        "contents": [
	          [
	            {
	              "contents": "cardano.epoch-validation.benchmark",
	              "tag": "Contains"
	            },
	            [
	              {
	                "contents": ".monoclock.basic.",
	                "tag": "Contains"
	              }
	            ]
	          ],
	          [
	            {
	              "contents": "cardano.epoch-validation.benchmark",
	              "tag": "Contains"
	            },
	            [
	              {
	                "contents": "diff.RTS.cpuNs.timed.",
	                "tag": "Contains"
	              }
	            ]
	          ],
	          [
	            {
	              "contents": "#ekgview.#aggregation.cardano.epoch-validation.benchmark",
	              "tag": "StartsWith"
	            },
	            [
	              {
	                "contents": "diff.RTS.gcNum.timed.",
	                "tag": "Contains"
	              }
	            ]
	          ]
	        ],
	        "subtrace": "FilterTrace"
	      },

	      "cardano.epoch-validation.utxo-stats": {
	        "subtrace": "NoTrace"
	      },
	      "cardano.node-metrics": {
	        "subtrace": "Neutral"
	      },
	      "cardano.node.metrics": {
	        "subtrace": "Neutral"
	      }
	    }
	  }
	}
```