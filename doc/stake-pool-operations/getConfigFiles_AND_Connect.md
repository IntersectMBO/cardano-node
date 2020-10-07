# 設定ファイルを取得する

ノードを開始し、ネットワークに接続するためには、3つの設定ファイルが必要です

* topology.json
* genesis.json
* config.json

設定ファイルは以下からダウンロードできます

 [https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html](https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html)


CLIから使用可能なもの

Cardanoテストネット用

    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/testnet-config.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/testnet-byron-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/testnet-shelley-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/testnet-topology.json

メインネット用

    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-config.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-byron-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-shelley-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-topology.json

ノードを開始するには、コマンド`cardano-node run`とオプションのセットを使用します。

使用可能な全オプションのリストは`cardano-node run --help`で入手できます  

	--topology FILEPATH             The path to a file describing the topology.
  	--database-path FILEPATH        Directory where the state is stored.
  	--socket-path FILEPATH          Path to a cardano-node socket
  	--host-addr HOST-NAME           Optionally limit node to one ipv6 or ipv4 address
  	--port PORT                     The port number
  	--config NODE-CONFIGURATION     Configuration file for the cardano-node
  	--validate-db                   Validate all on-disk database files
  	--shutdown-ipc FD               Shut down the process when this inherited FD reaches EOF
  	--shutdown-on-slot-synced SLOT  Shut down the process after ChainDB is synced up to the
  	                                specified slot
    -h,--help                       Show this help text

パッシブノードを開始するには

     cardano-node run \
       --topology path/to/mainnet-topology.json \
       --database-path path/to/db \
       --socket-path path/to/db/node.socket \
       --host-addr x.x.x.x \
       --port 3001 \
       --config path/to/mainnet-config.json

**x.x.x.xを自分のパブリックIPと置き換え、必要なファイルへの正しいパスを指定します。**

多くのコマンドは、環境変数CARDANO_NODE_SOCKET_PATHに依存しています

    export CARDANO_NODE_SOCKET_PATH=path/to/db/node.socket

ノードが現在のチップをフェッチすることで同期していることを確認してください。同期しているとslotNoが増加します。

        cardano-cli shelley query tip --mainnet

        {
        "blockNo": 36322,
        "headerHash": "3f1bea22be21452415851ae670f4bac9340471cb7f2f6a664fac56d7f60dbaad",
        "slotNo": 888561
        }

**注意:**`--mainnet`はCardanoメインネットを特定するものです。テストネットの場合は`--testnet-magic 1097911063`を使用してください。
