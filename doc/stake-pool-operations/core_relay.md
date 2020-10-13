# ブロック生成ノードおよびリレーノードのトポロジーファイルを設定する

ノードを起動する前に、トポロジーファイルを準備する必要があります。

#### ブロック生成ノードを設定する

__ブロック生成__ ノードが __自分の__ リレーノードとのみ「talk」するように設定します。ファイアウォールの設定も忘れないでください

    nano mainnet-topology.json

  	{
  	  "Producers": [
  	    {
  	      "addr": "<RELAY IP ADDRESS",
  	      "port": <PORT>,
  	      "valency": 1
  	    }
  	  ]
  	}

#### リレーノードを設定する

自分の __リレーノード__ が自分の __ブロック生成__ ノードおよびネットワーク内の __他のリレー__ ノードと`talk`するように`topology.json`ファイルを編集します



    nano mainnet-topology.json

    {
      "Producers": [
        {
          "addr": "<BLOCK-PRODUCING IP ADDRESS",
          "port": PORT,
          "valency": 1
        },
        {
          "addr": "<IP ADDRESS>",
          "port": <PORT>,
          "valency": 1
        },
        {
          "addr": "<IP ADDRESS",
          "port": <PORT>,
          "valency": 1
        }
      ]
    }
