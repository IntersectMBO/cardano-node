# Configure topology files for block-producing and relay nodes.

Before you start your nodes, you need to prepare the topology files.

#### Configure the block-producing node

Make the __block-producing__ node to "talk" only to __YOUR__ relay node. Do not forget to configure your firewall also:

    nano shelley_testnet-topology.json

  	{
  	  "Producers": [
  	    {
  	      "addr": "<RELAY IP ADDRESS",
  	      "port": <PORT>,
  	      "valency": 1
  	    }
  	  ]
  	}

#### Configure the relay node:

Make your __relay node__ `talk` to your __block-producing__ node and __other relays__ in the network by editing the `topology.json` file:


    nano shelley_testnet-topology.json

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

__NOTE:__ Here you can find peers in the **Shelley Testnet** to connect to:  https://github.com/input-output-hk/cardano-ops/blob/batch-ff-relays-pr/topologies/ff-peers.nix
