# Configure topology files for block-producing and relay nodes.

Before we register our stake pool, let's configure our __block-producing__ and __relay__ nodes:

__NOTE:__ Here you can find peers to connect to, and submit your own relay's data:  https://github.com/input-output-hk/cardano-ops/blob/master/topologies/ff-peers.nix#L5-L10

### Configure the block-producing node

Get the configuration files for your block-producing node if you don't have them already, for example

    mkdir config-files
    cd config-files     

    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-config.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-topology.json

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

### Configure the relay node:

Make your __relay node__ `talk` to your __block-producing__ node and __other relays__ in the network by editing the `shelley_testnet-topology.json` file:


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
