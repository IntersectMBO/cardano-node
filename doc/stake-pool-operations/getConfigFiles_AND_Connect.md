## Get genesis, config and topology files; start the node

Starting the node and connecting it to the testnet requires 3 configuration files:

* shelley_testnet-topology.json
* shelley_testnet-genesis.json
* shelley_testnet-config.json

Let us create a new directory inside `cardano-node`to store the configuration files that we need no start the node.

    mkdir cardano-node/relay
    cd cardano-node/relay

Now, you can download `shelley_testnet-config.json` `shelley_testnet-genesis.json` and `shelley_testnet-topology.json`. from: https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html

Or with the command line using:

    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-config.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-topology.json

Starting the the node uses the command `cardano-node run` and a set of options.

You can get the complete list of available options with `cardano-node run --help`  

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

Start your node:


     cardano-node run \
       --topology path/to/shelley_testnet-topology.json \
       --database-path path/to/db \
       --socket-path path/to/db/node.socket \
       --host-addr x.x.x.x \
       --port 3001 \
       --config path/to/shelley_testnet-config.json

__NOTE__ you need to replace x.x.x.x with your public IP and indicate the correct paths to the required files.

You can check whether the node is syncing by fetching the current tip. (The `--testnet-magic 42` identifies the Shelley testnet)

        export CARDANO_NODE_SOCKET_PATH=path/to/db/node.socket
        cardano-cli shelley query tip --testnet-magic 42

        > Tip (SlotNo {unSlotNo = 74680}) (ShelleyHash {unShelleyHash = HashHeader {unHashHeader = edfefc4ac1e6a6ad1551bcf0650ade22f2e99937936bb61d8d7d5fae2e6a19aa}}) (BlockNo {unBlockNo = 2030})

The syncing phase can take some time.
