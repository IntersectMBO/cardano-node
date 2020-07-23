# Get configuration files

Starting the node and connecting it to the testnet requires 3 configuration files:

* topology.json
* genesis.json
* config.json

You can download the configuration files from:

 [https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html](https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html)


From the CLI you can use

For shelley_testnet

    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-config.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-shelley-genesis.json
    wget https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/shelley_testnet-topology.json

For Mainnet_Candidate_Testnet_3

    wget https://hydra.iohk.io/build/3596468/download/1/mainnet_candidate_3-config.json
    wget https://hydra.iohk.io/build/3596468/download/1/mainnet_candidate_3-byron-genesis.json
    wget https://hydra.iohk.io/build/3596468/download/1/mainnet_candidate_3-shelley-genesis.json
    wget https://hydra.iohk.io/build/3596468/download/1/mainnet_candidate_3-topology.json

For Mainnet:

    wget https://hydra.iohk.io/build/3596468/download/1/mainnet-config.json
    wget https://hydra.iohk.io/build/3596468/download/1/mainnet-byron-genesis.json
    wget https://hydra.iohk.io/build/3596468/download/1/mainnet_candidate_3-shelley-genesis.json
    wget https://hydra.iohk.io/build/3596468/download/1/mainnet-topology.json

Starting the the node uses the command `cardano-node run` and a set of options.

Get the complete list of available options with `cardano-node run --help`  

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

To start a passive node:

     cardano-node run \
       --topology path/to/shelley_testnet-topology.json \
       --database-path path/to/db \
       --socket-path path/to/db/node.socket \
       --host-addr x.x.x.x \
       --port 3001 \
       --config path/to/shelley_testnet-config.json

**Replace x.x.x.x with your public IP and indicate the correct paths to the required files.**

Many commands relay on the environment variable CARDANO_NODE_SOCKET_PATH:

    export CARDANO_NODE_SOCKET_PATH=path/to/db/node.socket

Check that the node is syncing by fetching the current tip. When syncing slotNo should be increasing.

        cardano-cli shelley query tip --testnet-magic 42

        {
        "blockNo": 36322,
        "headerHash": "3f1bea22be21452415851ae670f4bac9340471cb7f2f6a664fac56d7f60dbaad",
        "slotNo": 888561
        }

**`--testnet-magic 42` identifies the testnets, for mainnet use `--mainnet` **
