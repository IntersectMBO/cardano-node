# Get configuration files

Starting the node and connecting it to the network requires 5 configuration files:

* topology.json
* byron-genesis.json
* shelley-genesis.json
* alonzo-genesis.json
* config.json

You can download the configuration files for **PREVIEW TESTNET**, **PREPRODUCTION TESTNET** and **MAINNET** from

* https://book.world.dev.cardano.org/environments.html

From the CLI you can use

For Preview testnet

    wget https://book.world.dev.cardano.org/environments/preview/config.json
    wget https://book.world.dev.cardano.org/environments/preview/topology.json
    wget https://book.world.dev.cardano.org/environments/preview/byron-genesis.json
    wget https://book.world.dev.cardano.org/environments/preview/shelley-genesis.json
    wget https://book.world.dev.cardano.org/environments/preview/alonzo-genesis.json


For Preproduction testnet

    wget https://book.world.dev.cardano.org/environments/preprod/config.json
    wget https://book.world.dev.cardano.org/environments/preprod/topology.json
    wget https://book.world.dev.cardano.org/environments/preprod/byron-genesis.json
    wget https://book.world.dev.cardano.org/environments/preprod/shelley-genesis.json
    wget https://book.world.dev.cardano.org/environments/preprod/alonzo-genesis.json

For Mainnet:

    wget https://book.world.dev.cardano.org/environments/mainnet/config.json
    wget https://book.world.dev.cardano.org/environments/mainnet/topology.json
    wget https://book.world.dev.cardano.org/environments/mainnet/byron-genesis.json
    wget https://book.world.dev.cardano.org/environments/mainnet/shelley-genesis.json
    wget https://book.world.dev.cardano.org/environments/mainnet/alonzo-genesis.json

Starting the node uses the command `cardano-node run` and a set of options.

Get the complete list of available options with `cardano-node run --help`

  	--topology FILEPATH             The path to a file describing the topology.
  	--database-path FILEPATH        Directory where the state is stored.
  	--socket-path FILEPATH          Path to a cardano-node socket
  	--host-addr IP-ADDRESS          Optionally limit node to one IPv4 address
  	--host-ipv6-addr IP-ADDRESS     Optionally limit node to one IPv6 address
  	--port PORT                     The port number
  	--config NODE-CONFIGURATION     Configuration file for the cardano-node
  	--validate-db                   Validate all on-disk database files
  	--shutdown-ipc FD               Shut down the process when this inherited FD reaches EOF
  	--shutdown-on-slot-synced SLOT  Shut down the process after ChainDB is synced up to the
  	                                specified slot
    -h,--help                       Show this help text

**Note**: If you do not specify `--host-addr` nor `--host-ipv6-addr`, node will use the default IPv4 and IPv6 addresses (depending which addresses are available).  If one specifies one of them only that address will be used, in particular if you only provide an IPv4 address, the node will not connect over IPv6.
To start a passive node:

     cardano-node run \
       --topology topology.json \
       --database-path db \
       --socket-path db/node.socket \
       --host-addr x.x.x.x \
       --port 3001 \
       --config config.json

**Replace x.x.x.x with your public IP and indicate the correct paths to the required files.**

Many commands rely on the environment variable CARDANO_NODE_SOCKET_PATH:

    export CARDANO_NODE_SOCKET_PATH=path/to/node.socket

Check that the node is syncing by fetching the current tip. When syncing `slot` should be increasing.

    cardano-cli query tip --mainnet

    {
        "epoch": 259,
        "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
        "slot": 26633911,
        "block": 5580350
    }

**Note**`--mainnet` identifies the Cardano mainnet, for **preview testnet** use `--testnet-magic 2` and for **preproduction testnet** use `--testnet-magic 1`
