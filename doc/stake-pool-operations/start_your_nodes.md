### Start your nodes

Start a __relay node__ with:

    cardano-node run \
    --topology shelley_testnet-topology.json \
    --database-path /db \
    --socket-path /db/node.socket \
    --host-addr <PUBLIC IP> \
    --port <PORT> \
    --config shelley_testnet-config.json

Start a __block producing__ node with:

    cardano-node run \
    --topology shelley_testnet-topology.json \
    --database-path /db \
    --socket-path /db/node.socket \
    --host-addr <PUBLIC IP> \
    --port <PORT> \
    --config shelley_testnet-config.json \
    --shelley-kes-key kes.skey \
    --shelley-vrf-key vrf.skey \
    --shelley-operational-certificate node.cert
