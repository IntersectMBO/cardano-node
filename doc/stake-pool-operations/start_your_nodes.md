### Start your nodes

Start a __relay node__ with:

    cardano-node run \
    --topology mainnet-topology.json \
    --database-path /db \
    --socket-path /db/node.socket \
    --host-addr <PUBLIC IP> \
    --port <PORT> \
    --config mainnet-config.json

Start a __block producing__ node with:

    cardano-node run \
    --topology mainnet-topology.json \
    --database-path /db \
    --socket-path /db/node.socket \
    --host-addr <PUBLIC IP> \
    --port <PORT> \
    --config mainnet-config.json \
    --shelley-kes-key kes.skey \
    --shelley-vrf-key vrf.skey \
    --shelley-operational-certificate node.cert

> Please note that when running a node, it is important to monitor this process so that you can restart it when the node crashes or appears offline. We recommend that stake pool operators run their nodes using standard service running and monitoring tools, which will automatically restart the node in case of failure or disk corruption, thereby avoiding such a situation. Systemd on Linux, a docker, or any equivalent are recommended as process monitoring tools. This does not apply to Daedalus users, as the node process monitoring is handled by Daedalus itself.
