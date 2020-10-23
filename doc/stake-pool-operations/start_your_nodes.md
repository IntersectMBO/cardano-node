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

Please note that when running a node, it is important to use process monitoring so that the node can be automatically restarted when it terminates unexpectedly. We recommend that node operators run their nodes using a standard service monitoring or supervisor tool which will automatically restart the node in case of failure (such as temporary disk corruption), thereby avoiding downtime. We recommend systemd on Linux, docker’s auto-restart functionality, or any equivalent process monitoring tool. This does not apply to Daedalus users, as the node process monitoring is handled by Daedalus itself.
