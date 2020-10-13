### ノードを起動する

__リレー__ ノードを起動します

    cardano-node run \
    --topology mainnet-topology.json \
    --database-path /db \
    --socket-path /db/node.socket \
    --host-addr <PUBLIC IP> \
    --port <PORT> \
    --config mainnet-config.json

__ブロック生成__ ノードを起動します

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
