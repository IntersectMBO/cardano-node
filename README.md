[![Build Status](https://badge.buildkite.com/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a.svg)](https://buildkite.com/input-output-hk/cardano-node)

# cardano-node

Integration of the [ledger](https://github.com/input-output-hk/cardano-ledger),
[consensus](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus),
[networking](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network)
and [node shell](https://github.com/input-output-hk/cardano-shell)
repositories.
[Logging](https://github.com/input-output-hk/iohk-monitoring-framework) is
provided as
a [feature](https://github.com/input-output-hk/cardano-shell/blob/master/app/Cardano/Shell/Features/Logging.hs)
by the node shell to the other packages.

## Docker

### Build

```shell script
# Include default config
docker build --build-arg NETWORK=mainnet -t cardano-node:mainnet .

# Requires config mounted at runtime
docker build -t cardano-node .
```
### Run

```shell script
docker run -it cardano-node:mainnet

# Mount all config at runtime
docker run -it -v /my-custom-config:/config cardano-node

# Mount some config at runtime
docker run -it -v /my-custom-config/config.yaml:/config/config.yaml cardano-node:mainnet

# Mount state and socket for IPC
docker run -it -v /node-socket:/data/ipc -v /node-db:/data/db cardano-node:mainnet

```

### In a `docker-compose.json`
```yaml
...
services:
  cardano-node:
    image: cardano-node:mainnet
    volumes:
      - node-db:/data/db
      - node-ipc:/data/ipc
    restart: on-failure
volumes:
  node-db:
  node-ipc:
```