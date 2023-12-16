
## Build Cardano Node + Image

https://github.com/input-output-hk/cardano-node-wiki/wiki/building-the-node-using-nix

```
# Build + Install the cardano node
nix build .#mainnet/node -o ~/bin/cardano-node

# Build + Install the cardano Docker image
nix run .#dockerImage/node/load \
  && GITHASH=`git log -n1 --pretty='%H'` \
  && docker tag inputoutput/cardano-node:$GITHASH inputoutput/cardano-node:dev \
  && docker rmi inputoutput/cardano-node:$GITHASH

GITTAG=`git describe --exact-match --tags $GITHASH`
if [ $? -eq 0 ]; then
  echo "Current tag: $GITTAG"
  docker tag inputoutput/cardano-node:dev inputoutput/cardano-node:$GITTAG
fi

# Bash into the node to look around
docker run --rm -it --entrypoint=bash \
  -v node-data:/opt/cardano/data \
  inputoutput/cardano-node:dev

cardano-node run \
  --config /opt/cardano/config/mainnet-config.json \
  --topology /opt/cardano/config/mainnet-topology.json \
  --socket-path /opt/cardano/ipc/socket \
  --database-path /opt/cardano/data \
  --host-addr 0.0.0.0 \
  --port 3001
```

## Manual Testing

1. Run -e NETWORK=mainnet and check graceful shutdown SIGINT with -it
2. Run -e NETWORK=mainnet and check graceful shutdown SIGTERM with --detach
3. Run without -e NETWORK and check graceful shutdown SIGINT with -it
4. Run without -e NETWORK and check graceful shutdown SIGTERM with --detach
5. Check cardano-cli access

### Run with NETWORK

Run -e NETWORK=mainnet and check graceful shutdown SIGINT with -it

```
docker run --rm -it \
  -p 3001:3001 \
  -e NETWORK=mainnet \
  -v node-data:/data/db \
  inputoutput/cardano-node:dev
```

Run -e NETWORK=mainnet and check graceful shutdown SIGTERM with --detach

```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -e NETWORK=mainnet \
  -v node-data:/data/db \
  inputoutput/cardano-node:dev

docker logs -f relay
```

### Run without NETWORK

Check graceful shutdown SIGINT with -it

```
docker run --rm -it \
  -p 3001:3001 \
  -v node-data:/opt/cardano/data \
  inputoutput/cardano-node:dev run
```

Check graceful shutdown SIGTERM with --detach

```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -v node-data:/opt/cardano/data \
  -v node-ipc:/opt/cardano/ipc \
  inputoutput/cardano-node:dev run

docker logs -f relay
```

### Check cardano-cli

```
alias cardano-cli="docker run --rm -it \
  -v node-ipc:/opt/cardano/ipc \
  inputoutput/cardano-node:dev cli"

cardano-cli query tip --mainnet
```
