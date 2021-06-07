
## Build Cardano Node + Image

https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/building-the-node-using-nix.html

```
# Build + Install the cardano node
nix-build -A scripts.mainnet.node -o ~/bin/cardano-node

# Build + Install the cardano Docker image
docker load -i $(nix-build -A dockerImage --no-out-link) \
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
5. Run with -e CARDANO_UPDATE_TOPOLOGY and check cron job
6. Check cardano-cli access

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

### Run with -e CARDANO_UPDATE_TOPOLOGY

```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -e CARDANO_UPDATE_TOPOLOGY=true \
  -e CARDANO_PUBLIC_IP="35.239.139.180" \
  -e CARDANO_CUSTOM_PEERS="relay01.astorpool.net:3001" \
  -v shelley-data:/opt/cardano/data \
  -v shelley-ipc:/opt/cardano/ipc \
  inputoutput/cardano-node:dev run

docker logs -f relay
```

### Check cardano-cli

```
alias cardano-cli="docker run --rm -it \
  -v node-ipc:/opt/cardano/ipc \
  inputoutput/cardano-node:dev"

cardano-cli query tip --mainnet
```
