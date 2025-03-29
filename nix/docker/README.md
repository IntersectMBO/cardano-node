# Building Node and Submit API Images
To build and load the oci images into the Docker engine, the most
basic commands are:
```
# Build and load the cardano-node oci image
nix build .#dockerImage/node
docker load -i result

# Build and load the cardano-submit-api oci image
nix build .#dockerImage/submit-api
docker load -i result
```

From a bash shell, the following command example for the cardano-node image
offers a little more convenience by tagging the loaded image with `dev` and
also tagging with a git tag if one is present at the build git commit hash.
```
nix build .#dockerImage/node \
  && RES=$(docker load -i result) \
  && LOADED="${RES##Loaded image: }" \
  && GITHASH=$(git log -n1 --pretty='%H') \
  && docker tag "$LOADED" ghcr.io/intersectmbo/cardano-node:dev \
  && docker rmi "$LOADED"

GITTAG=$(git describe --exact-match --tags $GITHASH)
if [ $? -eq 0 ]; then
  echo "Current tag: $GITTAG"
  docker tag ghcr.io/intersectmbo/cardano-node:dev "ghcr.io/intersectmbo/cardano-node:$GITTAG"
fi
```

To take a quick look around in the cardano-node container:
```
docker run \
  --rm -it --entrypoint=bash \
  ghcr.io/intersectmbo/cardano-node:dev
```

A similar command can be run to look around the cardano-submit-api container.


# Cardano Node Image Operation
## Scripts Mode
To launch cardano-node with pre-loaded configuration, "scripts" mode,
use the `NETWORK` env variable to declare an existing cardano network name.

An example using a docker named volume to persist state to the host:
```
docker run \
  -v data:/data \
  -e NETWORK=mainnet \
  ghcr.io/intersectmbo/cardano-node:dev
```

In "scripts" mode, default state directories include `/{data,ipc,logs}`, with
`/data/db` being the default database state location.


## Custom Mode
To launch cardano-node with a custom configuration, "custom" mode, provide
entrypoint args starting with `run` and:
* Leave the `NETWORK` env variable unset
* Optionally include additional cardano-node args to the entrypoint after `run`
* Optionally include environment variables interpreted by [nix/docker/context/node/bin/run-node](context/node/bin/run-node),
  or `/usr/local/bin/run-node` in the container

For example, launch a custom cardano-node container using cardano-node args and
a local configuration mapped into the container:
```
docker run \
  -v "$PWD/config/cardano:/config" \
  ghcr.io/intersectmbo/cardano-node:dev \
  run \
  --config /config/mainnet/config.json \
  --topology /config/mainnet/topology.json \
  --database-path /data/db
```

Custom mode may also leverage standard mainnet or testnet network config
files found at `/opt/cardano/config` and organized under a subdirectory of the
network's name.  For example, to utilize standard configs for preprod network,
but modify the cardano-node listening port:
```
  docker run \
    -v preprod-data:/data \
    -e CARDANO_CONFIG="/opt/cardano/config/preprod/config.json" \
    -e CARDANO_TOPOLOGY="/opt/cardano/config/preprod/topology.json" \
    -e CARDANO_PORT="6001" \
    ghcr.io/intersectmbo/cardano-node:dev \
    run
```

In "custom" mode, default state directories include
`/opt/cardano/{data,ipc,logs}`, with `/opt/cardano/data/db` being the default
database state location.  These state directories are symlinked to root in the container:
`/opt/cardano/{data,ipc,logs} -> /{data,ipc,logs}` for more consistency between modes.
Standard network config files can be found under `/opt/cardano/config`.


## Merge Mode
With the `NETWORK` env variable set and one or both of
`CARDANO_<CONFIG|TOPOLOGY>_JSON_MERGE` env variables set and containing valid
json, cardano-node will run with deep merged base `NETWORK` config and json
merge config.

Optional env variables and cardano-node args which can be used in custom mode
can also be used in this mode.  Merge mode uses the same default state
directories as custom mode.

An example:
```
docker run \
  -v node-ipc:/ipc \
  -v mainnet-data:/data \
  -e NETWORK=mainnet \
  -e CARDANO_CONFIG_JSON_MERGE='{"MaxConcurrencyBulkSync": 2}' \
  -e CARDANO_TOPOLOGY_JSON_MERGE='{"useLedgerAfterSlot": 147000000}' \
  ghcr.io/intersectmbo/cardano-node:dev
```

## CLI Mode
To run cardano-cli, leave the `NETWORK` env variable unset and provide
entrypoint args starting with `cli` followed by cardano-cli command args.
The cardano-node ipc socket state will need to be provided in cli mode.

An example using a docker named volume to share cardano-node ipc socket state
follows. In this example, another container running cardano-node and also
sharing ipc socket state to the same named volume would already be running.
```
docker run \
  -v node-ipc:/ipc \
  ghcr.io/intersectmbo/cardano-node:dev \
  cli \
  query tip \
  --mainnet
```

See the [Cardano-node socket sharing](#cardano-node-socket-sharing) section for
more on sharing ipc state between containers.


## Bind Mounting Considerations
For "custom" mode, the `/opt/cardano/{data,ipc,logs}` default state directories have been
symlinked to the "scripts" mode default state directories of `/{data,ipc,logs}`
respectively.  This makes bind mounting easier when switching between
"scripts" and "custom" container modes as bind mounting any of the root
default state directory locations, `/{data,ipc,logs}`, will work for both modes.


## Cardano-node Socket Sharing
To share a cardano-node socket with a different container, a volume can be made
for establishing cross-container communication:
```
docker run -v node-ipc:/ipc -e NETWORK=mainnet ghcr.io/intersectmbo/cardano-node:dev
docker run -v node-ipc:/ipc -e NETWORK=mainnet ghcr.io/intersectmbo/some-node-client
```


## Pre-existing State Mounting
If pre-existing cardano-node database state for a network of interest is
available on the host, but not yet available in the container, the state can be
mapped into the container with a local volume driver bind mount.

An example using preprod state from a `$PWD/preprod-data` path follows.  Recall
from above that scripts, custom and merge modes use a default database state
location of `/data/db` either directly or indirectly by symlink, so the host
`$PWD/preprod-data` directory should contain the database state under a `db`
subdir.
```
docker run \
  -v "$PWD/preprod-data:/data" \
  -e NETWORK=preprod \
  ghcr.io/intersectmbo/cardano-node:dev
```

Pre-existing host state may be obtained by any number of means such as host
cardano-node service, rsync'd known good state from a remote, [Mithril
snapshot](https://mithril.network/doc/) or other out-of-band methods.


## Host Mount Image Network Configs
To host mount and explore the image configuration files found under
`/opt/cardano` a new named docker volume can be created.  Docker will
initialize the new named volume with the contents of the container.

An example would be:
```
docker run \
  -v opt-cardano:/opt/cardano \
  ghcr.io/intersectmbo/cardano-node:dev
```

The above command will create a new named volume of `opt-cardano` with the
container contents of `/opt/cardano` and print some image mode information
before exiting.  If there is already a named volume of `opt-cardano`, simply
select a different name, or allow a random name assignment by dropping the
`$NAME:` suffix of the `-v` option.

Exploring the configuration contents from the host after creating the named
volume can be done with commands such as:
```
docker volume ls

# Use the "Mountpoint" value in this command below
docker volume inspect opt-cardano

sudo tree /var/lib/docker/volumes/opt-cardano/_data
```


# Cardano Submit API Image Operation
## Scripts Mode
To launch cardano-submit-api with pre-loaded configuration, "scripts" mode,
use the `NETWORK` env variable to declare an existing cardano network name.

An example using a docker named volume to share ipc socket state:
```
docker run \
  -v node-ipc:/ipc \
  -e NETWORK=mainnet \
  ghcr.io/intersectmbo/cardano-submit-api:dev
```

In "scripts" mode, the `node.socket` file is expected at `/ipc`.


## Custom Mode
To launch cardano-submit-api with a custom configuration, "custom" mode,
leave the `NETWORK` env variable unset and provide a complete set of
cardano-submit-api args to the entrypoint.

For example:
```
docker run \
  -v node-ipc:/ipc \
  -v $PWD/config.json:/config.json \
  ghcr.io/intersectmbo/cardano-submit-api:dev \
  --config /config.json \
  --mainnet \
  --socket-path /ipc/node.socket
```

See the [docker-compose.yml](../../docker-compose.yml) file for a demonstration
of cardano-node and cardano-submit-api together.


## Bind Mounting Considerations:
In the cardano-submit-api container a `/node-ipc` directory is symlinked to `/ipc`
both to align the default ipc socket state directory in both the cardano-node
and cardano-submit-api images and to remain backwards compatible.


# Manual Testing
1. Run -e NETWORK=mainnet and check graceful shutdown SIGINT with -it
2. Run -e NETWORK=mainnet and check graceful shutdown SIGTERM with --detach
3. Run without -e NETWORK and check graceful shutdown SIGINT with -it
4. Run without -e NETWORK and check graceful shutdown SIGTERM with --detach
5. Check cardano-cli access


## Run with NETWORK
Run -e NETWORK=mainnet and check graceful shutdown SIGINT with -it
```
docker run --rm -it \
  -p 3001:3001 \
  -e NETWORK=mainnet \
  -v node-data:/data \
  ghcr.io/intersectmbo/cardano-node:dev
```

Run -e NETWORK=mainnet and check graceful shutdown SIGTERM with --detach
```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -e NETWORK=mainnet \
  -v node-data:/data \
  ghcr.io/intersectmbo/cardano-node:dev

docker logs -f relay
```


## Run without NETWORK
Check graceful shutdown SIGINT with -it
```
docker run --rm -it \
  -p 3001:3001 \
  -v node-data:/data \
  ghcr.io/intersectmbo/cardano-node:dev run
```

Check graceful shutdown SIGTERM with --detach
```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -v node-data:/data \
  -v node-ipc:/ipc \
  ghcr.io/intersectmbo/cardano-node:dev run

docker logs -f relay
```


## Check cardano-cli
Check successful cardano-cli query
```
alias cardano-cli="docker run \
  --rm -it \
  -v node-ipc:/ipc \
  ghcr.io/intersectmbo/cardano-node:dev cli"

cardano-cli query tip --mainnet
```
