# Building Node, Submit API and Tracer Images
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

Similar commands can be run to look around the cardano-submit-api and
cardano-tracer containers.


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
* Optionally include environment variables interpreted by
  [nix/docker/context/node/bin/run-node](context/node/bin/run-node), or
  `/usr/local/bin/run-node` in the container

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
`/opt/cardano/{data,ipc,logs} -> /{data,ipc,logs}` for consistency between modes.
Standard network config files can be found under `/opt/cardano/config`.


## Merge Mode
With the `NETWORK` env variable set and one or both of
`CARDANO_<CONFIG|TOPOLOGY>_JSON_MERGE` env variables set and containing valid
json, cardano-node will run with deep merged base `NETWORK` config and json
merge config.

Optional env variables and cardano-node args which can be used in custom mode
can also be used in this mode.  Merge mode uses the same default state
directories as custom mode.

An example where prometheus binding is set away from localhost while preserving other defaults:
```
docker run \
  -v node-ipc:/ipc \
  -v mainnet-data:/data \
  -e NETWORK=mainnet \
  -e CARDANO_CONFIG_JSON_MERGE='{"TraceOptions":{"":{"backends":["EKGBackend","Forwarder","PrometheusSimple suffix 0.0.0.0 12798","Stdout HumanFormatColoured"]}}}' \
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

## Cardano Ledger Snapshot Conversion
The snapshot-converter utility is included in the cardano-node image at path
`/usr/local/bin/snapshot-converter`.  It can be used to convert between ledger
state types as needed, without relying on host level tooling or full
chain ledger replays.

An example follows to convert preprod ledger state in a named docker volume
from a memory based ledger snapshot to an LMDB snapshot when node is not
already running:
```
docker run -v preprod-data:/data --rm -it --entrypoint=bash ghcr.io/intersectmbo/cardano-node:dev -c '
  mv /data/db/ledger /data/db/ledger-old \
    && mkdir -p /data/db/ledger \
    && snapshot-converter --mem-in /data/db/ledger-old/20807240 --lmdb-out /data/db/ledger/20807240 --config /opt/cardano/config/preprod/config.json
'
```

Note that once ledger state is converted, the cardano-node container will need
to be run with a node configuration aligned with the new ledger state type,
otherwise ledger replay from genesis will re-occur.

For more info, see the [UTxO Migration Guide](https://ouroboros-consensus.cardano.intersectmbo.org/docs/references/miscellaneous/utxo-hd/migrating/).

## Legacy Tracing System
Cardano-node now defaults to using the new tracing system.  The legacy tracing
system is deprecated and will be removed in a future node version.  While still
available, the legacy tracing system can be used by following the example
above in "custom" mode whereby config is passed, and in this case, the config
passed is the legacy style configuration.

Legacy default configuration files are also available within the image at paths:
`/opt/cardano/config/$NETWORK/config-legacy.json`

An example of legacy tracing system usage is:
```
docker run \
  -v preprod-data:/data \
  -e CARDANO_CONFIG="/opt/cardano/config/preprod/config-legacy.json" \
  -e CARDANO_TOPOLOGY="/opt/cardano/config/preprod/topology.json" \
  ghcr.io/intersectmbo/cardano-node:dev \
  run
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


# Cardano Tracer Image Operation
## Scripts Mode
To launch cardano-tracer with pre-loaded configuration, "scripts" mode,
use the `NETWORK` env variable to declare an existing cardano network name.

An example using a docker named volume to persist socket state to the host:
```
docker run \
  -v node-ipc:/ipc \
  -e NETWORK=mainnet \
  ghcr.io/intersectmbo/cardano-tracer:dev
```

In "scripts" mode, default state directories include `/{ipc,logs}` and default
mode of operation is to accept socket connections from node, `AcceptAt`, at a
path of `/ipc/tracer.socket`.  Both tracer socket connection modes of
`AcceptAt` and `ConnectTo` necessitate a shared volume mount between a node
container and a tracer container.  See the [Cardano-node to Cardano-tracer
Socket Connection](#cardano-node-to-cardano-tracer-socket-connection) section
for more on making the required socket connections.


## Custom Mode
To launch cardano-tracer with a custom configuration, "custom" mode, provide
entrypoint args starting with `run` and:
* Leave the `NETWORK` env variable unset
* Optionally include additional cardano-tracer args to the entrypoint after `run`
* Optionally include environment variables interpreted by
  [nix/docker/context/tracer/bin/run-tracer](context/tracer/bin/run-tracer), or
  `/usr/local/bin/run-tracer` in the container

For example, launch a custom cardano-tracer container using cardano-tracer args and
a local configuration mapped into the container:
```
docker run \
  -v node-ipc:/ipc \
  -v "$PWD/config/tracer:/config" \
  ghcr.io/intersectmbo/cardano-tracer:dev \
  run \
  --config /config/mainnet/config.json
```

Custom mode may also leverage standard mainnet or testnet network tracer config
files found at `/opt/cardano/config` and organized under a subdirectory of the
network's name.  For example, to utilize standard configs for preprod network,
but modify the cardano-tracer minimum log severity:
```
docker run \
  -e CARDANO_CONFIG="/opt/cardano/config/preprod/tracer-config.json" \
  -e CARDANO_MIN_LOG_SEVERITY="Debug" \
  ghcr.io/intersectmbo/cardano-tracer:dev \
  run
```

In "custom" mode, default state directories include `/opt/cardano/{ipc,logs}`.
These state directories are symlinked to root in the container:
`/opt/cardano/{ipc,logs} -> /{ipc,logs}` for consistency between modes.
Standard network tracer config files can be found under `/opt/cardano/config`.


## Merge Mode
With the `NETWORK` env variable set and `CARDANO_CONFIG_JSON_MERGE` env
variable set and containing valid json, cardano-tracer will run with deep
merged base `NETWORK` tracer config and json merge config.

Optional env variables and cardano-tracer args which can be used in custom mode
can also be used in this mode.  Merge mode uses the same default state
directories as custom mode.

An example which changes the prometheus binding address from a default of
localhost (`127.0.0.1`) to `0.0.0.0`:
```
docker run \
  -v node-ipc:/ipc \
  -e NETWORK=mainnet \
  -e CARDANO_CONFIG_JSON_MERGE='{"hasPrometheus":{"epHost": "0.0.0.0"}}' \
  ghcr.io/intersectmbo/cardano-tracer:dev
```

Similar bind mounting, and host mounted network tracer config considerations
exist for the cardano-tracer image as also detailed above for the cardano-node
image.


## Cardano-node to Cardano-tracer Socket Connection
To establish a cardano-node container to cardano-tracer container socket
connection, in addition to a shared volume mount where such a socket can be
accessed by both containers, the cardano-node container will need to be started
with an extra argument.  To include the extra argument, either custom mode or
merge mode for the node image will be required.

An example for node to connect to a tracer socket, using custom mode to
append the extra cli arg:
```
docker run \
  -v node-ipc:/ipc \
  -e CARDANO_CONFIG="/opt/cardano/config/mainnet/config.json" \
  -e CARDANO_TOPOLOGY="/opt/cardano/config/mainnet/topology.json" \
  ghcr.io/intersectmbo/cardano-node:dev \
  run \
  --tracer-socket-path-connect /ipc/tracer.socket

docker run \
  -v node-ipc:/ipc \
  -e NETWORK=mainnet \
  ghcr.io/intersectmbo/cardano-tracer:dev
```

An example for node to use a socket to accept a tracer connection, using merge
mode to set the tracer node name and append the extra cli arg via env variable:
```
docker run \
  -v node-ipc:/ipc \
  -e NETWORK=mainnet \
  -e CARDANO_TRACER_SOCKET_PATH_ACCEPT="/ipc/node-tracer.socket" \
  -e CARDANO_CONFIG_JSON_MERGE='{"TraceOptionNodeName":"node1"}' \
  ghcr.io/intersectmbo/cardano-node:dev

docker run \
  -v node-ipc:/ipc \
  -e NETWORK=mainnet \
  -e CARDANO_CONFIG_JSON_MERGE='{"network":{"contents":["/ipc/node-tracer.socket"],"tag":"ConnectTo"}}' \
  ghcr.io/intersectmbo/cardano-tracer:dev
```

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
