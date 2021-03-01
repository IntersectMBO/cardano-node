
# Running on Plain Docker

## Build Cardano Node Image

https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/building-the-node-using-nix.html

```
# Build + Install the Cardano Docker image
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
  -v shelley-data:/opt/cardano/data \
  inputoutput/cardano-node:dev

cardano-node run \
  --config /opt/cardano/config/mainnet-config.json \
  --topology /opt/cardano/config/mainnet-topology.json \
  --socket-path /opt/cardano/ipc/socket \
  --database-path /opt/cardano/data \
  --host-addr 0.0.0.0 \
  --port 3001
```

## Populating the Data Volume

```
docker run --name=tmp -v shelley-data:/data centos
docker cp ~/data/protocolMagicId tmp:/data
docker cp ~/data/immutable tmp:/data
docker cp ~/data/ledger tmp:/data
docker rm tmp
```

## Manual Testing

Here some manual test that we can perform ...

### Run with NETWORK

Run -e NETWORK=mainnet and check graceful shutdown SIGINT with -it

```
docker rm -f relay
docker run --rm -it \
  -p 3001:3001 \
  -e NETWORK=mainnet \
  -v shelley-data:/data/db \
  inputoutput/cardano-node:dev
```

Run -e NETWORK=mainnet and check graceful shutdown SIGTERM with --detach

```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -e NETWORK=mainnet \
  -v shelley-data:/data/db \
  inputoutput/cardano-node:dev

docker logs -f relay
```

### Run without NETWORK

Check graceful shutdown SIGINT with -it

```
docker rm -f relay
docker run --rm -it \
  -p 3001:3001 \
  -v shelley-data:/opt/cardano/data \
  inputoutput/cardano-node:dev run
```

Check graceful shutdown SIGTERM with --detach

```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -v shelley-data:/opt/cardano/data \
  -v shelley-ipc:/opt/cardano/ipc \
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
  -v shelley-data:/opt/cardano/data \
  -v shelley-ipc:/opt/cardano/ipc \
  inputoutput/cardano-node:dev run

docker logs -f relay
```

### Run with CARDANO_TOPOLOGY from JSON

```
CUSTOM_TOPOLOGY='
{
  "Producers": [
    {
      "addr": "relays-new.cardano-mainnet.iohk.io",
      "port": 3001,
      "valency": 1
    },
    {
      "addr": "relay01.astorpool.net",
      "port": 3001,
      "valency": 1
    }
  ]
}'

docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -e CARDANO_TOPOLOGY="$CUSTOM_TOPOLOGY" \
  -v shelley-data:/opt/cardano/data \
  inputoutput/cardano-node:dev run

docker logs -f relay
```

### Check cardano-cli

```
alias cardano-cli="docker run --rm -it \
  -v shelley-ipc:/opt/cardano/ipc \
  inputoutput/cardano-node:dev cardano-cli"

cardano-cli query tip --mainnet
```

# Running on Kubernetes

## Creating a Kubernetes cluster

### Installing Google Cloud SDK

https://cloud.google.com/kubernetes-engine/docs/quickstart

https://cloud.google.com/sdk/docs/cheatsheet

To install gcloud and kubectl, perform the following steps:

  1) Install the Cloud SDK, which includes the gcloud command-line tool.
  2) Install the kubectl command-line tool

```
gcloud components install kubectl
```

### Configuring default settings

```
PROJECT_ID=beaconchain

CLUSTER_NAME=cardano
CLUSTER_ZONE=us-central1-a

# Setting a default project ID
gcloud config set project $PROJECT_ID

# Setting a default compute zone or region
gcloud config set compute/zone $CLUSTER_ZONE
```

### Creating a GKE cluster

https://cloud.google.com/sdk/gcloud/reference/container/clusters/create

Lets create a single zone cluster with two medium size nodes

```
gcloud container clusters create $CLUSTER_NAME \
  --disk-size=32GB \
  --machine-type=e2-medium \
  --node-locations=$CLUSTER_ZONE \
  --zone=$CLUSTER_ZONE \
  --num-nodes=2
```

### Get credentials for the cluster

```
gcloud container clusters get-credentials $CLUSTER_NAME
```

## Running a Relay + Block Producer

### Create the Cardano Namespace

First, we create a dedicated namespace for Cardano resources.

```
$ kubectl create namespace cardano

namespace/cardano created
```
and set it as the default in the current context

```
$ kubectl config set-context --current --namespace=cardano

Context "gke_beaconchain_us-central1-a_cardano" modified.
```

### Attach label to the node

Here we get the external IP for each k8s node in the cluster

```
$ kubectl get nodes --output wide

NAME                                     STATUS   ROLES    AGE   VERSION             INTERNAL-IP   EXTERNAL-IP      OS-IMAGE                             KERNEL-VERSION   CONTAINER-RUNTIME
gke-cardano-default-pool-f7c5bca9-jp88   Ready    <none>   14s   v1.18.12-gke.1210   10.128.0.30   35.193.226.217   Container-Optimized OS from Google   5.4.49+          docker://19.3.9
gke-cardano-default-pool-f7c5bca9-p8q1   Ready    <none>   14s   v1.18.12-gke.1210   10.128.0.29   34.121.196.107   Container-Optimized OS from Google   5.4.49+          docker://19.3.9

EXTERNAL_IP=35.193.226.217
NODE_PORT=30010

# The IP address that you choose must be a valid IPv4 or IPv6 address from within the service-cluster-ip-range CIDR range.
RELAY_CLUSTER_IP=10.3.240.100
BPROD_CLUSTER_IP=10.3.240.200
```

We want to deploy the relay to a node with a known IP.
Here we assign node type labels that we later use in node selectors.

```
$ kubectl label nodes gke-cardano-default-pool-f7c5bca9-jp88 nodeType=relay
$ kubectl label nodes gke-cardano-default-pool-f7c5bca9-p8q1 nodeType=bprod
```

### Create a Firewall Rule

Here we create a firewall rule for incomming trafic to a given node port.

```
$ gcloud compute firewall-rules create k8s-relay --allow tcp:$NODE_PORT

NAME       NETWORK  DIRECTION  PRIORITY  ALLOW      DENY  DISABLED
k8s-relay  default  INGRESS    1000      tcp:30010        False
```

### Relay Configuration

Next we create a configuration map for the Relay

```
RELAY_TOPOLOGY=`cat << EOF
{
  "Producers": [
    {
      "addr": "relays-new.cardano-mainnet.iohk.io",
      "port": 3001,
      "valency": 1
    },
    {
      "addr": "$BPROD_CLUSTER_IP",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF`

$ kubectl create configmap relaycfg \
  --from-literal=publicIP="$EXTERNAL_IP:$NODE_PORT" \
  --from-literal=customPeers="$BPROD_CLUSTER_IP:3001" \
  --from-literal=topology="$RELAY_TOPOLOGY"

configmap/relaycfg created
```

### BlockProducer Configuration

Next we create a configuration map for the Block Producer

```
BPROD_TOPOLOGY=`cat << EOF
{
  "Producers": [
    {
      "addr": "$RELAY_CLUSTER_IP",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF`

$ kubectl create configmap bprodcfg \
  --from-literal=topology="$BPROD_TOPOLOGY"

configmap/bprodcfg created
```

### Creating the Block Producer Keys Secret

Store the Block Producer configuration keys in a Kubernetes Secret

```
kubectl create secret generic nodekeys \
  --from-file=./cardano/keys/pool/kes.skey \
  --from-file=./cardano/keys/pool/vrf.skey \
  --from-file=./cardano/keys/pool/node.cert

secret/nodekeys created
```

### Deploying the Relay + Block Producer Pods

Finally, we deploy a Cardano pods like this ...

```
$ kubectl apply -f nix/docker/k8s/cardano-nodes.yaml

storageclass.storage.k8s.io/cardano-standard-rwo unchanged
persistentvolumeclaim/relay-data created
pod/relay created
service/relay-tcp created
persistentvolumeclaim/bprod-data created
pod/bprod created
service/bprod-tcp created
```

### Looking at the Relay output

```
$ kubectl logs --tail=500 -f relay

Running the cardano node ...
Generating /var/cardano/config/mainnet-topology.json ...
CARDANO_CONFIG=/opt/cardano/config/mainnet-config.json
CARDANO_TOPOLOGY=/var/cardano/config/mainnet-topology.json
CARDANO_BIND_ADDR=0.0.0.0
CARDANO_PORT=3001
CARDANO_DATABASE_PATH=/opt/cardano/data
CARDANO_SOCKET_PATH=/opt/cardano/ipc/socket
CARDANO_LOG_DIR=/opt/cardano/logs
CARDANO_PUBLIC_IP=35.193.226.217:30010
CARDANO_CUSTOM_PEERS=10.3.240.200:3001
CARDANO_UPDATE_TOPOLOGY=true
CARDANO_BLOCK_PRODUCER=false
cardano-node run --config /opt/cardano/config/mainnet-config.json --topology /var/cardano/config/mainnet-topology.json --database-path /opt/cardano/data --socket-path /opt/cardano/ipc/socket --host-addr 0.0.0.0 --port 3001
Topology update: 47 * * * * root topologyUpdate
Initially waiting for 10 minutes ...
Listening on http://127.0.0.1:12798
[relay:cardano.node.networkMagic:Notice:5] [2021-02-27 14:35:06.97 UTC] NetworkMagic 764824073
[relay:cardano.node.basicInfo.protocol:Notice:5] [2021-02-27 14:35:06.97 UTC] Byron; Shelley
[relay:cardano.node.basicInfo.version:Notice:5] [2021-02-27 14:35:06.97 UTC] 1.25.1
...
[relay:cardano.node.ChainDB:Notice:35] [2021-02-27 14:35:08.22 UTC] Chain extended, new tip: 1dbc81e3196ba4ab9dcb07e1c37bb28ae1c289c0707061f28b567c2f48698d50 at slot 1
[relay:cardano.node.ChainDB:Notice:35] [2021-02-27 14:35:08.22 UTC] Chain extended, new tip: 52b7912de176ab76c233d6e08ccdece53ac1863c08cc59d3c5dec8d924d9b536 at slot 2
[relay:cardano.node.ChainDB:Notice:35] [2021-02-27 14:35:08.22 UTC] Chain extended, new tip: be06c81f4ad34d98578b67840d8e65b2aeb148469b290f6b5235e41b75d38572 at slot 3
```

### Looking at the Block Producer output

```
$ kubectl logs --tail=500 -f bprod

Running the cardano node ...
Generating /var/cardano/config/mainnet-topology.json ...
CARDANO_CONFIG=/opt/cardano/config/mainnet-config.json
CARDANO_TOPOLOGY=/var/cardano/config/mainnet-topology.json
CARDANO_BIND_ADDR=0.0.0.0
CARDANO_PORT=3001
CARDANO_DATABASE_PATH=/opt/cardano/data
CARDANO_SOCKET_PATH=/opt/cardano/ipc/socket
CARDANO_LOG_DIR=/opt/cardano/logs
CARDANO_PUBLIC_IP=
CARDANO_CUSTOM_PEERS=
CARDANO_UPDATE_TOPOLOGY=false
CARDANO_BLOCK_PRODUCER=true
CARDANO_SHELLEY_KES_KEY=/var/cardano/config/keys/kes.skey
CARDANO_SHELLEY_VRF_KEY=/var/cardano/config/keys/vrf.skey
CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE=/var/cardano/config/keys/node.cert
cardano-node run --config /opt/cardano/config/mainnet-config.json --topology /var/cardano/config/mainnet-topology.json --database-path /opt/cardano/data --socket-path /opt/cardano/ipc/socket --host-addr 0.0.0.0 --port 3001 --shelley-kes-key /var/cardano/config/keys/kes.skey --shelley-vrf-key /var/cardano/config/keys/vrf.skey --shelley-operational-certificate /var/cardano/config/keys/node.cert
Listening on http://127.0.0.1:12798
[bprod:cardano.node.networkMagic:Notice:5] [2021-02-27 14:35:06.97 UTC] NetworkMagic 764824073
[bprod:cardano.node.basicInfo.protocol:Notice:5] [2021-02-27 14:35:06.97 UTC] Byron; Shelley
[bprod:cardano.node.basicInfo.version:Notice:5] [2021-02-27 14:35:06.97 UTC] 1.25.1
...
[bprod:cardano.node.ChainDB:Notice:35] [2021-02-27 14:35:08.22 UTC] Chain extended, new tip: 1dbc81e3196ba4ab9dcb07e1c37bb28ae1c289c0707061f28b567c2f48698d50 at slot 1
[bprod:cardano.node.ChainDB:Notice:35] [2021-02-27 14:35:08.22 UTC] Chain extended, new tip: 52b7912de176ab76c233d6e08ccdece53ac1863c08cc59d3c5dec8d924d9b536 at slot 2
[bprod:cardano.node.ChainDB:Notice:35] [2021-02-27 14:35:08.22 UTC] Chain extended, new tip: be06c81f4ad34d98578b67840d8e65b2aeb148469b290f6b5235e41b75d38572 at slot 3
```

## Deleting Resources

To stop the nodes, we can simply delete their respective pod.
The cardano node process will shutdown gracefully and continue from where it left off when we redeploy the pod.

```
$ kubectl delete pod,svc --all

pod "bprod" deleted
pod "relay" deleted
service "bprod-clip" deleted
service "relay-clip" deleted
service "relay-np" deleted
```

The persistent volume is a cluster resource that survives namespace deletion.

```
$ kubectl delete pvc,pv --all

persistentvolumeclaim "relay-data" deleted
persistentvolume "pvc-53819848-09bf-4bc7-9a26-25e318b1f98e" deleted
```

## Delete the cluster

```
$ gcloud container clusters delete $CLUSTER_NAME \
  --zone=$CLUSTER_ZONE
```

# Running on Docker Compose

## Prepare the Configuration Volume

The block producer gets its keys from a volume that we prepare here

```
docker run --name=tmp -v bprod-keys:/keys centos
docker cp ~/cardano/keys/pool/kes.skey tmp:/keys/kes.skey
docker cp ~/cardano/keys/pool/vrf.skey tmp:/keys/vrf.skey
docker cp ~/cardano/keys/pool/node.cert tmp:/keys/node.cert
docker rm -f tmp
```

## Running the Relay + Block Producer

```
$ docker-compose -f nix/docker/compose/docker-compose.yaml up --detach

Creating relay ... done
Creating bprod ... done
```

## Stopping the Relay + Block Producer

```
$ docker-compose -f nix/docker/compose/docker-compose.yaml down

Stopping bprod ... done
Stopping relay ... done
Removing bprod ... done
Removing relay ... done
Removing network cardano
```
