#!/bin/sh

### parameters

# process
create_new_genesis=0
clean_explorer_db=1
run_cluster_nodes=1
run_explorer=1
run_tx_generator=0

# the number of transactions to enter into the chain
numtx=1000

# the transactions are sent to these nodes
targetnodes="0 1 2"

# add bytes of data to every transaction
addsizetx=0


### >>>>>> do not change anything below this point

# check for explorer binary
if [ ! -d cardano-explorer.git ]; then
  echo "cardano explorer missing. run the script 'prepare-explorer.sh' first."
  exit 1
fi

BASEDIR=$(realpath $(dirname $0))
mkdir -p db logs socket

# exit on first error
set -e


# generate new genesis
if [ $create_new_genesis -eq 1 ]; then
  ./genesis.sh
fi


# prepare SQL database
# (assuming user 'cexplorer' has been defined in the database system)
. configuration/psql-settings.sh

psql -d postgres -c "DROP DATABASE cexplorer;" || echo "DB missing"
psql -d postgres -c "CREATE DATABASE cexplorer OWNER=cexplorer;"


# run cluster in tmux
if [ $run_cluster_nodes -eq 1 ]; then
  tmux new-s -s Cluster3Node "./run-3node-cluster.sh" 
fi

exit 0


# run transaction generator (parallel)
{
  if [ $run_tx_generator -eq 1]; then
    sleep 15
    echo "running tx generator"
    cabal new-run exe:
  fi
} &


# run explorer (parallel)
{
  if [ $run_explorer -eq 1]; then
    sleep 14
    echo "running explorer"
    GENESISHASH=`cat configuration/latest-genesis/GENHASH`
    GENESISJSON="configuration/latest-genesis/genesis.json"
    cd cardano-explorer.git
    EXPLORER="cabal new-run exe:cardano-explorer-node -- "
    exec ${EXPLORER} \
      --genesis-hash ${GENESISHASH}\
      --genesis-file ${GENESISJSON} \
      --log-config ${BASEDIR}/cardano-explorer.git/log-configuration.yaml \
      --socket-path ${BASEDIR}/socket/node-0.socket \
      --schema-dir ${BASEDIR}/cardano-explorer.git/schema
      --network pbftbm
  fi
} &


# wait for transactions
sleep 30
echo "waiting for transactions..."

# analyse benchmark run
sleep 5
echo "analyse data"

echo "TPS - transactions per second"
echo "TBPS - transaction bytes per second"
echo "number of slots total"
echo "number of slots per node"
echo "number of missed slots"
echo "distribution of block size in transactions"
echo "distribution of block size in bytes"
echo "distribution of seconds between blocks"

