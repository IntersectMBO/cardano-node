#!/bin/sh

### parameters

# the number of transactions to enter into the chain
numtx = 1000

# the transactions are sent to these nodes
targetnodes = "0 1 2"

# add bytes of data to every transaction
addsizetx = 0


### >>>>>> do not change anything below this point

mkdir -p db logs socket

# exit on first error
set -e

# generate new genesis

# prepare SQL database

# (assuming user 'cexplorer' has been defined in the database system)
. configuration/psql-settings.sh

psql -d postgres -c "DROP DATABASE cexplorer;"
psql -d postgres -c "CREATE DATABASE cexplorer OWNER=cexplorer;"


# run cluster in tmux
tmux new-s -s Cluster3Node "./run-3node-cluster.sh" 

# run transaction generator (parallel)
{
  sleep 15
  echo "running tx generator"
} &

# run explorer (parallel)
{
  sleep 18
  echo "running explorer"
} &


# wait for transactions
sleep 30
echo "waiting for transactions..."

# analyse benchmark run
sleep 5
echo "analyse data"

