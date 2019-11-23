#!/bin/sh

### parameters

# process
create_new_genesis=0
clean_explorer_db=1
run_cluster_nodes=1
run_explorer=1
run_tx_generator=1


### >>>>>> do not change anything below this point

# check for explorer binary
if [ ! -d cardano-explorer.git ]; then
  echo "cardano explorer missing. run the script 'prepare-explorer.sh' first."
  exit 1
fi

BASEDIR=$(realpath $(dirname $0))

# clean
if [ -d db ]; then rm -rf db; fi
if [ -d logs ]; then rm -rf logs; fi
if [ -d socket ]; then rm -rf socket; fi
if [ -d db-0 ]; then rm -rf db-0; fi
if [ -d db-1 ]; then rm -rf db-1; fi
if [ -d db-2 ]; then rm -rf db-2; fi

# mk dirs
mkdir -p db logs socket


# 1) generate new genesis
if [ $create_new_genesis -eq 1 ]; then
  ./genesis.sh
  read -p "Continue? (y|n)" answ
  case $answ in
    [Nn]) exit 1;;
    * ) echo continuing;;
  esac
fi


# 2) prepare SQL database
# (assuming user 'cexplorer' has been defined in the database system)

if [ $clean_explorer_db -eq 1 ]; then
  { . configuration/psql-settings.sh
    psql -d postgres -c "DROP DATABASE cexplorer;" || echo "DB missing"
    psql -d postgres -c "CREATE DATABASE cexplorer OWNER=cexplorer;" || echo "ignored"
    read -p "Continue? (y|n)" answ
    case $answ in
      [Nn]) exit 1;;
      * ) echo continuing;;
    esac
  }
fi


# 3) run cluster in tmux
if [ $run_cluster_nodes -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Nodes "./run-3node-cluster.sh; $SHELL"
  sleep 1
  tmux set-window-option remain-on-exit on
fi


# 4) run transaction generator
if [ $run_tx_generator -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n TxGen "./run_tx_generator.sh; $SHELL" 
  sleep 1
  tmux set-window-option remain-on-exit on
fi


# 5) run explorer
if [ $run_explorer -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Explorer "./run_explorer.sh; $SHELL"
  sleep 1
  tmux set-window-option remain-on-exit on
fi


# wait for transactions
tmux select-window -t :0
tmux new-window -n Analysis "\
  echo 'waiting for transactions...'\
  sleep 30\
  echo 'analyse data'\
  echo 'TPS - transactions per second'\
  echo 'TBPS - transaction bytes per second'\
  echo 'number of slots total'\
  echo 'number of slots per node'\
  echo 'number of missed slots'\
  echo 'distribution of block size in transactions'\
  echo 'distribution of block size in bytes'\
  echo 'distribution of seconds between blocks' \
  $SHELL"
sleep 1
tmux set-window-option remain-on-exit on

