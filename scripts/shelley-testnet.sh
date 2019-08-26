#!/usr/bin/env bash

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

# start a tmux session:
# tmux new-session -s 'Demo' -t demo

# then run this script


ALGO="--real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
GENHASH="33873aeaf8a47fefc7c2ea3f72e98a04459e07ec3edfb63c9ca709f540f69503"
NETARGS=(
        --slot-duration 2
        --genesis-file "configuration/${GENHASH:0:5}/genesis.json"
        --genesis-hash "${GENHASH}"
        --pbft-signature-threshold 0.7
        --require-network-magic
        node
        --topology "configuration/simple-topology.json"
        ${ALGO}
)

# SCR="./scripts/start-node.sh"
# CMD="stack exec --nix cardano-node --"
CMD="cabal new-exec cardano-node --"

# SPECIAL=""
SPECIAL="--live-view"
# VERBOSITY="--tracing-verbosity-minimal" 
# VERBOSITY="--tracing-verbosity-normal" 
VERBOSITY="--tracing-verbosity-maximal" 
HOST="127.0.0.1"
HOST6="::1"

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.yaml"
}
function mkdlgkey () {
  printf -- "--signing-key configuration/${GENHASH:0:5}/delegate-keys.%03d.key" "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate configuration/${GENHASH:0:5}/delegation-cert.%03d.json" "$1"
}

# for acceptor logs:
mkdir -p logs/

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 0
tmux send-keys "${CMD} $(mklogcfg 0) $(mkdlgkey 0) $(mkdlgcert 0) ${NETARGS[*]} -n 0 --host-addr ${HOST6} --port 3000 ${SPECIAL} ${VERBOSITY}" C-m
tmux select-pane -t 1
tmux send-keys "${CMD} $(mklogcfg 1) $(mkdlgkey 1) $(mkdlgcert 1) ${NETARGS[*]} -n 1 --host-addr ${HOST}  --port 3001 ${SPECIAL} ${VERBOSITY}" C-m
tmux select-pane -t 2
tmux send-keys "${CMD} $(mklogcfg 2) $(mkdlgkey 2) $(mkdlgcert 2) ${NETARGS[*]} -n 2 --host-addr ${HOST6} --port 3002 ${SPECIAL} ${VERBOSITY}" C-m
