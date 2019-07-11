#!/usr/bin/env bash

# build first:
#> cabal new-build --reorder-goals

# create tmux session:
#> tmux new-session -s 'Demo' -t demo

ALGO="--real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS="--system-start '${NOW}' --slot-duration 2 node -t configuration/simple-topology.json ${ALGO}"
ACCARGS="--system-start '${NOW}' --slot-duration 2 trace-acceptor"
#SCR="./scripts/start-node.sh"
#CMD="stack exec --nix cardano-node --"
CMD=`find dist-newstyle/ -type f -name "cardano-node"`
#SPECIAL=""
SPECIAL="--live-view"
HOST="127.0.0.1"

ENVVARS="TERM=xterm-256color"
export TERM=xterm-256color

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.yaml"
}

tmux split-window -v
tmux select-pane -t 0
tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 4
tmux send-keys "${ENVVARS} ${CMD} $(mklogcfg acceptor) ${ACCARGS}" C-m
sleep 2
tmux select-pane -t 0
tmux send-keys "${ENVVARS} ${CMD} $(mklogcfg 0) ${NETARGS} -n 0 --host ${HOST} --port 3000 ${SPECIAL}" C-m
tmux select-pane -t 1
tmux send-keys "${ENVVARS} ${CMD} $(mklogcfg 1) ${NETARGS} -n 1 --host ${HOST} --port 3001 ${SPECIAL}" C-m
tmux select-pane -t 2
tmux send-keys "${ENVVARS} ${CMD} $(mklogcfg 2) ${NETARGS} -n 2 --host ${HOST} --port 3002 ${SPECIAL}" C-m

