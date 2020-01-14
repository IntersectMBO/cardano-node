#!/usr/bin/env bash

set -e

# build first:
#> cabal v2-build --reorder-goals

# create tmux session:
#> tmux new-session -s 'Demo' -t demo

EXTRA=""

. $(dirname $0)/lib-node.sh
NODE="$(executable_runner cardano-node)"

# for logs:
mkdir -p logs/

PWD=$(pwd)

#tmux split-window -v
#tmux select-pane -t 0
tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 1
tmux send-keys "cd '${PWD}'; ${NODE} $(nodeargs 0 '.liveview' "${ALGO} $(echo -n ${EXTRA})") " C-m
tmux select-pane -t 2
tmux send-keys "cd '${PWD}'; ${NODE} $(nodeargs 1 '.liveview' "${ALGO} $(echo -n ${EXTRA})") " C-m
tmux select-pane -t 3
tmux send-keys "cd '${PWD}'; ${NODE} $(nodeargs 2 '.liveview' "${ALGO} $(echo -n ${EXTRA})") " C-m
