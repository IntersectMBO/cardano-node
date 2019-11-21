#!/usr/bin/env bash

set -e

# build first:
#> cabal new-build --reorder-goals

# create tmux session:
#> tmux new-session -s 'Demo' -t demo

# CMD="stack exec --nix cardano-node --"
CMD="cabal new-run --"
# EXTRA="--live-view"
EXTRA=""

. $(dirname $0)/lib-node.sh

# for acceptor logs:
mkdir -p logs/

PWD=$(pwd)

tmux split-window -v
tmux select-pane -t 0
tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 4
tmux send-keys "cd '${PWD}'; ${CMD} trace-acceptor $(acceptorargs)" C-m
sleep 2
tmux select-pane -t 1
tmux send-keys "cd '${PWD}'; ${CMD} exe:cardano-node $(nodeargs 0 " ${EXTRA}")" C-m
tmux select-pane -t 2
tmux send-keys "cd '${PWD}'; ${CMD} exe:cardano-node $(nodeargs 1 " ${EXTRA}")" C-m
tmux select-pane -t 3
tmux send-keys "cd '${PWD}'; ${CMD} exe:cardano-node $(nodeargs 2 " ${EXTRA}")" C-m
