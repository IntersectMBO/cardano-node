#!/usr/bin/env bash

set -e

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

# start a tmux session:
# tmux new-session -s 'Demo' -t demo

# then run this script

# VERBOSITY="--tracing-verbosity-minimal"
# VERBOSITY="--tracing-verbosity-normal"
VERBOSITY="--tracing-verbosity-maximal"

EXTRA=""

. $(dirname $0)/lib-node.sh
NODE="$(executable_runner cardano-node)"

sleep 2

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
tmux send-keys "cd '${PWD}'; ${NODE} run $(nodeargs 0 '' "${ALGO} $(echo -n ${EXTRA})") " C-m
tmux select-pane -t 2
tmux send-keys "cd '${PWD}'; ${NODE} run $(nodeargs 1 '' "${ALGO} $(echo -n ${EXTRA})") " C-m
tmux select-pane -t 3
tmux send-keys "cd '${PWD}'; ${NODE} run $(nodeargs 2 '' "${ALGO} $(echo -n ${EXTRA})") " C-m
