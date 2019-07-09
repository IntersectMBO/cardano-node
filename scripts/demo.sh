#!/usr/bin/env bash

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

# start a tmux session:
# tmux new-session -s 'Demo' -t demo

# then run this script


ALGO="--bft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS="--system-start \"${NOW}\" --slot-duration 2 node -t configuration/simple-topology.json ${ALGO}"
#SCR="./scripts/start-node.sh"
CMD="stack exec --nix cardano-node --"
HOST="127.0.0.1"

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.yaml"
}

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 0
tmux send-keys "${CMD} $(mklogcfg 0) ${NETARGS} -n 0 --host ${HOST} --port 3000 --live-view" C-m
tmux select-pane -t 1
tmux send-keys "${CMD} $(mklogcfg 1) ${NETARGS} -n 1 --host ${HOST} --port 3001 --live-view" C-m
tmux select-pane -t 2
tmux send-keys "${CMD} $(mklogcfg 2) ${NETARGS} -n 2 --host ${HOST} --port 3002 --live-view" C-m
