#!/usr/bin/env bash

#tmux new-session -s 'Demo' -t demo

ALGO="--bft"
SCR="./scripts/start-node.sh"
CMD="${SCR} ${ALGO}"

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.log"
}

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 0
tmux send-keys "${CMD} $(mklogcfg 0) -n 0" C-m
tmux select-pane -t 1
tmux send-keys "${CMD} $((mklogcfg 1)) -n 1" C-m
tmux select-pane -t 2
tmux send-keys "${CMD} $(mklogcfg 2) -n 2" C-m

