#!/usr/bin/env bash

#tmux new-session -s 'Demo' -t demo

NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS="--system-start \"${NOW}\" --slot-duration 2 node -t configuration/simple-topology.json --bft"
#SCR="./scripts/start-node.sh"
CMD="cabal new-exec cardano-node --"

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.yaml"
}

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 0
tmux send-keys "${CMD} $(mklogcfg 0) ${NETARGS} -n 0" C-m
tmux select-pane -t 1
tmux send-keys "${CMD} $(mklogcfg 1) ${NETARGS} -n 1" C-m
tmux select-pane -t 2
tmux send-keys "${CMD} $(mklogcfg 2) ${NETARGS} -n 2" C-m

