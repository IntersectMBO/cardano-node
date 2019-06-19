#!/usr/bin/env bash

#tmux new-session -s 'Demo' -t demo

ALGO="--bft"
SCR="./scripts/start-node.sh"
CMD="${SCR} ${ALGO}"

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 0
tmux send-keys "${CMD} -n 0" C-m
tmux select-pane -t 1
tmux send-keys "${CMD} -n 1" C-m
tmux select-pane -t 2
tmux send-keys "${CMD} -n 2" C-m

