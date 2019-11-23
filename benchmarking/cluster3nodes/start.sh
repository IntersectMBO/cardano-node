#!/usr/bin/env bash

tmux new-s -s Cluster3Nodes -n Main "./benchmark.sh"


sleep 8
# rotate between windows
tmux select-window -t :0
while [ -n "${TMUX}" ]; do
  tmux next-window
  sleep 4
done

