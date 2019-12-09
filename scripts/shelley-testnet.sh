#!/usr/bin/env bash

set -e

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

# start a tmux session:
# tmux new-session -s 'Demo' -t demo

# then run this script
# CMD="stack exec --nix --"
CMD="cabal v2-run --"

# VERBOSITY="--tracing-verbosity-minimal"
# VERBOSITY="--tracing-verbosity-normal"
VERBOSITY="--tracing-verbosity-maximal"

# EXTRA=""
EXTRA="
  --trace-block-fetch-decisions
  --trace-block-fetch-client
  --trace-block-fetch-server
  --trace-tx-inbound
  --trace-tx-outbound
  --trace-local-tx-submission-server
  --trace-mempool
  --trace-forge
  --trace-chain-sync-protocol
  --trace-block-fetch-protocol
  --trace-tx-submission-protocol
  --trace-local-chain-sync-protocol
  --trace-local-tx-submission-protocol
"

. $(dirname $0)/lib-node.sh

sleep 2

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
tmux send-keys "cd '${PWD}'; ${CMD} exe:cardano-node $(nodeargs 0 "${ALGO} $(echo -n ${EXTRA})") " C-m
tmux select-pane -t 2
tmux send-keys "cd '${PWD}'; ${CMD} exe:cardano-node $(nodeargs 1 "${ALGO} $(echo -n ${EXTRA})") " C-m
tmux select-pane -t 3
tmux send-keys "cd '${PWD}'; ${CMD} exe:cardano-node $(nodeargs 2 "${ALGO} $(echo -n ${EXTRA})") " C-m
