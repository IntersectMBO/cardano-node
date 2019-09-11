#!/usr/bin/env bash

# requires a local dns server which resolves
# * local6.iohk.io to ::1
# * local.iohk.io  to 127.0.0.1
# You can use `unbound` with this configuration put in `/etc/unbound/unbound.conf`
# ```
# server:
#   verbosity: 1
#   local-data: "local.iohk.io A 127.0.0.1"
#   local-data: "local6.iohk.io AAAA ::1"
# ```

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

# start a tmux session:
# tmux new-session -s 'Demo' -t demo

# then run this script


ALGO="--real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS="--slot-duration 2 node -t configuration/simple-topology-dns.json ${ALGO}"
#SCR="./scripts/start-node.sh"
#CMD="stack exec --nix cardano-node --"
CMD="cabal new-run exe:cardano-node --"
HOST="127.0.0.1"
HOST6="::1"

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.yaml"
}

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
# tmux split-window -v

tmux select-pane -t 0
tmux send-keys "${CMD} $(mklogcfg 0) ${NETARGS} -n 0 --host-addr ${HOST6} --port 3000 --live-view" C-m
tmux select-pane -t 1
tmux send-keys "${CMD} $(mklogcfg 1) ${NETARGS} -n 1 --host-addr ${HOST}  --port 3001 --live-view" C-m
tmux select-pane -t 2
tmux send-keys "${CMD} $(mklogcfg 2) ${NETARGS} -n 2 --host-addr ${HOST6} --port 3002 --live-view" C-m
