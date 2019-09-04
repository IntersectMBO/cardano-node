#!/usr/bin/env bash

set -e

# build first:
#> cabal new-build --reorder-goals

# create tmux session:
#> tmux new-session -s 'Demo' -t demo

genesis="33873"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi

cabal new-build "exe:cardano-cli"
genesis_hash="$(cabal new-exec -- cardano-cli --real-pbft print-genesis-hash --genesis-json ${genesis_file})"

ALGO="--real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        --slot-duration 2
        --genesis-file "${genesis_file}"
        --genesis-hash "${genesis_hash}"
        --pbft-signature-threshold 0.7
        --require-network-magic
        --database-path "db"
        node
        --topology "configuration/simple-topology.json"
        ${ALGO}
)
ACCARGS=(
        --slot-duration 2
        trace-acceptor
)
# SCR="./scripts/start-node.sh"
# CMD="stack exec --nix cardano-node --"
CMD=`find dist-newstyle/ -type f -name "cardano-node"`
test -n "${CMD}" || {
        cabal new-build exe:cardano-node || {
                echo "ERROR: couldn't find or cabal new-build exe:cardano-node" >&2
                exit 1
        }
        CMD=`find dist-newstyle/ -type f -name "cardano-node"`
}
# SPECIAL=""
SPECIAL="--live-view"
HOST="127.0.0.1"
HOST6="::1"

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.yaml"
}
function mkdlgkey () {
  printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key" "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json" "$1"
}

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
tmux send-keys "cd '${PWD}'; ${CMD} $(mklogcfg acceptor) $(mkdlgkey 0) $(mkdlgcert 0) ${ACCARGS[*]}" C-m
sleep 2
tmux select-pane -t 0
tmux send-keys "cd '${PWD}'; ${CMD} $(mklogcfg 0) $(mkdlgkey 0) $(mkdlgcert 0) ${NETARGS[*]} -n 0 --host-addr ${HOST6} --port 3000 ${SPECIAL}" C-m
tmux select-pane -t 1
tmux send-keys "cd '${PWD}'; ${CMD} $(mklogcfg 1) $(mkdlgkey 1) $(mkdlgcert 1) ${NETARGS[*]} -n 1 --host-addr ${HOST}  --port 3001 ${SPECIAL}" C-m
tmux select-pane -t 2
tmux send-keys "cd '${PWD}'; ${CMD} $(mklogcfg 2) $(mkdlgkey 2) $(mkdlgcert 2) ${NETARGS[*]} -n 2 --host-addr ${HOST6} --port 3002 ${SPECIAL}" C-m
