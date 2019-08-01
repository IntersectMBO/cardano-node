#!/usr/bin/env bash

# build first:
#> cabal new-build --reorder-goals

# create tmux session:
#> tmux new-session -s 'Demo' -t demo

ALGO="--real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        --system-start "'${NOW}'"
        --slot-duration 2
        --genesis-file "configuration/Test.Cardano.Chain.Genesis.Dummy.dummyConfig.configGenesisData.json"
        --genesis-hash "fc32ebdf3c9bfa2ebf6bdcac98649f610601ddb266a2a2743e787dc9952a1aeb"
        node
        --topology "configuration/simple-topology.json"
        ${ALGO}
)
ACCARGS=(
        --system-start "'${NOW}'"
        --slot-duration 2
        trace-acceptor
)
#SCR="./scripts/start-node.sh"
#CMD="stack exec --nix cardano-node --"
CMD=`find dist-newstyle/ -type f -name "cardano-node"`
test -n "${CMD}" || {
        cabal new-build exe:cardano-node || {
                echo "ERROR: couldn't find or cabal new-build exe:cardano-node" >&2
                exit 1
        }
        CMD=`find dist-newstyle/ -type f -name "cardano-node"`
}
#SPECIAL=""
SPECIAL="--live-view"
HOST="127.0.0.1"

function mklogcfg () {
  echo "--log-config configuration/log-config-${1}.yaml"
}
function mkdlgkey () {
  printf -- "--signing-key configuration/delegate-keys.%03d.key" "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate configuration/delegation-cert.%03d.json" "$1"
}

tmux split-window -v
tmux select-pane -t 0
tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 4
tmux send-keys "${CMD} $(mklogcfg acceptor) $(mkdlgkey 0) $(mkdlgcert 0) ${ACCARGS[*]}" C-m
sleep 2
tmux select-pane -t 0
tmux send-keys "${CMD} $(mklogcfg 0) $(mkdlgkey 0) $(mkdlgcert 0) ${NETARGS[*]} -n 0 --host ${HOST} --port 3000 ${SPECIAL}" C-m
tmux select-pane -t 1
tmux send-keys "${CMD} $(mklogcfg 1) $(mkdlgkey 1) $(mkdlgcert 1) ${NETARGS[*]} -n 1 --host ${HOST} --port 3001 ${SPECIAL}" C-m
tmux select-pane -t 2
tmux send-keys "${CMD} $(mklogcfg 2) $(mkdlgkey 2) $(mkdlgcert 2) ${NETARGS[*]} -n 2 --host ${HOST} --port 3002 ${SPECIAL}" C-m

