#!/usr/bin/env bash

BASEPATH="$(realpath $(dirname $0))"

. "${BASEPATH}"/../../scripts/lib-node.sh "$(realpath "${BASEPATH}"/../..)"
NODE="$(executable_runner cardano-node)"

# VERBOSITY="--tracing-verbosity-minimal"
# VERBOSITY="--tracing-verbosity-normal"
VERBOSITY="--tracing-verbosity-maximal"

EXTRA=""

### prep cli arguments

function nodecfg () {
        printf -- "--config configuration/log-config-${1}.yaml "
}
function commonargs() {
        printf -- "--topology configuration/simple-topology.json "
        printf -- "--database-path ./db/ "
        printf -- "--genesis-file ${genesis_file} "
        printf -- "--genesis-hash ${genesis_hash} "
        printf -- "--socket-dir /tmp/cluster3nodes-socket/ "
}

function nodeargs () {
        local extra="$2"
        commonargs
        nodecfg $1
        dlgkey $1
        dlgcert $1
        printf -- "${extra} "
        printf -- "--port $((3000 + $1)) "
}

echo "Prebuilding the node.."
${NODE} --help || true

atexit() {
        rm -f "${script}"
}

case "$TERM" in
        linux | xterm )
                trap atexit EXIT
                script="$(mktemp)" && echo -e "
startup_message off
split
focus
split -v
focus
screen -t 'To exit:  C-a C-\   To scroll:  C-a C-ESC, then arrows/PgUp/PgDn' ${NODE} $(nodeargs 0 "$(echo -n ${EXTRA})")
focus
screen -t node-1 ${NODE} $(nodeargs 1 "$(echo -n ${EXTRA})")
focus
screen -t node-2 ${NODE} $(nodeargs 2 "$(echo -n ${EXTRA})")
" > "$script" && screen -c "$script"
                ;;
        * )
                ${NODE} $(nodeargs 0 "$(echo -n ${EXTRA})") &
                ${NODE} $(nodeargs 1 "$(echo -n ${EXTRA})") &
                ${NODE} $(nodeargs 2 "$(echo -n ${EXTRA})") &
                sleep 300
                ;; esac
