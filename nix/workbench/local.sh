usage_local() {
     usage "local" "Managing local cluster" <<EOF
    assert-stopped   Assert that 'supervisord' is not running
    supervisord-running
                     Test if 'supervisord' is running

    get-node-socket-path STATE-DIR
                     Given a state dir, print the default node socket path
                       for 'cardano-cli'

    wait-for-node-socket
                     Given a state dir, wait until a node socket appears

    record-node-pids STATE-DIR
                     Given a state dir, record node pids
EOF
}

local_() {
local op=${1:-$(usage_local)}; shift

case "${op}" in
    assert-stopped )
        local_ supervisord-running &&
          fail "supervisord is already running. Please run 'stop-cluster' first!" ||
          true
        ;;
    supervisord-running )
        test "$(sleep 0.5s; netstat -pltn 2>/dev/null | grep ':9001 ' | wc -l)" != "0"
        ;;

    get-node-socket-path )
        local usage="USAGE: wb local get-node-socket-path STATE-DIR"
        local state_dir=${1:?$usage}

        echo -n $state_dir/node-0/node.socket
        ;;

    wait-for-node-socket )
        while test ! -S $CARDANO_NODE_SOCKET_PATH
        do msg "waiting 5 seconds for $CARDANO_NODE_SOCKET_PATH to appear.."
           sleep 5
        done
        ;;

    record-node-pids )
        local usage="USAGE: wb local record-node-pids STATE-DIR"
        local state_dir=${1:?$usage}

        msg 'recording node pids..'
        pstree -Ap "$(cat $state_dir/supervisor/supervisord.pid)" |
            grep 'cabal.*cardano-node' |
            sed -e 's/^.*-+-cardano-node(\([0-9]*\))-.*$/\1/' \
                > $state_dir/supervisor/cardano-node.pids
        ;;

    * ) usage_local;; esac
}
