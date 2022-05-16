usage_supervisor() {
     usage "supervisor" "Backend:  manages a local cluster using 'supervisord'" <<EOF

    Please see documentation for 'wb backend' for the supported commands.

    Supervisor-specific:

    save-child-pids RUN-DIR
    save-pid-maps RUN-DIR
EOF
}

backend_supervisor() {
op=${1:?$(usage_supervisor)}; shift

case "$op" in
    name )
        echo 'supervisor';;

    is-running )
        test "$(sleep 0.5s; netstat -pltn 2>/dev/null | grep ':9001 ' | wc -l)" != "0";;

    setenv-defaults )
        local usage="USAGE: wb supervisor $op PROFILE-DIR"
        local profile_dir=${1:?$usage}

        setenvjq    'port_shift_ekg'        100
        setenvjq    'port_shift_prometheus' 200
        setenvjqstr 'supervisor_conf'      "$profile_dir"/supervisor.conf
        ;;

    allocate-run )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        local supervisor_conf=$(envjqr 'supervisor_conf')

        mkdir -p               "$dir"/supervisor
        cp -f $supervisor_conf "$dir"/supervisor/supervisord.conf
        ;;

    describe-run )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}

        local basePort=$(                   envjq 'basePort')
        local port_ekg=$((       basePort+$(envjq 'port_shift_ekg')))
        local port_prometheus=$((basePort+$(envjq 'port_shift_prometheus')))

        cat <<EOF
  - EKG URL (node-0):        http://localhost:$port_ekg/
  - Prometheus URL (node-0): http://localhost:$port_prometheus/metrics
EOF
        ;;

    start-node )
        local usage="USAGE: wb supervisor $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        supervisorctl start $node
        backend_supervisor save-child-pids "$dir"
        ;;

    stop-node )
        local usage="USAGE: wb supervisor $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        supervisorctl stop $node
        ;;

    wait-node )
        local usage="USAGE: wb supervisor $op RUN-DIR [NODE-NAME]"
        local dir=${1:?$usage}; shift
        local node=${1:-$(dirname $CARDANO_NODE_SOCKET_PATH | xargs basename)}; shift

        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$(backend_supervisor get-node-socket-path "$dir" $node)
        fi

        local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' $dir/profile.json) i=0
        echo -n "workbench:  supervisor:  waiting ${patience}s for $CARDANO_NODE_SOCKET_PATH to appear: " >&2
        while test ! -S $CARDANO_NODE_SOCKET_PATH
        do printf "%3d" $i; sleep 1
           i=$((i+1))
           if test $i -ge $patience
           then echo
                msg "FATAL:  workbench:  supervisor:  patience ran out for $node after ${patience}s"
                backend_supervisor stop-cluster "$dir"
                fatal "$node startup did not succeed:  check logs in $(dirname $CARDANO_NODE_SOCKET_PATH)/stdout"
           fi
           echo -ne "\b\b\b"
        done >&2
        echo " $node online after $i seconds" >&2
        ;;

    start-cluster )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        supervisord --config  "$dir"/supervisor/supervisord.conf $@

        backend_supervisor wait-node "$dir" 'node-0'
        backend_supervisor save-child-pids "$dir"
        backend_supervisor save-pid-maps "$dir";;

    get-node-socket-path )
        local usage="USAGE: wb supervisor $op STATE-DIR NODE-NAME"
        local state_dir=${1:?$usage}
        local node_name=${2:?$usage}

        echo -n $state_dir/$node_name/node.socket
        ;;

    start-generator )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        supervisorctl start generator
        backend_supervisor save-child-pids "$dir";;

    wait-node-stopped )
        local usage="USAGE: wb supervisor $op RUN-DIR NODE"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        progress_ne "supervisor" "waiting until $node stops:  ....."
        local i=0
        while supervisorctl status $node > /dev/null
        do echo -ne "\b\b\b\b\b"; printf "%5d" $i >&2; i=$((i+1)); sleep 1
        done >&2
        echo -e "\b\b\b\b\bdone, after $(with_color white $i) seconds" >&2
        ;;

    wait-pools-stopped )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local i=0 pools=$(jq .composition.n_pool_hosts $dir/profile.json)
        msg_ne "supervisor:  waiting until all pool nodes are stopped: 000000"
        touch $dir/flag/cluster-termination
        for ((pool_ix=0; pool_ix < $pools; pool_ix++))
        do while supervisorctl status node-${pool_ix} > /dev/null &&
                   test -f $dir/flag/cluster-termination
           do echo -ne "\b\b\b\b\b\b"; printf "%6d" $i;          i=$((i+1)); sleep 1; done
              echo -ne "\b\b\b\b\b\b"; echo -n "node-${pool_ix} 000000"
        done >&2
        if test -f $dir/flag/cluster-termination
        then echo " done." >&2
        else echo " termination requested." >&2; fi
        ;;

    stop-cluster )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        supervisorctl stop all

        if test -f "${dir}/supervisor/supervisord.pid"
        then kill $(<${dir}/supervisor/supervisord.pid) $(<${dir}/supervisor/child.pids) 2>/dev/null
        else pkill supervisord
        fi
        ;;

    cleanup-cluster )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        msg "supervisor:  resetting cluster state in:  $dir"
        rm -f $dir/*/std{out,err} $dir/node-*/*.socket $dir/*/logs/* 2>/dev/null || true
        rm -fr $dir/node-*/state-cluster/;;

    save-child-pids )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local svpid=$dir/supervisor/supervisord.pid
        local pstree=$dir/supervisor/ps.tree
        pstree -p "$(cat "$svpid")" > "$pstree"

        local pidsfile="$dir"/supervisor/child.pids
        { grep '\\---\|--=' "$pstree" || true; } |
            sed 's/^.*\\--- \([0-9]*\) .*/\1/; s/^[ ]*[^ ]* \([0-9]+\) .*/\1/
                ' > "$pidsfile"
        ;;

    save-pid-maps )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local mapn2p=$dir/supervisor/node2pid.map; echo '{}' > "$mapn2p"
        local mapp2n=$dir/supervisor/pid2node.map; echo '{}' > "$mapp2n"
        local pstree=$dir/supervisor/ps.tree
        for node in $(jq_tolist keys "$dir"/node-specs.json)
        do local service_pid=$(supervisorctl pid $node)
           if   test $service_pid = '0'
           then continue
           elif test -z "$(ps h --ppid $service_pid)"
           then local pid=$service_pid
           else local pid=$(fgrep -e "= $(printf %05d $service_pid) " -A1 "$pstree" |
                                tail -n1 | sed 's/^.*\\--- \([0-9]*\) .*/\1/; s/^[ ]*[^ ]* \([0-9]*\) .*/\1/')
           fi
           jq_fmutate "$mapn2p" '. * { "'$node'": '$pid' }'
           jq_fmutate "$mapp2n" '. * { "'$pid'": "'$node'" }'
        done
        ;;

    * ) usage_supervisor;; esac
}
