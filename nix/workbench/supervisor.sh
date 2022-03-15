usage_supervisor() {
     usage "supervisor" "Backend:  manages a local cluster using 'supervisord'" <<EOF
    is-running       Test if 'supervisord' is running

    get-node-socket-path RUN-DIR
                     Given a state dir, print the default node socket path
                       for 'cardano-cli'

    describe-run RUN-DIR
    allocate-run RUN-DIR
    start-cluster RUN-DIR
    start-generator RUN-DIR
    cleanup-cluster RUN-DIR
    wait-pools-stopped RUNDIR
    stop-cluster RUN-DIR

    Supervisor-specific:

    save-pids RUN-DIR
EOF
}

backend_supervisor() {
op=${1:?$(usage_supervisor)}; shift

case "$op" in
    name )
        echo 'supervisor';;

    is-running )
        test "$(sleep 0.5s; netstat -pltn 2>/dev/null | grep ':9001 ' | wc -l)" != "0";;

    get-node-socket-path )
        local usage="USAGE: wb supervisor $op STATE-DIR"
        local state_dir=${1:?$usage}

        echo -n $state_dir/node-0/node.socket
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

    start-cluster )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        supervisord --config  "$dir"/supervisor/supervisord.conf $@

        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$(backend_supervisor get-node-socket-path "$dir")
        fi

        local patience=$(jq .tolerances.cluster_startup_overhead_s $dir/profile.json) i=0
        echo -n "workbench:  supervisor:  waiting ${patience}s for $CARDANO_NODE_SOCKET_PATH to appear: " >&2
        while test ! -S $CARDANO_NODE_SOCKET_PATH
        do printf "%3d" $i; sleep 1
           i=$((i+1))
           if test $i -ge $patience
           then echo
                msg "FATAL:  workbench:  supervisor:  patience ran out after ${patience}s"
                backend_supervisor stop-cluster "$dir"
                fatal "node startup did not succeed:  check logs in $dir/node-0/stdout"
           fi
           echo -ne "\b\b\b"
        done >&2
        echo " node-0 online after $i seconds" >&2

        backend_supervisor save-pids "$dir";;

    start-generator )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        supervisorctl start generator;;

    cleanup-cluster )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        msg "supervisor:  resetting cluster state in:  $dir"
        rm -f $dir/*/std{out,err} $dir/node-*/*.socket $dir/*/logs/* 2>/dev/null || true
        rm -fr $dir/node-*/state-cluster/;;

    wait-pools-stopped )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local i=0 pools=$(jq .composition.n_pool_hosts $dir/profile.json)
        msg_ne "supervisor:  waiting until all pool nodes are stopped: 000000"
        for ((pool_ix=0; pool_ix < $pools; pool_ix++))
        do while supervisorctl status node-${pool_ix} > /dev/null
           do echo -ne "\b\b\b\b\b\b"; printf "%6d" $i;          i=$((i+1)); sleep 1; done
              echo -ne "\b\b\b\b\b\b"; echo -n "node-${pool_ix} 000000"
           done >&2
        echo " done." >&2;;

    stop-cluster )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        supervisorctl stop all

        if test -f "${dir}/supervisor/supervisord.pid"
        then kill $(<${dir}/supervisor/supervisord.pid) $(<${dir}/supervisor/cardano-node.pids) 2>/dev/null
        else pkill supervisord
        fi
        ;;

    save-pids )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local svpid=$dir/supervisor/supervisord.pid pstree=$dir/supervisor/ps.tree
        pstree -Ap "$(cat "$svpid")" > "$pstree"

        local pidsfile="$dir"/supervisor/cardano-node.pids
        { grep -e '-[{]\?cardano-node[}]\?([0-9]*)-' "$pstree" || fail 'save-pids: pattern not found';
        } | sed -e 's/^.*[+`|]-[{]\?cardano-node[}]\?(\([0-9]*\))-.*$/\1/' \
                > "$pidsfile"

        local mapn2p="$dir"/supervisor/node2pid.map; echo '{}' > "$mapn2p"
        local mapp2n="$dir"/supervisor/pid2node.map; echo '{}' > "$mapp2n"
        for node in $(jq_tolist keys "$dir"/node-specs.json)
        do local service_pid=$(supervisorctl pid $node)
           local pid=$(fgrep -e "($service_pid)-" "$pstree" |
                       sed -e 's/^.*-cardano-node(\([0-9]*\))-.*$/\1/')
           jq_fmutate "$mapn2p" '. * { "'$node'": '$pid' }'
           jq_fmutate "$mapp2n" '. * { "'$pid'": "'$node'" }'
        done

        msg "supervisor:  pid file:      $svpid"
        msg "supervisor:  process tree:  $pstree"
        msg "supervisor:  node pids:     $pidsfile"
        msg "supervisor:  node pid maps: $mapn2p $mapp2n"
        ;;

    lostream-fixup-jqargs )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}

        echo --compact-output --argjson mapp2n '[{}]';;# --slurpfile mapp2n "$dir"/supervisor/pid2node.map;;
        #echo --compact-output --slurpfile mapp2n "$dir"/supervisor/pid2node.map;;

    lostream-fixup-jqexpr )
        local usage="USAGE: wb supervisor $op"

        echo '| $mapp2n[0] as $map | . * { host: ($map[.pid] // $dirHostname) }';;

    * ) usage_supervisor;; esac
}
