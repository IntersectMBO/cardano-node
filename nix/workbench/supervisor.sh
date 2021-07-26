usage_supervisor() {
     usage "supervisor" "Backend:  manages a local cluster using 'supervisord'" <<EOF
    is-running       Test if 'supervisord' is running

    get-node-socket-path RUN-DIR
                     Given a state dir, print the default node socket path
                       for 'cardano-cli'

    record-extended-env-config ENV-JSON [ENV-CONFIG-OPTS..]
                     Extend the env JSON with the backend-specific
                       environment config options:
                         --port-shift-ekg INT
                         --port-shift-prometheus INT
                         --supervisor-conf FILE

    describe-run RUN-DIR
    allocate-run RUN-DIR
    start-cluster RUN-DIR
    start-generator RUN-DIR
    cleanup-cluster RUN-DIR
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

    record-extended-env-config )
        local usage="USAGE: wb supervisor $op ENV-JSON [ENV-CONFIG-OPTS..]"
        local env_json=${1:?$usage}; shift

        local port_shift_ekg=200
        local port_shift_prometheus=300
        local supervisor_conf=
        while test $# -gt 0
        do case "$1" in
               --port-shift-ekg )        port_shift_ekg=$2; shift;;
               --port-shift-prometheus ) port_shift_prometheus=$2; shift;;
               --supervisor-conf )       supervisor_conf=$2; shift;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        test -r "$supervisor_conf" ||
            fatal "supervisor record-extended-env-config requires the --supervisor-conf FILE option to point to a readable file, but it was:  $supervisor_conf"

        local env_json="$env_json"
        local args=(
            --argjson port_shift_ekg        "$port_shift_ekg"
            --argjson port_shift_prometheus "$port_shift_prometheus"
            --arg     supervisor_conf       "$supervisor_conf"
        )
        jq_fmutate "$env_json" '. *
          { type:                  "supervisor"
          , port_shift_ekg:        $port_shift_ekg
          , port_shift_prometheus: $port_shift_prometheus
          , supervisor_conf:       $supervisor_conf
          }
        ' "${args[@]}"
        ;;

    describe-run )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}

        local basePort=$(jq .basePort "$dir"/env.json)
        local port_ekg=$((       basePort+$(jq .port_shift_ekg        "$dir"/env.json)))
        local port_prometheus=$((basePort+$(jq .port_shift_prometheus "$dir"/env.json)))

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

        local supervisor_conf=$(jq -r .supervisor_conf "$dir"/env.json)

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

        echo -n "workbench:  supervisor:  waiting for $CARDANO_NODE_SOCKET_PATH to appear: " >&2
        while test ! -S $CARDANO_NODE_SOCKET_PATH
        do echo -n '.'; sleep 1
        done >&2
        echo >&2

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

    stop-cluster )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        supervisorctl stop all

        if test -f "${dir}/supervisor/cardano-node.pids"
        then kill $(<${dir}/supervisor/supervisord.pid) $(<${dir}/supervisor/cardano-node.pids)
        else pkill supervisord
        fi
        rm -f ${dir}/supervisor/supervisord.pid ${dir}/supervisor/cardano-node.pids
        ;;

    save-pids )
        local usage="USAGE: wb supervisor $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local svpid=$dir/supervisor/supervisord.pid pstree=$dir/supervisor/ps.tree
        pstree -Ap "$(cat "$svpid")" > "$pstree"

        local pidsfile="$dir"/supervisor/cardano-node.pids
        { fgrep '+-{cardano-node}' "$pstree" || fail 'save-pids: pattern not found';
        } | sed -e 's/^.*-+-cardano-node(\([0-9]*\))-.*$/\1/' \
                > "$pidsfile"

        local mapn2p="$dir"/supervisor/node2pid.map; echo '{}' > "$mapn2p"
        local mapp2n="$dir"/supervisor/pid2node.map; echo '{}' > "$mapp2n"
        for node in $(jq_tolist keys "$dir"/node-specs.json)
        do local cabalpid=$(supervisorctl pid $node)
           local pid=$(fgrep -e "-cabal($cabalpid)-" "$pstree" |
                       sed -e 's/^.*-+-cardano-node(\([0-9]*\))-.*$/\1/')
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
