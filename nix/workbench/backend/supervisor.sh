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
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}
        test "$(sleep 0.5s
                netstat -pltn 2>/dev/null |
                grep ':9001 '             |
                wc -l)" = "0" ||
            echo 'supervisord'
        # `pgrep` piped to `wc -l` instead "--count" to make it Mac comptible
        # Also only shorthand options: like `-x` instead of `--exact`
        for exe in 'cardano-node' 'tx-generator' 'cardano-tracer'
        do test $(pgrep -x $exe | wc -l)   = 0 || echo $exe
        done
        ;;

    setenv-defaults )
        local usage="USAGE: wb backend $op BACKEND-DIR"
        local backend_dir=${1:?$usage}

        setenvjq    'port_shift_ekg'        100
        setenvjq    'port_shift_prometheus' 200
        setenvjq    'port_shift_rtview'     300
        setenvjqstr 'supervisor_conf'      "$backend_dir"/supervisor.conf
        ;;

    allocate-run )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        local supervisor_conf=$(envjqr 'supervisor_conf')

        mkdir -p               "$dir"/supervisor
        cp -f $supervisor_conf "$dir"/supervisor/supervisord.conf

        local svcs=$dir/profile/node-services.json
        local gtor=$dir/profile/generator-service.json
        local trac=$dir/profile/tracer-service.json
        local hche=$dir/profile/healthcheck-service.json

        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do local node_dir="$dir"/$node
           mkdir -p                                          "$node_dir"
           cp $(jq '."'"$node"'"."start"'          -r $svcs) "$node_dir"/start.sh
           cp $(jq '."'"$node"'"."config"'         -r $svcs) "$node_dir"/config.json
           cp $(jq '."'"$node"'"."topology"'       -r $svcs) "$node_dir"/topology.json
        done

        local gen_dir="$dir"/generator
        mkdir -p                                              "$gen_dir"
        cp $(jq '."start"'                         -r $gtor)  "$gen_dir"/start.sh
        cp $(jq '."config"'                        -r $gtor)  "$gen_dir"/run-script.json
        cp $(jq '."plutus-redeemer"'               -r $gtor)  "$gen_dir"/plutus-redeemer.json
        cp $(jq '."plutus-datum"'                  -r $gtor)  "$gen_dir"/plutus-datum.json

        local trac_dir="$dir"/tracer
        mkdir -p                                    "$trac_dir"
        cp $(jq '."start"'                        -r $trac) "$trac_dir"/start.sh
        cp $(jq '."config"'                        -r $trac) "$trac_dir"/config.json

        local hche_dir="$dir"/healthcheck
        mkdir -p                                    "$hche_dir"
        cp $(jq '."start"'                        -r $hche) "$hche_dir"/start.sh
        ;;

    deploy-genesis )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift
        ;;

    describe-run )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}

        local basePort=$(                   envjq 'basePort')
        local port_ekg=$((       basePort+$(envjq 'port_shift_ekg')))
        local port_prometheus=$((basePort+$(envjq 'port_shift_prometheus')))
        local port_rtview=$((    basePort+$(envjq 'port_shift_rtview')))

        cat <<EOF
  - RTView URL:              http://localhost:$port_rtview
  - EKG URL (node-0):        http://localhost:$port_ekg/
  - Prometheus URL (node-0): http://localhost:$port_prometheus/metrics
EOF
        ;;

    start-node )
        local usage="USAGE: wb backend $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        supervisorctl      start                  $node
        backend_supervisor wait-node       "$dir" $node
        backend_supervisor save-child-pids "$dir"
        ;;

    stop-node )
        local usage="USAGE: wb backend $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        supervisorctl stop $node
        ;;

    wait-node )
        local usage="USAGE: wb backend $op RUN-DIR [NODE-NAME]"
        local dir=${1:?$usage}; shift
        local node=${1:-$(dirname $CARDANO_NODE_SOCKET_PATH | xargs basename)}; shift
        local socket=$(backend_supervisor get-node-socket-path "$dir" $node)

        local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' $dir/profile.json) i=0
        echo -n "workbench:  supervisor:  waiting ${patience}s for socket of $node: " >&2
        while test ! -S $socket
        do printf "%4d" $i; sleep 1
           i=$((i+1))
           if test $i -ge $patience
           then echo
                progress "supervisor" "$(red FATAL):  workbench:  supervisor:  patience ran out for $(white $node) after ${patience}s, $(blue socket) $(red $socket)"
                backend_supervisor stop-cluster "$dir"
                fatal "$node startup did not succeed:  check logs in $(white $(dirname $socket)/stdout) $(red \&) $(white stderr)"
           fi
           echo -ne "\b\b\b\b"
        done >&2
        echo " $node up (${i}s)" >&2
        ;;

    start-nodes )
        local usage="USAGE: wb backend $op RUN-DIR [HONOR_AUTOSTART=]"
        local dir=${1:?$usage}; shift
        local honor_autostart=${1:-yes-please}

        local nodes=($(jq_tolist keys "$dir"/node-specs.json))

        if test -n "$honor_autostart"
        then for node in ${nodes[*]}
             do jqtest ".\"$node\".autostart" "$dir"/node-specs.json &&
                     supervisorctl start $node &
             done
             wait
        else supervisorctl start ${nodes[*]}; fi

        for node in ${nodes[*]}
        do jqtest ".\"$node\".autostart" "$dir"/node-specs.json &&
                backend_supervisor wait-node "$dir" $node; done

        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$(backend_supervisor get-node-socket-path "$dir" 'node-0')
        fi

        backend_supervisor save-child-pids "$dir"
        backend_supervisor save-pid-maps   "$dir"
        ;;

    start-cluster )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        # Make sure it never runs in unbuffered mode:
        # https://docs.python.org/3/using/cmdline.html#envvar-PYTHONUNBUFFERED
        if ! PYTHONUNBUFFERED="" supervisord --config "$dir"/supervisor/supervisord.conf $@ >"$dir"/supervisor/stderr 2>"$dir"/supervisor/stderr
        then progress "supervisor" "$(red fatal: failed to start) $(white supervisord)"
             echo "$(red supervisord.conf) --------------------------------" >&2
             cat "$dir"/supervisor/supervisord.conf
             echo "$(red supervisord.log) ---------------------------------" >&2
             cat "$dir"/supervisor/supervisord.log
             echo "$(white -------------------------------------------------)" >&2
             fatal "could not start $(white supervisord)"
        fi;;

    start-tracers )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        if jqtest ".node.tracer" "$dir"/profile.json
        then if ! supervisorctl start tracer
             then progress "supervisor" "$(red fatal: failed to start) $(white cardano-tracer)"
                  echo "$(red config.json) -------------------------------------" >&2
                  cat "$dir"/tracer/config.json
                  echo "$(red tracer stdout) -----------------------------------" >&2
                  cat "$dir"/tracer/stdout
                  echo "$(red tracer stderr) -----------------------------------" >&2
                  cat "$dir"/tracer/stderr
                  echo "$(white -------------------------------------------------)" >&2
                  fatal "could not start $(white cardano-tracer)"
             fi

             progress_ne "supervisor" "waiting for $(yellow cardano-tracer) to create socket: "
             while test ! -e "$dir"/tracer/tracer.socket; do sleep 1; done
             echo $(green ' OK') >&2
             backend_supervisor save-child-pids "$dir"
        fi;;

    get-node-socket-path )
        local usage="USAGE: wb backend $op STATE-DIR NODE-NAME"
        local state_dir=${1:?$usage}
        local node_name=${2:?$usage}

        echo -n $state_dir/$node_name/node.socket
        ;;

    start-healthchecks )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        ls -l $dir/{tracer/tracer,node-{0,1}/node}.socket || true
        if ! supervisorctl start healthcheck
        then progress "supervisor" "$(red fatal: failed to start) $(white healthcheck)"
             echo "$(red healthcheck stdout) -----------------------------------" >&2
             cat "$dir"/healthcheck/stdout
             echo "$(red healthcheck stderr) -----------------------------------" >&2
             cat "$dir"/healthcheck/stderr
             echo "$(white -------------------------------------------------)" >&2
             fatal "could not start $(white supervisord)"
        fi
        backend_supervisor save-child-pids "$dir";;

    start-generator )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_supervisor;;
               * ) break;; esac; shift; done

        ls -l $dir/{tracer/tracer,node-{0,1}/node}.socket || true
        if ! supervisorctl start generator
        then progress "supervisor" "$(red fatal: failed to start) $(white generator)"
             echo "$(red run-script.json) ------------------------------------" >&2
             cat "$dir"/generator/run-script.json
             echo "$(red generator stdout) -----------------------------------" >&2
             cat "$dir"/generator/stdout
             echo "$(red generator stderr) -----------------------------------" >&2
             cat "$dir"/generator/stderr
             echo "$(white -------------------------------------------------)" >&2
             fatal "could not start $(white supervisord)"
        fi
        backend_supervisor save-child-pids "$dir";;

    wait-node-stopped )
        local usage="USAGE: wb backend $op RUN-DIR NODE"
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
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local pools=$(jq .composition.n_pool_hosts "${dir}"/profile.json)
        local start_time=$(date +%s)
        msg_ne "supervisor:  waiting until all pool nodes are stopped: 000000"
        for ((pool_ix=0; pool_ix < ${pools}; pool_ix++))
        do
            while \
                ! test -f "${dir}"/flag/cluster-stopping \
                && \
                supervisorctl status "node-${pool_ix}" > /dev/null
            do
                echo -ne "\b\b\b\b\b\b"
                printf "%6d" "$(($(date +%s) - start_time))"
                sleep 1
            done
            if ! test -f "${dir}"/flag/cluster-stopping
            then
                echo -ne "\b\b\b\b\b\b"
                echo -n "node-${pool_ix} 000000"
            fi
        done >&2
        echo -ne "\b\b\b\b\b\b"
        local elapsed=$(($(date +%s) - start_time))
        if test -f "${dir}"/flag/cluster-stopping
        then
            echo " Termination requested -- after $(yellow ${elapsed})s" >&2
        else
            touch "${dir}"/flag/cluster-stopping
            echo " All nodes exited      -- after $(yellow ${elapsed})s" >&2
        fi
        ;;

    stop-all )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        supervisorctl stop all || true
        ;;

    fetch-logs )
        # Unlike Nomad local or cloud, nothing to do here, logs are already in
        # the run directory.
        ;;

    stop-cluster )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        # This flag/file makes `scenario_watcher` exit
        touch ${dir}/flag/cluster-stopping
        if test -f ${dir}/supervisor/supervisord.pid -a \
                -f ${dir}/supervisor/child.pids
        then kill $(<${dir}/supervisor/supervisord.pid) $(<${dir}/supervisor/child.pids) 2>/dev/null
        else pkill supervisord
        fi
        ;;

    cleanup-cluster )
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift

        msg "supervisor:  resetting cluster state in:  $dir"
        rm -f $dir/*/std{out,err} $dir/*/exit_code $dir/node-*/*.socket $dir/*/logs/* 2>/dev/null || true
        rm -fr $dir/node-*/state-cluster/;;

    save-child-pids )
        local usage="USAGE: wb backend pass $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local svpid=$dir/supervisor/supervisord.pid
        local pstree=$dir/supervisor/ps.tree
        pstree -p "$(cat "$svpid")" > "$pstree"

        local pidsfile="$dir"/supervisor/child.pids
        { grep -e '---\|--=' "$pstree" || true; } |
          sed 's/^.*--[=-] \([0-9]*\) .*/\1/; s/^[ ]*[^ ]* \([0-9]+\) .*/\1/
              ' > "$pidsfile"
        ;;

    save-pid-maps )
        local usage="USAGE: wb backend pass $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local mapn2p=$dir/supervisor/node2pid.map; echo '{}' > "$mapn2p"
        local mapp2n=$dir/supervisor/pid2node.map; echo '{}' > "$mapp2n"
        local pstree=$dir/supervisor/ps.tree

        for node in $(jq_tolist keys "$dir"/node-specs.json)
        do ## supervisord's service PID is the immediately invoked binary,
           ## ..which isn't necessarily 'cardano-node', but could be 'time' or 'cabal' or..
           local service_pid=$(supervisorctl pid $node)
           if   test $service_pid = '0'
           then continue
           elif test -z "$(ps h --ppid $service_pid)" ## Any children?
           then local pid=$service_pid ## <-=^^^ none, in case we're running executables directly.
                ## ..otherwise, it's a chain of children, e.g.: time -> cabal -> cardano-node
           else local pid=$(grep -e "[=-] $(printf %05d $service_pid) " -A5 "$pstree" |
                            grep -e '---\|--=' |
                            head -n1 |
                            sed 's/^.*--[=-] \([0-9]*\) .*/\1/;
                                 s/^[ ]*[^ ]* \([0-9]*\) .*/\1/')
           fi
           if test -z "$pid"
           then warn "supervisor" "failed to detect PID of $(white $node)"; fi
           jq_fmutate "$mapn2p" '. * { "'$node'": '$pid' }'
           jq_fmutate "$mapp2n" '. * { "'$pid'": "'$node'" }'
        done
        ;;

    * ) usage_supervisor;; esac
}

###############################################################################
# Debugging ###################################################################
###############################################################################

if test -n "${SUPERVISOR_DEBUG:-}"
then
  exec 5> "$(dirname "$(readlink -f "$0")")"/supervisor.sh.debug
  BASH_XTRACEFD="5"
  PS4='$LINENO: '
  set -x
fi

debugMsg() {
  if test -n "${SUPERVISOR_DEBUG:-}"
  then
    >&2 echo -e "\n\n\t\t----------$1\n"
  fi
}
