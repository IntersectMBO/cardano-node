usage_scenario() {
     usage "scenario" "Run scenario control" <<EOF
    $(helpcmd idle DIR)              Isolated cluster, no workload, runs indefinitely

    $(helpcmd fixed DIR)             Isolated cluster;
                            Terminates at profile-implied conditions

    $(helpcmd fixed-loaded DIR)      Isolated cluster under tx-generator workload;
                            Terminates after profile-implied transaction
                            amount is submitted, or other condition satisfied

    $(helpcmd chainsync DIR)         Chain syncing:
                            1. start the preset-defined chaindb-server node,
                               feeding it a generated chaindb
                            2. start the fetcher node, connected to the chaindb-server

EOF
}

scenario() {
local op=${1:---help}; shift || true
local usage="USAGE: wb scenario DIR [SCENARIO-OP] OP-ARGS.."
local dir=${1:-}
local p=$dir/profile.json

if test -z "$dir"
then op=--help
else progress "run | scenario" "starting $(yellow $op)"
fi

case "$op" in
    idle )
        backend start-tracers        "$dir"

        scenario_setup_exit_trap              "$dir"
        # Trap start
        ############
        backend start-nodes          "$dir"
        # Trap end
        ##########
        scenario_cleanup_termination

        backend stop-all             "$dir"
        ;;

    tracer-only )
        backend start-tracers        "$dir"
        ;;

    fixed )
        backend start-tracers        "$dir"

        scenario_setup_exit_trap              "$dir"
        scenario_setup_workload_termination   "$dir"
        # Trap start
        ############
        backend start-nodes          "$dir"
        backend wait-pools-stopped   "$dir"
        # Trap end
        ##########
        scenario_cleanup_termination

        backend stop-all             "$dir"
        ;;

    fixed-loaded )
        backend start-tracers        "$dir"

        scenario_setup_exit_trap     "$dir"
        # Trap start
        ############
        backend start-nodes          "$dir"
        backend start-generator      "$dir"
        backend start-healthchecks   "$dir"
        scenario_setup_workload_termination   "$dir"
        # Trap end
        ##########

        backend wait-pools-stopped   "$dir"
        scenario_cleanup_termination

        backend stop-all             "$dir"
        ;;

    chainsync )
        # When using the nomad backend `chaindb` must be called after
        # `backend start` because the node-#, generator and tracer directories
        # may be created after the nomad job has started (are symlinks to the
        # containers directories).
        backend start-tracers "$dir"

        # `chaindb` explorer:
        local explorer=(
            mainnet-chunks-with-snapshot-at-slot
            "$dir"/node-1/db
            $(jq '.chaindb.ledger_snapshot.explorer'       $p)
            $(jq '.chaindb.mainnet_chunks.explorer'        $p)
        )
        progress "scenario" "preparing ChainDB for the $(green "explorer (fetcher)")"
        chaindb "${explorer[@]}"
        # `chaindb` server:
        local chaindb_server=(
            mainnet-chunks-with-snapshot-at-slot
            "$dir"/node-0/db
            $(jq '.chaindb.ledger_snapshot.chaindb_server' $p)
            $(jq '.chaindb.mainnet_chunks.chaindb_server'  $p)
        )
        progress "scenario" "preparing ChainDB for the $(green server node)"
        chaindb "${chaindb_server[@]}"

        # Nodes must be started AFTER the `chaindb` part!
        progress "scenario" "starting the $(yellow ChainDB server node)"
        backend start-node        "$dir" 'node-0'

        progress "scenario" "starting the $(yellow fetcher node)"
        backend start-node        "$dir" 'node-1'
        ## TODO:
        # +RTS -s$out/rts.dump

        scenario_setup_exit_trap  "$dir"
        backend wait-node-stopped "$dir" 'node-1'
        scenario_cleanup_exit_trap

        backend stop-all          "$dir"

        analysis_trace_frequencies 'current'
        ;;

    * ) usage_scenario;; esac
}

__scenario_exit_trap_dir=
scenario_exit_trap() {
    echo >&2
    msg "scenario:  $(with_color yellow exit trap triggered)"
    backend stop-all     "$__scenario_exit_trap_dir"
    backend fetch-logs   "$__scenario_exit_trap_dir"
    backend stop-cluster "$__scenario_exit_trap_dir"
    msg "scenario:  $(with_color yellow exit trap finished)"
}

scenario_setup_exit_trap() {
    local run_dir=$1
    export __scenario_exit_trap_dir=$run_dir
    trap scenario_exit_trap EXIT
}

scenario_cleanup_exit_trap() {
    trap - EXIT
}

__scenario_watcher_pid=
scenario_watcher() {
  local run_dir=$1
  while \
      ! test -f "${run_dir}"/flag/cluster-stopping            \
    &&                                                        \
        test "${__scenario_watcher_end_time}" -ge $(date +%s)
  do
    sleep 1
  done
  if ! test -f "${run_dir}"/flag/cluster-stopping
  then
    echo >&2
    touch "${run_dir}"/flag/cluster-stopping
    msg "scenario:  $(yellow end of time reached) for:  $(red $(jq '.meta.tag' -r ${__scenario_exit_trap_dir}/meta.json))"
    msg      "scenario:  $(red signalled termination)"
    progress "scenario" "now:  $(yellow $(date))"
  fi
}

scenario_setup_workload_termination() {
    local run_dir=$1

    export __scenario_watcher_self=$BASHPID
    local termination_tolerance_s=40
    local now=$(date +%s)
    local till_shutdown=$(($(jq '.meta.timing.shutdown_end' $run_dir/meta.json) - now))
    local till_workload=$(($(jq '.meta.timing.workload_end' $run_dir/meta.json) - now))
    local till_earliest=$(($(jq '.meta.timing.earliest_end' $run_dir/meta.json) - now))
    export __scenario_watcher_end_time=$((now + till_earliest + termination_tolerance_s))
    progress "scenario" "now:  $(yellow $(date --date=@$now))"
    progress "scenario" "until end:  workload $(yellow $till_workload), $(blue shutdown) $(yellow $till_shutdown), $(blue earliest) $(yellow $till_earliest)"
    progress "scenario" "shutdown tolerance:  $(yellow $termination_tolerance_s) s"
    # progress "scenario" "until end: workload $(yellow $(date --date=@$__scenario_watcher_end_time))"
    progress "scenario" "force-termination in $(white $((till_earliest + termination_tolerance_s))) seconds at $(yellow $(date --date=@$__scenario_watcher_end_time))"
    scenario_watcher "${run_dir}" &
    __scenario_watcher_pid=$!
}

scenario_cleanup_termination() {
    kill $__scenario_watcher_pid 2>/dev/null || true
    scenario_cleanup_exit_trap
}
