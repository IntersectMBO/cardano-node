usage_scenario() {
     usage "scenario" "Run scenario control" <<EOF
    idle DIR              Idle, isolated cluster scenario, runs indefinitely

    fixed DIR             Isolated cluster;
                            Terminates after profile-implied time elapses

    fixed-loaded DIR      Isolated cluster under tx-generator workload;
                            Terminates after profile-implied transaction
                            amount is submitted

    chainsync DIR         Chain syncing:
                            1. start the preset-defined chaindb-server node,
                               feeding it a generated chaindb
                            2. start the fetcher node, connected to the chaindb-server

EOF
}

scenario() {
local op=${1:---help}; shift
local usage="USAGE: wb scenario SCENARIO-OP OP-ARGS.."
local dir=${1:?$usage}; shift
local tag=$(jq '.meta.tag' -r $dir/meta.json)
local p=$dir/profile.json

progress "run | scenario" "starting $(yellow $op)"
case "$op" in
    idle )
        backend start                "$dir"
        backend start-nodes          "$dir"
        ;;

    fixed )
        backend start                "$dir"

        scenario_setup_exit_trap     "$dir"
        scenario_setup_workload_termination   "$dir"
        backend start-nodes          "$dir"
        backend wait-pools-stopped   "$dir"
        scenario_cleanup_termination

        backend stop-cluster         "$dir"
        ;;

    fixed-loaded )
        backend start                "$dir"

        scenario_setup_exit_trap     "$dir"
        backend start-nodes          "$dir"
        backend start-generator      "$dir"

        scenario_setup_workload_termination   "$dir"
        backend wait-pools-stopped   "$dir"
        scenario_cleanup_termination

        backend stop-cluster         "$dir"
        ;;

    chainsync )
        local observer=(
            mainnet-chunks-with-snapshot-at-slot
            "$dir"/node-1/run/current/node-1/db-testnet
            $(jq '.chaindb.ledger_snapshot.observer'       $p)
            $(jq '.chaindb.mainnet_chunks.observer'        $p)
        )
        progress "scenario" "preparing ChainDB for the $(green "observer (fetcher)")"
        chaindb "${observer[@]}"

        local chaindb_server=(
            mainnet-chunks-with-snapshot-at-slot
            "$dir"/node-0/run/current/node-0/db-testnet
            $(jq '.chaindb.ledger_snapshot.chaindb_server' $p)
            $(jq '.chaindb.mainnet_chunks.chaindb_server'  $p)
        )
        progress "scenario" "preparing ChainDB for the $(green server node)"
        chaindb "${chaindb_server[@]}"

        backend start "$dir"

        progress "scenario" "starting the $(yellow ChainDB server node)"
        backend start-node        "$dir" 'node-0'

        progress "scenario" "starting the $(yellow fetcher node)"
        backend start-node        "$dir" 'node-1'
        ## TODO:
        # +RTS -s$out/rts.dump

        scenario_setup_exit_trap  "$dir"
        backend wait-node-stopped "$dir" 'node-1'
        scenario_cleanup_exit_trap

        backend stop-cluster      "$dir"

        analysis_trace_frequencies 'current'
        ;;

    * ) usage_scenario;; esac
}

__scenario_exit_trap_dir=
scenario_exit_trap() {
    echo >&2
    msg "scenario:  $(with_color yellow exit trap triggered)"
    backend stop-cluster "$__scenario_exit_trap_dir"
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
    while test $__scenario_watcher_end_time -ge $(date +%s)
    do sleep 1; done
    echo >&2
    msg "scenario:  $(yellow end of time reached) for:  $(red $(jq '.meta.tag' -r $__scenario_exit_trap_dir/meta.json))"
    rm -f $dir/flag/cluster-termination
    msg "scenario:  $(red signalled termination)"
    progress "scenario" "now:  $(yellow $(date))"
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
    progress "scenario" "force-termination in $(white $((till_earliest + termination_tolerance_s))) seconds.."
    scenario_watcher &
    __scenario_watcher_pid=$!
}

scenario_cleanup_termination() {
    kill $__scenario_watcher_pid 2>/dev/null || true
    scenario_cleanup_exit_trap
}
