usage_scenario() {
     usage "scenario" "Run scenario control" <<EOF
    idle DIR              Idle, isolated cluster scenario, runs indefinitely;
                            Aliased as 'default' scenario

    loaded DIR            Isolated cluster under tx-generator workload;
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

progress "run | scenario" "starting $(with_color blue $op)"
case "$op" in
    idle | default )
        backend start-cluster "$dir"
        ;;

    fixed )
        backend start-cluster      "$dir"

        scenario_setup_termination "$dir"
        backend wait-pools-stopped "$dir"
        scenario_cleanup_termination

        backend stop-cluster       "$dir"
        ;;

    loaded )
        backend start-cluster   "$dir"
        backend start-generator "$dir"
        ;;

    fixed-loaded )
        backend start-cluster      "$dir"
        backend start-generator    "$dir"

        scenario_setup_termination "$dir"
        backend wait-pools-stopped "$dir"
        scenario_cleanup_termination

        backend stop-cluster       "$dir"
        ;;

    chainsync )
        local chaindb_args=(
            mainnet-chunks-with-snapshot-at-slot
            "$dir"/node-0/run/current/node-0/db-testnet
            $(jq '.node.shutdown_on_slot_synced.observer' $p)
            $(jq '.node.mainnet_chaindb_upto_chunk' $p)
        )
        progress "scenario" "preparing ChainDB for the server node"
        chaindb "${chaindb_args[@]}"

        progress "scenario" "starting the ChainDB server node"
        backend start-cluster "$dir"

        progress "scenario" "starting the fetcher node"
        backend start-node        "$dir" 'node-1'
        ## TODO:
        # time -o $out/totals time -f %M -o $out/highwater
        # +RTS -s$out/rts.dump

        scenario_setup_exit_trap  "$dir"
        backend wait-node-stopped "$dir" 'node-1'
        scenario_cleanup_exit_trap

        backend stop-cluster      "$dir"
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
    while test $__scenario_watcher_end_time -gt $(date +%s)
    do sleep 3; done
    echo >&2
    msg "scenario:  $(with_color yellow end of time reached) for:  $(with_color red $(jq '.meta.tag' -r $__scenario_exit_trap_dir/meta.json))"
    rm -f $dir/flag/cluster-termination
    msg "scenario:  $(with_color red signalled termination)"
}

scenario_setup_termination() {
    local run_dir=$1

    scenario_setup_exit_trap $run_dir

    export __scenario_watcher_self=$BASHPID
    local termination_tolerance_s=40
    export __scenario_watcher_end_time=$(jq '
           .meta.timing.earliest_end + '$termination_tolerance_s  $run_dir/meta.json)
    scenario_watcher &
    __scenario_watcher_pid=$!
}

scenario_cleanup_termination() {
    kill $__scenario_watcher_pid 2>/dev/null || true
    scenario_cleanup_exit_trap
}
