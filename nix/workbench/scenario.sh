usage_scenario() {
     usage "scenario" "Run scenario control" <<EOF
    idle DIR              Idle, isolated cluster scenario, runs indefinitely;
                            Aliased as 'default' scenario

    loaded DIR            Isolated cluster under tx-generator workload;
                            Terminates after profile-implied transaction
                            amount is submitted

    chainsync DIR         Chain syncing:
                            1. start the preset-defined proxy node,
                               using its respective connected topology mode,
                               fetching the chain up to the specified slot
                            2. restart the proxy with a disconnected topogy mode,
                               effectively making it an idle chaindb server
                            3. start the fetcher node, connected to the proxy

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
        ## This starts all nodes, due to unconditional start-up,
        ## to start in default mode -- meaning that:
        ##  - the proxy does start, connected
        ##  - the fetcher doesn't
        backend start-cluster   "$dir"
        ;;

    * ) usage_scenario;; esac
}

__scenario_exit_trap_dir=
scenario_exit_trap() {
    echo >&2
    msg "scenario:  $(with_color yellow exit trap triggered)"
    backend stop-cluster "$__scenario_exit_trap_dir"
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
    export __scenario_exit_trap_dir=$run_dir
    trap scenario_exit_trap EXIT

    export __scenario_watcher_self=$BASHPID
    local termination_tolerance_s=40
    export __scenario_watcher_end_time=$(jq '
           .meta.timing.earliest_end + '$termination_tolerance_s  $run_dir/meta.json)
    scenario_watcher &
    __scenario_watcher_pid=$!
}

scenario_cleanup_termination() {
    kill $__scenario_watcher_pid 2>/dev/null || true
    trap - EXIT
}
