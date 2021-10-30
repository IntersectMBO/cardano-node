usage_scenario() {
     usage "scenario" "Run scenario control" <<EOF
    generic-idle DIR      Idle, isolated cluster scenario, runs indefinitely;
                            Aliased as 'default' scenario

    generic-loaded DIR    Isolated cluster under tx-generator workload;
                            Terminates after profile-implied transaction
                            amount is submitted

    chainsync DIR         Chain syncing:
                            1. start the preset-defined proxy node,
                               using its respective connected topology mode,
                               fetching the chain up to the specified slot
                            2. restart the proxy with a disconnected topogy mode,                                   effectively making it an idle chaindb server
                            3. start the fetcher node, connected to the proxy

EOF
}

scenario() {
local usage="USAGE: wb scenario $op RUNDIR"
local op=${1:---help)}; shift
local dir=${1:?$usage)}; shift

msg "starting scenario: $(with_color blue $op)"
case "$op" in
    generic-idle | default )
        backend start-cluster "$dir"
        ;;

    generic-loaded )
        backend start-cluster   "$dir"
        backend start-generator "$dir"
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
