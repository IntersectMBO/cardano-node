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
                            2. restart the proxy with a disconnected topogy mode,                                   effectively making it an idle chaindb server
                            3. start the fetcher node, connected to the proxy

EOF
}

scenario() {
local op=${1:---help}; shift
local usage="USAGE: wb scenario SCENARIO-OP OP-ARGS.."
local dir=${1:?$usage}; shift

msg "starting scenario: $(with_color blue $op)"
case "$op" in
    idle | default )
        backend start-cluster "$dir"
        ;;

    loaded )
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
