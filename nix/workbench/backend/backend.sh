usage_backend() {
     usage "backend" "Abstract over cluster backend operations" <<EOF
    is-running RUNDIR
                     Print every active components of the backend.
                       If nothing is printed, the backend is considered stopped.

    setenv-defaults BACKEND-DATA-DIR
                     Setup the global environment in env.jq,
                       using backend data in BACKEND-DATA-DIR

    allocate-run RUNDIR
    describe-run RUNDIR
    start RUNDIR     Start the backend
    start-nodes RUNDIR
    start-node RUNDIR NODE-NAME
    stop-node RUNDIR NODE-NAME
    wait-node RUNDIR NODE-NAME
                     Start/stop/wait for a particular node in the cluster
    get-node-socket-path RUNDIR [NODE]
                     Given a run directory, print a given node's node socket path
                       for 'cardano-cli'
    start-generator RUNDIR
                     Start generator

    wait-pools-stopped RUNDIR
                     Wait until all pools are stopped
    wait-workloads-stopped RUNDIR
                     Wait until all workloads are stopped
    stop-cluster RUNDIR
    cleanup-cluster RUNDIR
                     Wipe cluster state to pristine

    validate         Basic workbench backend sanity check

    assert-stopped   Assert that cluster is not running
EOF
}

backend() {
local op=${1:-$(usage_backend)} # No need to shift -- backends will use the op.

case "${op}" in
    # Prepare functions
    setenv-defaults )            backend_$WB_BACKEND_NAME "$@";;
    allocate-run )               backend_$WB_BACKEND_NAME "$@";;
    describe-run )               backend_$WB_BACKEND_NAME "$@";;
    # Start functions
    is-running )                 backend_$WB_BACKEND_NAME "$@";;
    start-cluster )              backend_$WB_BACKEND_NAME "$@";;
    deploy-genesis )             backend_$WB_BACKEND_NAME "$@";;
    # Sceneario functions
    start-tracers )              backend_$WB_BACKEND_NAME "$@";;
    start-nodes )                backend_$WB_BACKEND_NAME "$@";;
    start-generator )            backend_$WB_BACKEND_NAME "$@";;
    start-workload-by-name )     backend_$WB_BACKEND_NAME "$@";;
    start-healthchecks )         backend_$WB_BACKEND_NAME "$@";;
    # Fine grained
    start-node )                 backend_$WB_BACKEND_NAME "$@";;
    stop-node )                  backend_$WB_BACKEND_NAME "$@";;
    wait-node )                  backend_$WB_BACKEND_NAME "$@";;
    wait-node-stopped )          backend_$WB_BACKEND_NAME "$@";;
    get-node-socket-path )       backend_$WB_BACKEND_NAME "$@";;
    wait-pools-stopped )         backend_$WB_BACKEND_NAME "$@";;
    wait-workloads-stopped )     backend_$WB_BACKEND_NAME "$@";;
    # Stop functions
    stop-all )                   backend_$WB_BACKEND_NAME "$@";;
    fetch-logs )                 backend_$WB_BACKEND_NAME "$@";;
    stop-cluster )               backend_$WB_BACKEND_NAME "$@";;
    cleanup-cluster )            backend_$WB_BACKEND_NAME "$@";;

    ## Handle non-generic calls:
    passthrough | pass )         shift; backend_$WB_BACKEND_NAME "$@";;

    validate )
        local usage="USAGE: wb run $op"

        ## Check the backend echoes own name:
        local actual_name=$(backend_$WB_BACKEND_NAME name)
        if test "$actual_name" != "$WB_BACKEND_NAME"
        then fatal "Workbench is broken:  'workbench_$WB_BACKEND_NAME name' returned:  '$actual_name'"; fi

        # backend_$WB_BACKEND_NAME validate
        true;;

    assert-stopped )
        local running_components=($(backend is-running "run/current"))

        if test ${#running_components[*]} -gt 0
        then
          newline
          progress "$(red FATAL)" "backend reports running components (${running_components[*]})"
          progress "hint"         "  1. if any other local cluster is running on this machine, please use: $(yellow stop-cluster)"
          progress "hint"         "  2. if any other cardano-node process is active on this machine, please shut it down first"
          progress "hint"         "$(green any active cluster or node process may significantly impair metrics taken during benchmark)"
          fatal "aborted due to active components"
        fi
        true
        ;;

    * ) set +x; usage_backend;; esac
}
