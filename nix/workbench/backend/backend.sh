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
    setenv-defaults )            backend_$WB_BACKEND "$@";;
    allocate-run )               backend_$WB_BACKEND "$@";;
    describe-run )               backend_$WB_BACKEND "$@";;
    # Start functions
    is-running )                 backend_$WB_BACKEND "$@";;
    start-cluster )              backend_$WB_BACKEND "$@";;
    deploy-genesis )             backend_$WB_BACKEND "$@";;
    # Sceneario functions
    start-tracers )              backend_$WB_BACKEND "$@";;
    start-nodes )                backend_$WB_BACKEND "$@";;
    start-generator )            backend_$WB_BACKEND "$@";;
    start-healthchecks )         backend_$WB_BACKEND "$@";;
    # Fine grained
    start-node )                 backend_$WB_BACKEND "$@";;
    stop-node )                  backend_$WB_BACKEND "$@";;
    wait-node )                  backend_$WB_BACKEND "$@";;
    wait-node-stopped )          backend_$WB_BACKEND "$@";;
    get-node-socket-path )       backend_$WB_BACKEND "$@";;
    wait-pools-stopped )         backend_$WB_BACKEND "$@";;
    # Stop functions
    stop-all )                   backend_$WB_BACKEND "$@";;
    fetch-logs )                 backend_$WB_BACKEND "$@";;
    stop-cluster )               backend_$WB_BACKEND "$@";;
    cleanup-cluster )            backend_$WB_BACKEND "$@";;

    ## Handle non-generic calls:
    passthrough | pass )         shift; backend_$WB_BACKEND "$@";;

    validate )
        local usage="USAGE: wb run $op"

        ## Check the backend echoes own name:
        local actual_name=$(backend_$WB_BACKEND name)
        if test "$actual_name" != "$WB_BACKEND"
        then fatal "Workbench is broken:  'workbench_$WB_BACKEND name' returned:  '$actual_name'"; fi

        # backend_$WB_BACKEND validate
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
