usage_backend() {
     usage "backend" "Abstract over cluster backend operations" <<EOF
    is-running RUNDIR
                     Test if the cluster specified by the run directory
                       is currently running

    setenv-defaults PROFILE-DIR
                     Setup the global environment in env.jq,
                       using profile in PROFILE-DIR

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

    assert-is BACKEND-NAME
                     Check that the current backend is as expected

    assert-stopped   Assert that cluster is not running
EOF
}

backend() {
local op=${1:-$(usage_backend)} # No need to shift -- backends will use the op.

case "${op}" in
    is-running )                 backend_$WB_BACKEND "$@";;
    setenv-defaults )            backend_$WB_BACKEND "$@";;
    allocate-run )               backend_$WB_BACKEND "$@";;
    describe-run )               backend_$WB_BACKEND "$@";;
    start )                      backend_$WB_BACKEND "$@";;
    start-nodes )                backend_$WB_BACKEND "$@";;
    start-node )                 backend_$WB_BACKEND "$@";;
    stop-node )                  backend_$WB_BACKEND "$@";;
    wait-node )                  backend_$WB_BACKEND "$@";;
    wait-node-stopped )          backend_$WB_BACKEND "$@";;
    get-node-socket-path )       backend_$WB_BACKEND "$@";;
    start-generator )            backend_$WB_BACKEND "$@";;
    wait-pools-stopped )         backend_$WB_BACKEND "$@";;
    stop-cluster )               backend_$WB_BACKEND "$@";;
    cleanup-cluster )            backend_$WB_BACKEND "$@";;

    ## Handle non-generic calls:
    passthrough | pass )         backend_$WB_BACKEND "$@";;

    assert-is )
        local usage="USAGE: wb run $op BACKEND-NAME"
        local name=${2:?$usage}

        ## Check the backend echoes own name:
        local actual_name=$(backend_$WB_BACKEND name)
        if test "$actual_name" != "$name"
        then fatal "Workbench is broken:  'workbench_$WB_BACKEND name' returned:  '$actual_name'"; fi
        ;;

    assert-stopped )
        backend is-running &&
          fatal "backend reports that cluster is already running. Please stop it first:  $(yellow stop-cluster)" ||
          true
        ;;

    * ) usage_backend;; esac
}
