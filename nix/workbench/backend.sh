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
    start-cluster RUNDIR
                     Start the cluster nodes
    get-node-socket-path RUNDIR
                     Given a run directory, print the node socket path
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
    is-running )                 backend_$WORKBENCH_BACKEND "$@";;
    setenv-defaults )            backend_$WORKBENCH_BACKEND "$@";;
    allocate-run )               backend_$WORKBENCH_BACKEND "$@";;
    describe-run )               backend_$WORKBENCH_BACKEND "$@";;
    start-cluster )              backend_$WORKBENCH_BACKEND "$@";;
    get-node-socket-path )       backend_$WORKBENCH_BACKEND "$@";;
    start-generator )            backend_$WORKBENCH_BACKEND "$@";;
    wait-pools-stopped )         backend_$WORKBENCH_BACKEND "$@";;
    stop-cluster )               backend_$WORKBENCH_BACKEND "$@";;
    cleanup-cluster )            backend_$WORKBENCH_BACKEND "$@";;

    ## Handle non-generic calls:
    passthrough | pass )         backend_$WORKBENCH_BACKEND "$@";;

    assert-is )
        local usage="USAGE: wb run $op BACKEND-NAME"
        local name=${2:?$usage}

        ## Check the backend echoes own name:
        local actual_name=$(backend_$WORKBENCH_BACKEND name)
        if test "$actual_name" != "$name"
        then fatal "Workbench is broken:  'workbench_$WORKBENCH_BACKEND name' returned:  '$actual_name'"; fi
        ;;

    assert-stopped )
        backend is-running &&
          fatal "backend reports that cluster is already running. Please stop it first:  stop-cluster" ||
          true
        ;;

    * ) usage_backend;; esac
}
