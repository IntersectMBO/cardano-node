usage_backend() {
     usage "backend" "Abstract over cluster backend operations" <<EOF
    is-running       Test if cluster is running

    get-node-socket-path RUNDIR
                     Given a run directory, print the node socket path
                       for 'cardano-cli'

    record-extended-env-config ENV-JSON [ENV-CONFIG-OPTS..]
                     Extend the environment JSON file with backend-specific
                       environment config

    describe-run RUNDIR
    pre-run-hook RUNDIR
    lostream-fixup-jqargs RUNDIR
    lostream-fixup-jqexpr

    start-run RUNDIR Start an allocated run

    assert-is BACKEND-NAME
                     Check that the current backend is as expected

    assert-stopped   Assert that cluster is not running
EOF
}

backend() {
local op=${1:-$(usage_backend)} # No need to shift -- backends will use the op.

case "${op}" in
    is-running )                 $WORKBENCH_BACKEND "$@";;
    get-node-socket-path )       $WORKBENCH_BACKEND "$@";;
    record-extended-env-config ) $WORKBENCH_BACKEND "$@";;
    describe-run )               $WORKBENCH_BACKEND "$@";;
    pre-run-hook )               $WORKBENCH_BACKEND "$@";;
    start-run )                  cp "$2"/genesis/genesis.json "$2"/genesis.json
                                 $WORKBENCH_BACKEND "$@";;
    lostream-fixup-jqargs )      $WORKBENCH_BACKEND "$@";;
    lostream-fixup-jqexpr )      $WORKBENCH_BACKEND "$@";;

    ## Handle non-generic calls:
    passthrough | pass )         $WORKBENCH_BACKEND "$@";;

    assert-is )
        local usage="USAGE: wb run $op BACKEND-NAME"
        local name=${2:?$usage}

        ## Check the backend echoes own name:
        local actual_name=$($WORKBENCH_BACKEND name)
        if test "$actual_name" != "$name"
        then fatal "Workbench is broken:  '$WORKBENCH_BACKEND name' returned:  '$actual_name'"; fi
        ;;

    assert-stopped )
        backend is-running &&
          fatal "backend reports that cluster is already running. Please stop it first!" ||
          true
        ;;

    * ) usage_backend;; esac
}
