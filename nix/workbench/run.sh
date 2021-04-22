usage_run() {
     usage "run" "Managing cluster runs" <<EOF
    list                  List cluster runs
    mkname                Allocate a cluster run name/id/tag
    create                Create a cluster run directory
EOF
}

run() {
local op=${1:-list}; test $# -gt 0 && shift

case "${op}" in
    list )
        (test -d "$global_runsdir" && cd "$global_runsdir" && find . -type d)
        ;;

    mkname )
        local usage="USAGE: wb run mkname BATCH-NAME PROFILE-NAME"
        local batch=${1:?$usage}
        local prof=${2:?$usage}
        echo -n "$(date +'%Y'-'%m'-'%d'-'%H.%M').$batch.$prof"
        ;;

    create )
        local usage="USAGE: wb run create ??? BATCH-NAME PROFILE-NAME"
        ## Assumptions:
        ##   - genesis has been created
        ##   - the cluster is operating
        local name=${1:?$usage}
        local batch=${2:?$usage}
        local prof=${3:?$usage}

        local dir=$runsdir/$name
        if test "$(realpath "$dir")" = test "$(realpath "$global_runsdir")" -o "$name" = '.'
        then fatal "bad, bad tag '$name'"; fi
        ;;

    ### Undocumented
    describe | d )
        cat <<EOF
global_runsdir=$global_runsdir
EOF
        ;;

    * ) usage_run;; esac
}
