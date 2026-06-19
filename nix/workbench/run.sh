global_rundir_def=$(realpath ${WB_RUNDIR:-$PWD/run})

usage_run() {
    set +x
    usage "run" "Managing cluster runs" <<EOF
    $(helpcmd list-runs)              List local runs
     $(blk runs lsr)
    $(helpcmd list-remote)            List remote runs
     $(blk remote lsrr)
    $(helpcmd list-verbose)           List local runs, verbosely
     $(blk verb lsrv)
    $(helpcmd list-verbose-remote)    List remote runs, verbosely
     $(blk rverb lsrvr)
    $(helpcmd list-sets)              List local run sets
     $(blk sets lss)
    $(helpcmd list-sets-remote)       List remote cluster run sets
     $(blk rsets lssr)
    $(helpcmd set-add SETNAME RUN...)
     $(blk add sa)                  Add runs to the named run set
    $(helpcmd run-or-set)             Resolve a run-or-set name to a list of runs
     $(blk ros)
    $(helpcmd list-pattern)           List local runs
     $(blk lspat lsrp)
    $(helpcmd as-remote)            Helper to modify remote environment in $(blue \$WB_REMOTE)
     $(blk asr)

    $(helpcmd describe RUN)

    $(helpcmd fix-cardano-ops-run-structure RUN)
     $(blk fcors fix-cardano-ops)  Back-fill .meta.manifest / .meta.timing from
                            cardano-ops-style legacy meta.json.
                            (Also reachable as: fix-legacy-run-structure / fix-legacy / flrs.)

    $(helpcmd allocate --batch-name NAME --profile-data DIR --era-name ERA --backend-name BACKEND --manifest JSON [--genesis-cache-entry DIR] [-- BACKEND-ARGS..])
                          Allocate a cluster run.  All inputs are named flags:
                            --batch-name      batch key (no semantics attached)
                            --profile-data    directory containing profile.json / node-specs.json
                            --era-name        one of: byron shelley allegra mary alonzo babbage conway dijkstra
                            --backend-name    e.g. supervisor, nomad
                            --manifest        JSON manifest produced by 'manifest collect-from-checkout'
                            --genesis-cache-entry  optional pre-supplied cache entry
                          A unique tag is derived for this run and the
                            $(green current) alias is set to point at it.

    $(helpcmd fetch-run RUN)        Fetch a remote run
     $(blk fr fetch)
    $(helpcmd fetch-analysis RUN..)
     $(blk fa)                   Fetch analyses of remote runs
    $(helpcmd analyse-remote RUN..)  Run analyses of runs, remotely

    $(helpcmd start [--scenario NAME] [--idle] [--analyse] RUN)
                          Start the named run.
                            --scenario forces a scenario, different from what
                                is specified by the run profile;
                                See 'wb scenario --help' for scenario descriptions
                            --idle is a short-cut for '--scenario idle'
                            --analyse triggers analysis after 'start' returns

    $(helpcmd stop RUN)              Stop the named run

    $(helpcmd restart [--no-generator] BACKEND-START-ARGS..)
                          Stop and restart the current run (without a new allocation)

  $(red run) $(blue options):

    $(helpopt --rundir DIR)          Set the runs directory.  Defaults to $(green $global_rundir_def),
                            if it exists, otherwise to $(blue \$WB_RUNDIR), if that
                            exists, and finally unconditionally to $(green $global_rundir_def)
EOF
}

run_default_op='list-runs'

run() {
set -eu
if   test -v "WB_RUNDIR" && test -d "$WB_RUNDIR"
then global_rundir=$WB_RUNDIR
## Allow compatibility with cardano-ops legacy runs directory layout:
elif test -d "$global_rundir_def"
then global_rundir=$global_rundir_def
else global_rundir=$global_rundir_def
     mkdir "$global_rundir"
fi

if test -v "WB_REMOTE"
then remote=$WB_REMOTE
else remote='{"env":"deployer","depl":"nomad-1"}'            # nomad/nomad-1 as fallback default
fi

local sargs=()

run_remote_get_args=()
while test $# -gt 0
do case "$1" in
       --remote )      sargs+=($1 $2); remote=$2; shift;;
       --rundir )      sargs+=($1 $2); global_rundir=$2; shift;;
       --clean | -c )  sargs+=($1);    run_remote_get_args+=($1);;
       * ) break;; esac; shift; done

local op=${1:-$run_default_op}; test $# -gt 0 && shift

case "$op" in
    get-global-rundir )
        realpath --relative-to "$(pwd)" "$global_rundir";;

    as-remote | asr )
        local usage="USAGE: wb run $op <environment> <deployment> [rundir]"
        if [[ -z ${1+x} || -z ${2+x} ]]
        then
            echo $usage
            echo "where: <environment> is a Host from your '~/.ssh/config' or a literal like 'user@1.2.3.4'"
            echo "        <deployment> is a cardano-node or cardano-ops checkout in the environment user's home dir"
            echo "            [rundir] subdirectory containing runs; necessary only when differing from default 'run'"
        else
            remote=$(jq -cn --arg env $1 --arg depl $2 --arg dir ${3:-''} '{env: $env, depl: $depl} | if $dir=="" then . else . * {dir: $dir} end')
            echo "to modify remote in current shell environment, please execute:"
            echo $(yellow "export WB_REMOTE='$remote'")
        fi;;

    list-runs | runs | lsr )
        local usage="USAGE: wb run $op [--remote | -r]"
        local on_remote=
        while test $# -gt 0
        do case "$1" in
               --remote | -r ) on_remote='true';;
               * ) msg "FATAL:  list, unknown flag '$1'"; usage_run;;
           esac; shift; done

        if test -z "$on_remote"
        then (eval "$(run_ls_cmd "$global_rundir")")
        else
            local r=${1:-$remote}
            local env=$( jq <<<$r '.env' -r)
            local depl=$(jq <<<$r '.depl' -r)
            local dir=$( jq <<<$r '.dir // "run"' -r)
            ssh $env \
                sh -c "'$(run_ls_cmd $depl/$dir)'"
        fi;;

    list-remote | remote | lsrr ) ## Convenience alias for 'list'
        run list-runs --remote;;

    list-verbose | verb | lsrv )
        local usage="USAGE: wb run $op [--remote | -r] [--limit [N=10] | -n N]"
        local on_remote= limit=20
        while test $# -gt 0
        do case "$1" in
               --remote | -r ) on_remote='true';;
               --limit | -n )           limit=$2; shift;;
               * ) msg "FATAL:  list-verbose, unknown flag '$1'"; usage_run;;
           esac; shift; done

        if test -z "$on_remote"
        then (eval "$(run_ls_tabulated_cmd "$global_rundir" $limit)")
        else
            local r=${1:-$remote}
            local env=$( jq <<<$r '.env' -r)
            local depl=$(jq <<<$r '.depl' -r)
            local dir=$( jq <<<$r '.dir // "run"' -r)
            ssh $env \
                sh -c "'$(run_ls_tabulated_cmd $depl/$dir $limit)'"
        fi;;

    list-verbose-remote | rverb | lsrvr ) ## Convenience alias for 'list-verbose'
        run list-verbose --remote;;

    list-sets | sets | lss )
        local usage="USAGE: wb run $op [--remote | -r]"
        local on_remote=
        while test $# -gt 0
        do case "$1" in
               --remote | -r ) on_remote='true';;
               * ) msg "FATAL:  list-sets, unknown flag '$1'"; usage_run;;
           esac; shift; done

        if test -z "$on_remote"
        then (eval "$(run_ls_sets_cmd "$global_rundir")")
        else
            local r=${1:-$remote}
            local env=$( jq <<<$r '.env' -r)
            local depl=$(jq <<<$r '.depl' -r)
            local dir=$( jq <<<$r '.dir // "run"' -r)
            ssh $env \
                sh -c "'$(run_ls_sets_cmd $depl/$dir)'"
        fi;;

    list-sets-remote | rsets | lssr ) ## Convenience alias for 'list-sets'
        run list-sets --remote;;

    set-add | add | sa )
        local usage="USAGE: wb run $op NAME [RUN..]"
        local name=${1:?$usage}; shift
        mkdir -p "$global_rundir/.sets/$name" &&
            (cd  "$global_rundir/.sets/$name"
             for x in $*
             do if ! run get $x >/dev/null
                then fail "set-add $name:  constituent run missing: $(white $x)"
                fi
                ln -s ../../$x
             done);;

    run-or-set | ros )
        local usage="USAGE: wb run $op [--query] [--remote | -r] NAME"
        local query= get_args=() on_remote=
        while test $# -gt 0
        do case "$1" in
               --try | --query ) get_args+=($1); query='true';;
               --remote | -r ) on_remote='true';;
               --* ) msg "FATAL:  run-or-set, unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local name=${1:?$usage}
        local env=$( jq <<<$remote '.env' -r)
        local depl=$(jq <<<$remote '.depl' -r)
        local dir=$( jq <<<$remote '.dir // "run"' -r)
        if   test -n "$on_remote"
        then if test -n "$(ssh $env -- sh -c "'$(run_ls_sets_cmd $depl/$dir)'" |
                                       grep $name || true)"
             then rsync -Wa --delete-after \
                      $env:$depl/$dir/.sets/$name ../cardano-node/run/.sets
                  ssh $env -- \
                      sh -c "'cd $depl/$dir/.sets/$name && find . -type l | cut -d/ -f2'"
             else ssh $env -- \
                      sh -c "'if test -f $depl/$dir/$name/meta.json;
                              then echo $name;
                              else echo \"$(red run-or-set on $env/$depl:)  missing run or set $(white $name)\"
                              exit 1;
                              fi'"
             fi
        elif test -n "$(run list-sets | grep $name || true)"
        then (cd     "$global_rundir/.sets/$name"
              find . -type l | cut -d/ -f2)
        elif run get "${get_args[@]}" $name >/dev/null
        then echo $name
        elif test -n "$query"
        then return 1
        else fail "run-or-set:  missing run or set $(white $name)"
        fi;;

    list-pattern | lspat | lsrp )
        test -d "$global_rundir" &&
            (cd "$global_rundir"
             ls $1/meta.json |
                 grep -v 'current$\|deploy-logs$' |
                 cut -d/ -f1 |
                 sort || true);;

    describe )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$(run get $run)

        if ! run check "$run"
        then fatal "run fails sanity checks:  $run at $dir"; fi

        cat <<EOF
  - run dir:         $dir
  - profile JSON:    $dir/profile.json
  - node specs:      $dir/node-specs.json
  - topology:        $dir/topology/topology.pdf
  - node base port:  $(envjq 'basePort')
EOF
        backend describe-run "$dir"
        ;;

    ## Back-fill `.meta.manifest` / `.meta.timing` for genuine cardano-ops
    ## legacy meta.json (which carries `.meta.pins` / `.meta.timestamp` etc.
    ## instead). Dispatched by `check` when those fields are absent.
    fix-cardano-ops-run-structure | fix-cardano-ops | fcors \
        | fix-legacy-run-structure | fix-legacy | flrs )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$(run compute-path "$run")

        if test ! -f "$dir"/genesis/genesis.shelley.json -a -f "$dir"/genesis.json
        then msg "fixing up genesis naming in:  $dir"
             mkdir -p "$dir"/genesis
             mv "$dir"/genesis.json "$dir"/genesis/genesis.shelley.json; fi

        if test -z "$(ls -d "$dir"/node-* 2>/dev/null)"
        then msg "fixing up a legacy cardano-ops run in:  $dir"
             if   local dirs=$(ls -d "$dir"/logs/logs-*/ 2>/dev/null || true)
                  test -n "$dirs"
             then for logdir in $dirs
                  do local logs_less=$(basename "$logdir" | cut -c6-)
                     mv "$logdir" "$dir"/$logs_less; done
             elif local dirs=$(ls -d "$dir"/analysis/logs-*/ 2>/dev/null || true)
                  test -n "$dirs"
             then for logdir in $dirs
                  do local logs_less=$(basename "$logdir" | cut -c6-)
                     mv "$logdir" "$dir"/$logs_less; done; fi
        else msg "fixing up a cardano-ops run in:  $dir"; fi

        ## Back-fill manifest only when the run lacks one (i.e. truly legacy
        ## cardano-ops runs). Workbench runs already populate .meta.manifest in
        ## `allocate`, so running the legacy helper on them fails because the
        ## helper reads .meta.pins / .meta.node_commit_spec which exist only in
        ## cardano-ops meta.json.
        if test "$(jq -r '.meta.manifest // empty' "$dir"/meta.json)" = ""
        then progress "run | fix-cardano-ops-run-structure" "adding manifest"
             jq_fmutate "$dir"/meta.json '
                .meta.manifest = $manifest
                ' --argjson manifest "$(legacy_run_manifest $dir)"
        else progress "run | fix-cardano-ops-run-structure" "manifest present, skipping back-fill"; fi

        ## Same guard for timing: workbench runs already have .meta.timing.
        ## The legacy helper reads .meta.timestamp which exists only in
        ## cardano-ops meta.json.
        if test "$(jq -r '.meta.timing // empty' "$dir"/meta.json)" = ""
        then progress "run | fix-cardano-ops-run-structure" "adding timing"
             jq_fmutate "$dir"/meta.json '
                .meta.timing = $timing
                ' --argjson timing "$(legacy_run_timing $dir)"
        else progress "run | fix-cardano-ops-run-structure" "timing present, skipping back-fill"; fi

        jq ' .meta.profile_content
           | .analysis.filters += ["model"]
           | .node.tracing_backend = "trace-dispatcher"
           ' "$dir"/meta.json > "$dir"/profile.json;;

    compute-path )
        if test -f "$1/meta.json"
        then echo -n "$1"
        else realpath --relative-to "$(pwd)" "$global_rundir/$1"
        fi;;

    check )
        local usage="USAGE: wb run $op [--query] RUN"
        local query=
        while test $# -gt 0
        do case "$1" in
               --try | --query ) query='true';;
               --* ) msg "FATAL:  run-or-set, unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local run=${1:?$usage}
        local dir=$(run compute-path "$run")

        if ! jq_check_json "$dir"/meta.json 2>/dev/null
        then if test $run = 'current'
             then local alt=$(run list | tail -n1)
                  progress 'run | check' "$(with_color white current) missing, resetting to $(with_color white $alt)"
                  run set-current $alt
             elif test -n "$query"
             then return 1
             else fatal "run $run (at $dir) missing a file:  meta.json"; fi; fi

        ## Same file-based dispatch extended to accept the genesis sub-dir
        ## layout in addition to the legacy top-level name. profile.json + a
        ## shelley genesis (under either layout) is what distinguishes a
        ## workbench run from a cardano-ops legacy run.
        ## Era doesn't need migration: locli reads it via a 3-path decoder (see
        ## `allocate` below).
        test -f "$dir"/profile.json \
           -a \( -f "$dir"/genesis-shelley.json -o -f "$dir"/genesis/genesis.shelley.json \) \
           || run fix-cardano-ops-run-structure "$run";;

    get-path | get )
        local usage="USAGE: wb run $op [--query] RUN"
        local check_args=()
        while test $# -gt 0
        do case "$1" in
               --try | --query ) check_args+=($1);;
               --* ) msg "FATAL:  get-path, unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local runspec=${1:?$usage}
        local nrun=$(runspec_normalise $runspec)
        local run=$(runspec_run $nrun)

        if   run check "${check_args[@]}" "$run"
        then run compute-path             "$run"
        else return 1
        fi;;

    set-identifier | setid )
        local usage="USAGE: wb run $op [RUN IDENT]*"

        while test $# -gt 0
        do local run=${1:?$usage}
           local ident=${2:?$usage}

           local dir=$(run compute-path "$run")
           test -n "$dir" || fail "malformed run: $run"

           progress "analyse" "setting run identifier to:  $(white $ident), was $(blue $(jq -r .meta.ident "$dir"/meta.json))"

           ## XXX: ugly duplication.
           jq_fmutate "$dir"/meta.json \
             '.meta.ident = $ident' --arg ident "$ident"
           jq_fmutate "$dir"/analysis/summary.json \
             '.sumMeta.ident = $ident' --arg ident "$ident"

           shift 2
        done;;

    ## It's quite messy, semantically: set(when-specified)-and-get,
    decide-identifier | decid )
        local usage="USAGE: wb run $op (RUN | ID:RUN)"
        local runspec=${1:?$usage}
        local nrun=$(runspec_normalise $runspec)
        local run=$(runspec_run $nrun)  ident=$(runspec_id $nrun)
        local dir=$(run compute-path "$run")
        if   test          -n "$ident"
        then run setid "$run" "$ident"
             echo              $ident
        else jq -r .meta.ident "$dir"/meta.json
        fi;;

    show-meta | show | meta | s )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}

        jq '.' "$(run get "$run")"/meta.json;;

    set-current | set )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$(run get "$run")

        rm -f      "$global_rundir"/{current,-current}
        ln -s $run "$global_rundir"/-current
        ln -s $run "$global_rundir"/current;;

    current-run-path | current-path | path )
        realpath --relative-to "$(pwd)" "$global_rundir"/current;;

    current-run-tag | current-tag | tag | current )
        basename "$(run current-path)";;

    current-run-meta | current-meta | meta )
        jq '.' "$(run current-path)"/meta.json;;

    current-run-profile | current-profile | profile | p )
        jq '.' "$(run current-path)"/profile.json;;

    allocate )
        local usage="USAGE: wb run $op --batch-name NAME --profile-data DIR --era-name ERA --backend-name BACKEND --manifest JSON [--genesis-cache-entry DIR] [-- BACKEND-ARGS..]"
        ## All inputs are named flags. No positionals — that's the whole point.
        ## An unquoted/empty upstream variable cannot shift a later flag onto an
        ## earlier slot when there are no slots; missing-required is caught by
        ## the validation block below with a clear error.
        ## Flag names mirror `wb start`'s vocabulary (`--batch-name`,
        ## `--era-name`, `--backend-name`, `--profile-data`, …) so the same word
        ## means the same thing at both layers.
        local batch_name= era_name= backend_name=
        local profile_data= manifest= genesis_cache_entry=
        while test $# -gt 0
        do case "$1" in
               --batch-name )          batch_name=$2;          shift;;
               --era-name )            era_name=$2;            shift;;
               --backend-name )        backend_name=$2;        shift;;
               --profile-data )        profile_data=$2;        shift;;
               --manifest )            manifest=$2;            shift;;
               --genesis-cache-entry ) genesis_cache_entry=$2; shift;;
               -- ) shift; break;;
               --* ) fatal "run | allocate: unknown flag '$1'";;
               * )   fatal "run | allocate: unexpected positional '$1' (allocate takes only named flags; see $usage)";;
           esac; shift; done
        local backend_args=("$@")

        ## Required-input validation (all in one place, no scattered checks).
        test -n "$batch_name"   || fatal "run | allocate: --batch-name is required"
        test -n "$era_name"     || fatal "run | allocate: --era-name is required"
        test -n "$backend_name" || fatal "run | allocate: --backend-name is required"
        test -n "$profile_data" || fatal "run | allocate: --profile-data is required"
        test -n "$manifest"     || fatal "run | allocate: --manifest is required"
        test -d "$profile_data" \
            || fatal "run | allocate: --profile-data directory does not exist: $profile_data"
        test -f "$profile_data/profile.json" \
            || fatal "run | allocate: missing $profile_data/profile.json"

        ## Single source of truth: the profile name comes from the JSON the
        ## caller pointed us at, not from a separate argument that could drift.
        local profile_name
        profile_name=$(jq '.name' -r "$profile_data/profile.json")
        test -n "$profile_name" -a "$profile_name" != "null" \
            || fatal "run | allocate: could not read .name from $profile_data/profile.json"

        ## Validate era_name once and pre-compute the 4-letter form for the
        ## tag. Reused later instead of a second case-statement at tag time.
        local era_short
        case "$era_name" in
            byron)    era_short=byron;;
            shelley)  era_short=shey;;
            allegra)  era_short=alra;;
            mary)     era_short=mary;;
            alonzo)   era_short=alzo;;
            babbage)  era_short=bage;;
            conway)   era_short=coay;;
            dijkstra) era_short=dira;;
            *) fatal "run | allocate: unknown --era-name '$era_name' (expected one of: byron shelley allegra mary alonzo babbage conway dijkstra)";;
        esac

        ## 1. genesis cache entry:
        progress "run | genesis" "cache entry:  $(if test -n "$genesis_cache_entry"; then echo pre-supplied; else echo preparing a new one..; fi)"
        if test -z "$genesis_cache_entry"
        then genesis_cache_entry=$(
                 genesis prepare-cache-entry \
                  "$profile_data"/profile.json)
        fi

        ## 2. decide the tag:
        ##    Must match `^[a-zA-Z0-9-]{1,128}$)` or it won't be possible to use
        ##    it as a Nomad Namespace or Nomad Job name.
        ##    NOTE: The tag time is different from the genesis time
        local hash=$(jq '."cardano-node".commit | .[:5]' -r <<<$manifest)
        local date_pref=$(date --utc +'%Y-%m-%d'-'%H-%M')
        if [[ "$batch_name" == "undefined" ]]
        then batch_name=$(get_most_significant_git_tag)
        fi
        local batch_inf=$(echo -n ${batch_name} | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z^0-9]//g')
        local prof_suf=$(test -v "WB_PROFILEDBUILD" && test -n "$WB_PROFILEDBUILD" -a "$WB_PROFILEDBUILD" = 'yes' && echo '-prof')
        ## $era_short was pre-computed from $era_name during input validation above.
        local run="${date_pref}-${hash}-${batch_inf::12}-${profile_name}-${era_short}-${backend_name::3}${prof_suf}"
        ## The tag must match `^[a-zA-Z0-9-]{1,128}$` or it can't be used as a
        ## Nomad Namespace or Job name. Validate now, not after directory
        ## creation, so a malformed tag fails loudly with the offending value.
        [[ "$run" =~ ^[a-zA-Z0-9-]{1,128}$ ]] \
            || fatal "run | allocate: constructed tag does not match ^[a-zA-Z0-9-]{1,128}\$: '$run'"
        progress "run | tag" "allocated run identifier (tag):  $(with_color white $run)"

        ## 3. create directory:
        local dir=$(realpath --relative-to "$(pwd)" "$global_rundir/$run")
        local realdir=$(realpath --canonicalize-missing "$dir")

        test "$(dirname "$realdir")" = "$(realpath "$global_rundir")" ||
            fatal "profile | allocate bad tag/run dir:  $run @ $dir"
        test ! -e "$dir" ||
            fatal "profile | allocate tag busy:  $run @ $dir"
        mkdir -p "$dir"/flag && test -w "$dir" ||
            fatal "profile | allocate failed to create writable run directory:  $dir"

        ## 4. populate the directory with files shared by all backends:
        ## $profile_data and $profile_name were validated/derived above —
        ## no need to re-check the name matches what we just read from .name.
        progress "run | profile" "pre-supplied ($profile_name):  $profile_data"
        ln -s "$profile_data"                 "$dir"/profile
        if test -n "${WB_PROFILE_OVERLAY:-}"
        ## Allow 'wb' pick up the profile overlay in 'profiles.jq':
        then wb profile
        else jq . "$profile_data"/profile.json
        fi > "$dir"/profile.json
        progress "profile | overlay" "$(white $(jq .overlay "$dir"/profile.json))"
        cp    "$profile_data"/node-specs.json "$dir"/node-specs.json

        local ghc_version=$(get_node_ghc_version)

        progress "run | topology" \
                 "$(white $(jq -r .composition.topology "$dir"/profile.json))"
        ln -s "$profile_data"/topology.json        "$dir"
        ln -s "$profile_data"/topology.dot         "$dir"

        ## 5. backend specifics allocations:
        backend allocate-run "$dir" "${backend_args[@]}"

        ## 6. to be able to deploy the genesis the cluster must be started
        #     because Nomad uses `nomad exec wget`
        # FIXME: the problem is that supervisor uses "run/current" in its
        #        configuration and a "meta.json" is needed to pass `run check`
        #        that is called by `run set-current`. This error is only showing
        #        a "FATAL" message and not exiting by pure chance (bash error
        #        supression because of a command called inside `local`). So
        #        we ignore it as a temporary solution because all the bash
        #        cleaup needed to fix this will take too long.
        run set-current "$run" 2>/dev/null || true
        backend start-cluster "$dir"

        ## 7. allocate genesis time
        ##    NOTE: The genesis time is different from the tag time.
        progress "run | time" "allocating time:"
        local timing=$(profile allocate-time "$dir"/profile.json)
        profile describe-timing "$timing"

        ## Locli reads the era from meta.json via a 3-path decoder
        ## (`bench/locli/src/Cardano/Analysis/API/Context.hs:235-240`):
        ##   1. .meta.era                            (Alternative: eraDirect)
        ##   2. .meta.profile_content.era            (eraProfile)
        ##   3. .meta.profile_content.generator.era  (eraGenerator)
        ## We populate path #1 here for new workbench-root runs.
        ## Older workbench-master runs already carry path #2 in the
        ## profile_content snapshot (back when cardano-profile wrote `era` into
        ## every profile JSON). Legacy cardano-ops runs that went through
        ## `compat-meta-fixups` carry path #3.
        ## So no historical run needs a meta.json migration for era.
        local args=(
            --arg       run              "$run"
            --arg       batch_name       "$batch_name"
            --arg       era              "$era_name"
            --arg       profile_name     "$profile_name"
            --arg       ghc_version      "$ghc_version"
            --argjson   timing           "$timing"
            --slurpfile profile_content  "$dir"/profile.json
            --argjson   manifest         "$manifest"
        )
        jq_fmutate "$dir"/meta.json '. *
           { meta:
             { tag:              $run
             , batch:            $batch_name
             , era:              $era
             , profile:          $profile_name
             , node_ghc_version: $ghc_version
             , timing:           $timing
             , manifest:         $manifest
             , profile_overlay:  $profile_content[0].overlay
             , profile_content:  $profile_content[0]
             }
           }
           ' "${args[@]}"

        progress "run | genesis" "deriving from cache"
        if test -z "$genesis_cache_entry"
        then fail "internal error:  no genesis cache entry"
        else genesis derive-from-cache      \
                     "$dir"/profile.json    \
                     "$genesis_cache_entry" \
                     "$dir"/genesis         \
                     "$timing"
        fi
        echo >&2
        ## Add global_basedir Voltaire Plutus guardrails script
        cp "$global_basedir"/genesis/guardrails-script.plutus "$dir"/genesis/

        ## 8. deploy genesis
        progress "run | genesis" "deploying.."
        (
          # This step is resource intensive so we use a lockfile to avoid
          # running it in parallel to a benchmark.
          acquire_lock
          backend deploy-genesis "$dir"
        )

        ## 9. everything needed to start-[tracers|nodes|generator] should be
        ##    ready
        progress "run" "allocated $(with_color white $run) @ $dir"
        run     describe "$run"
        profile describe "$dir"/profile.json
        ;;

    fetch-run | fetch | fr )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}

        local env=$( jq <<<$remote '.env' -r)
        local depl=$(jq <<<$remote '.depl' -r)
        local dir=$( jq <<<$remote '.dir // "run"' -r)

        local args=(
            "$env"
            "$depl"
            "$dir"
            "$run"
            'for f in ${files[*]};
             do fp=${f/%/.tar.zst};
                fpp=${fp/#/compressed/logs-};
                if test -f $fpp;
                then cat $fpp;
                else tar c $f --zstd --ignore-failed-read;
                fi;
             done;
             '

            common-run-files
        )
        run_remote_get "${args[@]}";;

    fetch-analysis | fa )
        local usage="USAGE: wb run $op RUN.."
        local runs=() run
        for rs in $*
        do runs+=($(run "${sargs[@]}" run-or-set --query --remote $rs || echo $rs))
        done
        if test $# = 0; then runs=(current); fi

        local env=$( jq <<<$remote '.env' -r)
        local depl=$(jq <<<$remote '.depl' -r)
        local dir=$( jq <<<$remote '.dir // "run"' -r)

        progress "run | remote" "trying to fetch analyses:  $(white ${runs[*]})"
        for run in ${runs[*]}
        do if   test "$(ssh $env -- sh -c "'ls -ld $depl/$dir/$run          | wc -l'")" = 0
           then fail "fetch-analysis:  run does not exist on remote: $(white $run)"
           elif test "$(ssh $env -- sh -c "'ls -ld $depl/$dir/$run/analysis | wc -l'")" = 0
           then fail "fetch-analysis:  run has not been analysed on remote: $(white $run)"
           else local analysis_files=(
                   $(ssh $env -- \
                     sh -c "'cd $depl/$dir/$run && ls analysis/{cdf/*.cdf,*.{json,org,txt}} | fgrep -v -e flt.json -e logobjs.json -e perf-stats.json -e mach-views.json'" \
                     2>/dev/null)
                )
                local args=(
                   "${run_remote_get_args[@]}"
                   "$env"
                   "$depl"
                   "$dir"
                   "$run"
                   'tar c ${files[*]} --zstd;'

                   common-run-files
                   ${analysis_files[*]}
                )
                run_remote_get "${args[@]}"
           fi
        done
        ;;

    analyse-remote )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}

        local env=$( jq <<<$remote '.env' -r)
        local depl=$(jq <<<$remote '.depl' -r)
        local dir=$( jq <<<$remote '.dir // "run"' -r)

        if   test "$(ssh $env -- sh -c "'ls -ld $depl/$dir/$run          | wc -l'")" = 0
        then fail "analyse-remote:  run does not exist on remote: $(white $run)"
        else ssh $env -- sh -c "'export WB_RUNDIR=../$depl/$dir && cd cardano-node && echo env: $(yellow $env), rundir: $(color blue)\$WB_RUNDIR$(color reset), workbench: $(color yellow)\$(git log -n1)$(color reset) && make analyse RUN=$run'"
        fi
        ;;

    list-hosts | hosts )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$global_rundir/$run

        if test -f "$dir"/node-specs.json
        then jq             'keys | .[]' -r "$dir"/node-specs.json
        else jq '.hostname | keys | .[]' -r "$dir"/meta.json; fi;;

    trim )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$global_rundir/$run
        ## Fall back to the old genesis name.
        local genesis="$dir"/genesis/genesis.shelley.json
        test -f "$genesis" || genesis="$dir"/genesis-shelley.json
        local geneses_orig_dir=$global_rundir/.geneses.orig
        local genesis_orig="$geneses_orig_dir"/$run.orig.json

        if ! run get $run >/dev/null
        then fail "trim" "missing run: $(white $run)"
        fi

        mkdir -p "$geneses_orig_dir" "$dir"/genesis/

        if test -f "$genesis"
        then local size=$(ls -s "$genesis" | cut -d' ' -f1)
             if test "$size" -gt 1000
             then progress "run" "genesis size: ${size}k, trimming.."
                  mv    "$genesis" "$genesis_orig"
                  ln -s "$(realpath $genesis_orig)" "$genesis".orig
                  jq > "$genesis" '
                    .initialFunds = {}
                  | .staking      = {}
                  ' "$genesis_orig"; fi; fi
        ;;

    package | pack )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}

        run trim "$run"
        ( cd $global_rundir
          tar c --zstd --exclude '*/*/*.socket' "$run" > $run.tar.zst
        );;

    compat-meta-fixups | compat-f )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$(run get "$run")

        ## Era is not back-filled here: `allocate` already writes the
        ## canonical `.meta.era` top-level field for every workbench run,
        ## and locli's decoder reaches the historical era via
        ## `.meta.profile_content.era` (the profile JSON snapshot) on
        ## older runs without intervention.
        jq_fmutate "$dir"/meta.json '
           def compat_fixups:
             { genesis:
               { dense_pool_density: .composition.dense_pool_density
               , n_pools:            .composition.n_pools
               }
             };
           . * { meta:
                 { profile_content:   (.meta.profile_content | compat_fixups)
                 }
               }';;

    start )
        local usage="USAGE: wb run $op [--idle] [--scenario NAME] [--analyse] RUN"

        local scenario_override= analyse=yes analysis_can_fail=
        while test $# -gt 0
        do case "$1" in
               --idle )              scenario_override='generic-idle';;
               --scenario | -s )     scenario_override=$2; shift;;
               --no-analysis )       analyse=;;
               --analysis-can-fail | -f )
                                     analysis_can_fail=t;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local run=${1:-?$usage}; shift
        local dir=$(run get "$run")
        test -d "$dir" ||
            fatal "invalid run identifier: $run"

        progress "run" "starting $(with_color white $run)"

        run set-current "$run"

        ## Execute the scenario
        local scenario=${scenario_override:-$(jq -r .scenario "$dir"/profile.json)}
        scenario "$scenario" "$dir"

        (
          # This step is resource intensive so we use a lockfile to avoid
          # running it in parallel to a benchmark.
          acquire_lock
          backend fetch-logs     "$dir"
        )
        backend stop-cluster   "$dir"

        run compat-meta-fixups "$run"
        ;;

    stop )
        local run=${1:-current}
        local dir=$(run get "$run")

        local running_components=($(backend is-running "$dir"))
        if test ${#running_components[*]} -gt 0
        then progress "run" "terminating backend components ($(red ${running_components[*]})).."
             backend stop-cluster "$dir"
             progress "run" "cluster stopped"
        fi
        ;;

    restart )
        local run=$(run current-tag)
        local dir=$(run get "$run")

        test -d "$dir" ||
            fatal "no valid current run to restart:  please set run/current appropriately"

        progress "run" "restarting cluster in the same run directory: $dir"

        run stop                "$run"
        jq_fmutate "$dir"/meta.json '
          { modifiers: { wiped_and_restarted: true }
          } * .
        '
        backend cleanup-cluster "$dir"
        run start-cluster  "$@" "$run"

        msg "cluster re-started in the same run directory: $dir"
        ;;

    * ) usage_run;; esac
}

run_remote_get() {
    local usage='USAGE: run_remote_get ENV DEPLOYMENT RUN REMOTE-TAR-CMD OBJ..'
    local clean=
    while test $# -gt 0
    do case "$1" in
           --clean | -c ) clean='true';;
           -- ) shift; break;;
           --* ) msg "FATAL:  unknown flag '$1'"; fail "$usage";;
           * ) break;; esac; shift; done

    local env=${1:?$usage}; shift
    local depl=${1:?$usage}; shift
    local rdir=${1:?$usage}; shift
    local run=${1:?$usage}; shift
    local remote_tar_cmd=${1:?$usage}; shift
    local objects=($*)

    progress "run_remote_get" "env $(yellow $env) depl $(yellow $depl) run $(white $run)"
    progress "run_remote_get" "tar $(green $remote_tar_cmd)"

    local meta=$(ssh $env -- sh -c "'jq . $depl/$rdir/$run/meta.json'")
    if ! jq . <<<$meta >/dev/null
    then fail "run_remote_get:  malformed $(yellow meta.json) in $(white $run) on $(white $depl)@$(white env)"; fi

    ## Minor validation passed, create & populate run with remote data:
    local dir=$global_rundir/$run
    if test -z "$run" -o -z "$global_rundir"
    then fail "run_remote_get: run=$run global_rundir=$global_rundir"
    elif test -n "$clean"
    then rm -rf "$dir"
    fi
    mkdir -p "$dir"
    jq . <<<$meta > $dir/meta.json

    ## Genesis paths are listed under both layouts: the normalized one used by
    ## current workbench runs and the old one (genesis-shelley.json).
    ## The remote tar uses `--ignore-failed-read`, so listing both is safe.
    local common_run_files=(
        genesis/genesis.alonzo.json
        genesis/genesis.shelley.json
        genesis.alonzo.json
        genesis-shelley.json
        profile.json
        generator/protocol-parameters-queried.json
        generator/plutus-budget-summary.json
    )
    local xs0=(${objects[*]})
    local xs1=(${xs0[*]/#common-run-files/ ${common_run_files[*]}})
    local xs=(${xs1[*]})

    local count=${#xs[*]}
    progress "run | fetch $(white $run)" "objects to fetch:  $(white $count) total:  ${objects[*]}"

    local max_batch=9 base=0 batch
    while test $base -lt $count
    do local batch=(${xs[*]:$base:$max_batch})
       {
           local lbatch=(${batch[*]})
           ssh $env -- \
               sh -c "'files=(${lbatch[*]}); cd $depl/$rdir/$run && { ${remote_tar_cmd} }'" |
               (cd $dir
                tar x --zstd --ignore-zeros ||
                    progress "fetch error" "'files=(${lbatch[*]}); cd $depl/$rdir/$run && ${remote_tar_cmd}'"
               )
           progress "run | fetch $(white $run)" "batch done:  $(yellow ${batch[*]})"
       } &
       sleep 1
       base=$((base + max_batch))
    done
    progress "run | fetch $(white $run)" "batches started, waiting.."
    wait

    progress "run | fetch" "adding manifest"
    jq_fmutate "$dir"/meta.json '.meta.manifest = $manifest
    ' --argjson manifest "$(legacy_run_manifest $dir)"
}

node_cabal_source_at() {
    # FIXME
    echo 0123456789ABCDEF0123456789ABCDEF01234567
}

legacy_run_manifest() {
    local dir=$1
    local node=$(       jq -r '.meta.pins."cardano-node"' $dir/meta.json)
    local node_branch=$(jq -r '.meta.node_commit_spec'    $dir/meta.json)
    local node_ver=$(   jq -r '.meta.node_commit_desc'    $dir/meta.json)

    local args=(
      --arg Node          $node
      --arg NodeBranch    $node_branch
      --arg NodeApproxVer $node_ver
      --arg Network  $(node_cabal_source_at $node ouroboros-network)
      --arg Ledger   $(node_cabal_source_at $node cardano-ledger)
      --arg Plutus   $(node_cabal_source_at $node plutus)
      --arg Crypto   $(node_cabal_source_at $node cardano-crypto)
      --arg Base     $(node_cabal_source_at $node cardano-base)
      --arg Prelude  $(node_cabal_source_at $node cardano-prelude)
    )
    jq '
  { "cardano-node"         : $Node
  , "cardano-node-branch"  : $NodeBranch
  , "cardano-node-version" : $NodeApproxVer
  , "cardano-node-status"  : "unknown"
  , "ouroboros-network"    : $Network
  , "cardano-ledger"       : $Ledger
  , "plutus"               : $Plutus
  , "cardano-crypto"       : $Crypto
  , "cardano-base"         : $Base
  , "cardano-prelude"      : $Prelude
  }
  ' --null-input "${args[@]}"
}

legacy_run_timing() {
    local dir=$1
    local stamp=$(jq -r '.meta.timestamp' $dir/meta.json)

    local args=(
        --argjson stamp $stamp
    )
    jq '
    .meta.profile_content                                       as $prof
  | ($stamp + ($prof.generator.tx_count / $prof.generator.tps)) as $stamp_end
  |
  { future_offset:   "0 seconds"
  , start:           $stamp
  , shutdown_end:    $stamp_end
  , workload_end:    $stamp_end
  , earliest_end:    $stamp_end

  , start_tag:       .meta.tag[:16]
  , start_human:     ($stamp | todateiso8601)
  , systemStart:     ($stamp | todateiso8601)
  , shutdownTime:    ($stamp_end | todateiso8601)
  , workloadEndTime: ($stamp_end | todateiso8601)
  , earliestEndTime: ($stamp_end | todateiso8601)
  }' $dir/meta.json "${args[@]}"
}

expand_runsets() {
    local runs=() rs

    if test $# = 0
    then runs=(current)
    else for rs in $*
         do runs+=($(run run-or-set $rs))
         done
    fi
    echo ${runs[*]}
}

runspec_normalise() {
    local runspec=${1:?$usage}
    local precomma=$(cut -d: -f1 <<<$runspec) run= ident=

    if   test "${runspec::1}" = "/" -o \
              "${runspec::1}" = "." -o \
              "$precomma" = "$runspec"
    then ident="";        run=$runspec
    else ident=$precomma; run=$(cut -d: -f2 <<<$runspec)
    fi

    echo "$ident:$run"
}

runspec_id() {
    cut -d: -f1 <<<$1
}

runspec_run() {
    cut -d: -f2 <<<$1
}

run_ls_cmd() {
    local rundir=$1

    echo 'cd '$rundir' && \
          { find . -mindepth 2 -maxdepth 2 -type f -name meta.json -exec dirname \{\} \; |
            grep -v "current\$\|deploy-logs\$" &&
            find . -mindepth 3 -maxdepth 3 -type f -name *.ede -exec dirname \{\} \; |
            xargs -r dirname
          } |
          cut -c3- |
          sort || true'
}

# Will need to support new manifest format in a backwards compatible manner
run_ls_tabulated_cmd() {
    local rundir=$1 limit=$2

    echo 'cd '$rundir' && \
          find . -mindepth 2 -maxdepth 2 -type f -name meta.json -exec dirname \{\} \; |
          grep -v "current\$\|deploy-logs\$" |
          cut -c3- |
          sort |
          tail -n'$limit' |
          {
            printf "%-60s %-12s %7s %-15s %10s\n" \
                 "run tag" "identifier" "gitrev" "node version" "node branch"
            while read lst_tag; test -n "$lst_tag";
            do printf_args=(
               $(jq ".meta | .manifest as \$manif |
                      \"\\(.ident) \\(\$manif.\"cardano-node\" | .[:7]) \\(\$manif.\"cardano-node-version\") \\(\$manif.\"cardano-node-branch\")\"
                    " -r <$lst_tag/meta.json))
              printf "%-60s %-12s %7s %-15s %10s\n" \
                    $lst_tag ${printf_args[*]}
            done || true
          }'
}

run_ls_sets_cmd() {
    local rundir=$1

    echo 'cd '$rundir'/.sets 2>/dev/null && \
          find -L . -mindepth 3 -maxdepth 3 -type f -name meta.json -exec dirname \{\} \; |
          cut -d/ -f2 |
          sort -u || true'
}

get_node_ghc_version(){
    local node_executable

    if [[ $WB_BACKEND_NAME == nomad* ]]
    then
       node_executable=$(jq --raw-output '.containerPkgs."cardano-node"."nix-store-path"' $WB_BACKEND_DATA/container-specs.json)
       node_executable="$node_executable/bin/cardano-node"
    else
       node_executable=cardano-node
    fi

    if [[ -x $node_executable ]]
    then
        $node_executable +RTS --info | grep 'GHC version' | grep -E -o "([0-9]{1,2}[\.]){2}[0-9]{1,2}"
    else
        echo "unknown"
    fi
}

get_most_significant_git_tag(){
    local lasttag=$(git describe --tags --abbrev=0)
    local commhash=$(git rev-list -n 1 $lasttag)
    local oldesttag=$(git tag --sort=creatordate --points-at $commhash | head -n1)
    echo $oldesttag
}
