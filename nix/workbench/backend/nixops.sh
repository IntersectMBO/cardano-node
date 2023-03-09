usage_nixops() {
     usage "nixops" "Backend:  manages a cluster using 'nixops'" <<EOF

    Please see documentation for 'wb backend' for the supported commands.

    Nixops-specific:

EOF
}

backend_nixops() {
op=${1:?$(usage_nixops)}; shift

case "$op" in
    name )
        echo 'nixops';;

    is-running )
        local usage="USAGE: wb nixops $op RUN-DIR"
        test -f "${1:?$usage}"/flag/is-running;;

    setenv-defaults )
        local usage="USAGE: wb nixops $op BACKEND-DIR"
        local backend_dir=${1:?$usage}

        setenvjqstr 'deployment' $(basename $(pwd))
        ;;

    allocate-run )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local nixops_backend='aws'

        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --aws )      nixops_backend=aws;;
               --libvirtd ) nixops_backend=libvirtd;;
               * ) break;; esac; shift; done

        local deploy_args=("$@")

        mkdir -p           "$dir"/nixops

        local depl=$(envjqr 'deployment')

        progress "run | nixops" "updating NixOps state"
        local crmod=$(if nixops list | grep --quiet --no-messages -F "| $depl "
                      then echo modify
                      else echo create
                      fi)
        local crmod_args=(
            --deployment $depl
            -I profileJson="$dir"/profile.json

            nix/workbench/backend/nixops/physical-$nixops_backend.nix
        )
        nixops $crmod "${crmod_args[@]}"

        if test $crmod = "modify"
        ## The cluster already exists, so pre-deployment cleanup might be necessary.
        then progress "run | nixops" "cleaning up node nix stores"
             nixops ssh-for-each       \
                    --deployment $depl \
                    --parallel         \
                    -- bash -c '"nix-collect-garbage --delete-old >/dev/null 2>&1"'; fi

        progress "run | nixops" "deploying.."
        export WB_RUN_DIR=$(realpath "$dir")

        local host_resources=($(jq 'keys|.[]' $dir/node-specs.json | xargs echo))
        local host_resources_current=($(nixops info --plain 2>/dev/null | sed 's/^\([a-zA-Z0-9-]*\).*/\1/' | grep -ve '-ip$\|cardano-keypair-\|allow-\|relays-' || true))
        local other_resources=($(nixops info --plain 2>/dev/null | sed 's/^\([a-zA-Z0-9-]*\).*/\1/' | grep  -e '-ip$\|cardano-keypair-\|allow-\|relays-' || true))

        test "${host_resources[*]}" = "${host_resources_current[*]}" ||
            warn "run | nixops | deploy" "requested deployment host set does not match current NixOps deployment host set:  nixops (${host_resources_current[*]}) != requested (${host_resources[*]})"

        local host_count=${#host_resources[*]}
        progress "run | nixops | deploy" "hosts to deploy:  $host_count total:  $(yellow ${host_resources[*]})"

        local max_batch=10
        if test $host_count -gt $max_batch
        then progress "run | nixops | deploy" "that's too much for a single deploy -- deploying in batches of $max_batch nodes"

             progress "run | nixops | deploy" "deploying non-host resources first:  ${other_resources[*]}"
             time nixops_deploy_resources "$dir" ${other_resources[*]}

             local base=0 batch
             while test $base -lt $host_count
             do local batch=(${host_resources[*]:$base:$max_batch})
                time nixops_deploy_resources "$dir" ${batch[*]}
                base=$((base + max_batch))
             done
        else progress "run | nixops | deploy" "that's deployable in one go -- blasting ahead"
             time nixops_deploy_resources "$dir"
        fi
        ;;

    describe-run )
        local usage="USAGE: wb nixops $op RUN-DIR"

        cat <<EOF
  - NixOps deployment:       $(envjq 'deployment')
EOF
        ;;

    start-node )
        fail "NixOps backend does not implement operation:  $1";;
    stop-node )
        fail "NixOps backend does not implement operation:  $1";;

    wait-node )
        local usage="USAGE: wb nixops $op RUN-DIR [NODE-NAME]"
        local dir=${1:?$usage}; shift
        local node=${1:-$(dirname $CARDANO_NODE_SOCKET_PATH | xargs basename)}; shift
        local socket=$(backend_nixops get-node-socket-path "$dir" $node)

        local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' $dir/profile.json) i=0
        echo -n "workbench:  nixops:  waiting ${patience}s for socket of $node: " >&2
        while test ! -S $socket
        do printf "%3d" $i; sleep 1
           i=$((i+1))
           if test $i -ge $patience
           then echo
                progress "nixops" "$(red FATAL):  workbench:  nixops:  patience ran out for $(white $node) after ${patience}s, socket $socket"
                backend_nixops stop-cluster "$dir"
                fatal "$node startup did not succeed:  check logs in $(dirname $socket)/stdout & stderr"
           fi
           echo -ne "\b\b\b"
        done >&2
        echo " $node up (${i}s)" >&2
        ;;

    start-nodes )
        local usage="USAGE: wb nixops $op RUN-DIR [HONOR_AUTOSTART=]"
        local dir=${1:?$usage}; shift

        local depl=$(envjqr 'deployment')

        progress "run | nixops" "starting node services"
        ## XXX: tracer startup, based on jqtest ".node.tracer" "$dir"/profile.json
        nixops ssh-for-each       \
               --deployment $depl \
               --parallel         \
               -- bash -c '"systemctl start cardano-node"'

        local canary='node-0'
        local tolerance=$(jq .analysis.cluster_startup_overhead_s $dir/profile.json)

        if nixops_wait_service "$dir" "$canary" "cardano-node" "$tolerance"
        then node_fetch_debug  "$dir" "$canary"
             fail "nixops:  canary node $canary failed to start 'cardano-node' within $tolerance seconds"
        fi

        local node_commit=$(jq -r '.meta.manifest."cardano-node"' $dir/meta.json)
        # node_wait_for_commit_id 'explorer' "$node_commit"
        node_wait_for_commit_id "$canary" "$node_commit"
        ;;

    start )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local depl=$(envjqr 'deployment')

        progress "run | nixops" "cleaning up cluster node state"
        nixops ssh-for-each       \
               --deployment $depl \
               --parallel         \
               -- bash -c '"for svc in cardano-{node,tracer} systemd-journald;
                            do systemctl stop $svc 2>/dev/null || true;
                            done;
                            rm -rf /var/{lib/cardano-node,log/journal}/*;
                            systemctl start systemd-journald"'

        progress "run | nixops" "starting non-node services"
        ## XXX: tracer startup, based on jqtest ".node.tracer" "$dir"/profile.json
        nixops ssh-for-each       \
               --deployment $depl \
               --parallel         \
               -- bash -c '"systemctl start systemd-journald &&
                            systemctl start cardano-tracer   &&
                            sleep 3s"'
        ;;

    get-node-socket-path )
        local usage="USAGE: wb nixops $op STATE-DIR NODE-NAME"
        local state_dir=${1:?$usage}
        local node_name=${2:?$usage}

        echo -n $state_dir/$node_name/node.socket
        ;;

    start-generator )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local depl=$(envjqr 'deployment')
        local tolerance=$(jq .analysis.cluster_startup_overhead_s $dir/profile.json)

        if nixops_wait_service "$dir" "explorer" "tx-generator" "$tolerance"
        then node_fetch_debug  "$dir" "explorer"
             fail "nixops:  explorer failed to start 'tx-generator' within $tolerance seconds"
        fi

        progress "run | nixops" "waiting until local socket for the generator appears.."
        nixops ssh --deployment $depl \
              'explorer'              \
               -- bash -c '"while ! (journalctl -u cardano-node | grep LocalHandshakeTrace\|LocalSocketUp >/dev/null); sleep 1; echo -ne .; done"'
        progress "run | nixops" "LocalHandshakeTrace/LocalSocketUp seen"

        nixops ssh --deployment $depl \
              'explorer'              \
               -- bash -c '"systemctl start tx-generator"'
        ;;

    wait-node-stopped )
        local usage="USAGE: wb nixops $op RUN-DIR NODE"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        local depl=$(envjqr 'deployment')

        progress "run | nixops" "waiting until $node stops:  ....."

        nixops ssh --deployment $depl \
               $node \
               -- bash -c '"while systemctl --quiet is-active cardano-node;
                            do sleep 1;
                            done"'
        progress "run | nixops" "$node stopped."
        ;;

    wait-pools-stopped )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local i=0
        local pools=$(jq .composition.n_pool_hosts $dir/profile.json)
        local start_time=$(date +%s)
        local full_patience=$(jq "$dir"/profile.json '.analysis.finish_patience')

        touch $dir/flag/cluster-termination

        nixops_wait_for_nonempty_block "$dir" 200 ## generator warmup
        nixops_wait_for_empty_blocks   "$dir" $full_patience

        # nixops ssh-for-each       \
        #        --deployment $depl \
        #        --parallel         \
        #        -- bash -c '"while systemctl --quiet is-active cardano-node;
        #                     do sleep 1;
        #                     done"'

        local elapsed=$(($(date +%s) - start_time))
        # if test -f $dir/flag/cluster-termination
        # then echo " All nodes exited -- after $(yellow $elapsed)s" >&2
        # else echo " Termination requested -- after $(yellow $elapsed)s" >&2; fi
        ;;

    stop-cluster )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local depl=$(envjqr 'deployment')

        progress "run | nixops" "stopping node services"
        ## XXX: tracer startup, based on jqtest ".node.tracer" "$dir"/profile.json
        nixops ssh-for-each       \
               --deployment $depl \
               --parallel         \
               -- bash -c '"systemctl stop cardano-node"'
        ;;

    cleanup-cluster )
        local usage="USAGE: wb nixops $op RUN-DIR"
        local dir=${1:?$usage}; shift

        fatal "'nixops' backend does not implement:  $op";;

    * ) set +x; usage_nixops;; esac
}

function node_wait_for_commit_id() {
    local mach=$1 expected=$2 actual=

    progress_ne "nixops" "checking node commit on $mach:  "
    while actual=$(node_runtime_log_commit_id "$mach");
          test -z "$actual"
    do sleep 1; echo -n '.'; done

    if test "$expected" != "$actual"
    then newline; fail " expected $expected, got $actual"
    else echo " ok, $expected" >&2; fi
}

function node_runtime_apparent_systemstart() {
    nixops ssh $1 -- sh -c '"journalctl -u cardano-node | grep TraceStartLeadershipCheck | head -n2 | tail -n1"' |
        cut -d':' -f4- |
        jq '[ (.at | "\(.[:19])Z" | sub(" ";"T") | fromdateiso8601)
            , .data.slot
            ] | .[0] - .[1] | todateiso8601' -r
}

function node_runtime_log_commit_id() {
    local mach=$1

    nixops ssh "$mach" -- journalctl -u cardano-node |
        (grep commit || true) |
        tail -n1 |
        sed 's/.*"\([0-9a-f]\{40\}\)".*/\1/'
}

function nixops_deploy_resources() {
    local dir=$1; shift
    progress "run | nixops | deploy" "$*"
    if ! nixops deploy                          \
           -I profileJson="$dir"/profile.json   \
           --allow-reboot                       \
           --confirm                            \
           --show-trace                         \
           --no-build-output                    \
           --cores 0                            \
           -j 4                                 \
           --max-concurrent-copy 50             \
           ${1:+--include} "$@"                 \
           >> $dir/nixops/deploy.log 2>&1
    then msg "$(red nixops deployment failed, full log in:  cat $dir/nixops/deploy.log)"
         msg_ne "$(white Browse?) [Y/n] "
         case "$(read ans)" in
             "" | "y" | "yes" ) less $dir/nixops/deploy.log;; esac
         exit 1
    fi
}

function nixops_wait_for_nonempty_block() {
        local dir=$1 now r
        local now=$(date +%s)
        local since=$now
        local patience=400
        local patience_start=$(max "$(genesis_systemstart)" $now)
        local patience_start_pretty=$(date --utc --date=@$patience_start --iso-8601=s)
        local patience_until=$((patience_start + patience))

        progress_ne "run | nixops" "--( waiting for a non-empty block on explorer (patience until $patience_start_pretty + ${patience}s).  Seen empty: 00"
        while now=$(date +%s); test "${now}" -lt ${patience_until}
        do r=$(nixops ssh explorer -- \
               sh -c "'tac /var/lib/cardano-node/{logs/node,*:*/*}.json 2>/dev/null |
                       grep -F MsgBlock |
                       head -n 20 |
                       jq --compact-output \"select(.data.msg.\\\"txIds\\\" != [])\" |
                       wc -l'")
           if test "$r" -ne 0
           then l=$(nixops ssh explorer -- sh -c \
                  "'tac /var/lib/cardano-node/{logs/node,*:*/*}.json 2>/dev/null |
                    grep -F MsgBlock |
                    head -n 20 |
                    jq \".data.msg.\\\"txIds\\\" | select(. != []) | length\" |
                    jq . --slurp --compact-output'")
                echo ", got $l, after $((now - since)) seconds" >&2
                return 0; fi
           e=$(nixops ssh explorer -- sh -c \
              "'tac /var/lib/cardano-node/{logs/node,*:*/*}.json 2>/dev/null |
                grep -F MsgBlock |
                head -n 20 |
                jq --slurp \"map (.data.msg.\\\"txIds\\\" | select(. == [])) | length\"'")
           echo -ne "\b\b"; printf "%02d" "$e"
           sleep 5; done

        echo " patience ran out, stopping the cluster and collecting logs from the botched run." >&2
        fatal "No non-empty blocks reached the explorer in ${patience} seconds -- is the cluster dead (genesis mismatch?)?"
}

function nixops_wait_for_empty_blocks() {
        local dir=$1 full_patience=$2
        local poll_delay=20
        local patience=$full_patience
        local anyblock_patience=$full_patience

        echo -n "--( waiting for ${full_patience} empty blocks (txcounts): "
        local last_blkid='01234'
        local news=
        while test $patience -gt 1 -a $anyblock_patience -gt 1
        do while news=$(nixops ssh explorer -- sh -c \
                        "'set -euo pipefail;
                          { echo \"{ data: { msg: { blkid: 0, txIds: [] }}}\";
                            tac /var/lib/cardano-node/{logs/node,*:*/*}.json 2>/dev/null; } |
                            grep -F MsgBlock |
                            jq --compact-output \".data.msg | { blkid: (.blockHash | ltrimstr(\\\"\\\\\\\"\\\") | rtrimstr(\\\"\\\\\\\"\\\")), tx_count: (.txIds | length) } \"'" |
                        sed -n '0,/'$last_blkid'/ p' |
                        head -n-1 |
                        jq --slurp 'reverse | ## undo order inversion..
                          { txcounts: map (.tx_count)
                          , blks_txs: map ("\(.tx_count):\(.blkid)")
                          , last_blkid: (.[-1] // { blkid: $blkid}
                                        | .blkid)
                          }
                          ' --arg blkid ${last_blkid})
                 last_blkid=$(jq --raw-output .last_blkid <<<$news)
                 if test -n "${oneshot_action}"
                 then $oneshot_action; oneshot_action=; fi
                 echo -n " "
                 if test -z "${verbose}"
                 then jq -cj '.txcounts'
                 else echo -n "p${patience}"
                      jq -cj '.blks_txs | join(",") | "["+.+"]"'
                 fi <<<$news
                 ## A reasonable delay to see a new block.
                 sleep $pool_delay
                 jqtest '.txcounts
                        | length == 0
                          or (all (. == 0) | not)' <<<$news &&
                 test "$anyblock_patience" -gt 1
           do if jqtest '.txcounts | length != 0' <<<$news
              then patience=${full_patience}
                   anyblock_patience=${full_patience}
                   test -z "${verbose}" || echo -n "=${patience}"
              else anyblock_patience=$((anyblock_patience - 1)); fi; done
           patience=$((patience - 1))
           test -z "${verbose}" || echo -n "p${patience}a${anyblock_patience}t$(jq .txcounts <<<$news)"
        done | tee "last-run/logs/block-arrivals.gauge"
        echo

        verbose "run | nixops" "test termination:  patience=$patience.  anyblock_patience=$anyblock_patience"
        if test "$anyblock_patience" -le 1
        then errprint "No blocks reached the explorer in ${full_patience} seconds -- has the cluster died?"
             return 1; fi
}

node_fetch_debug() {
    local dir=${1:-run/current}
    local node=${2:-node-0}

    local depl=$(envjqr 'deployment')
    local debugdir=$dir/debug

    progress "nixops" "fetching debug logs:  $(yellow $node) -> $(yellow $debugdir)"

    nixops ssh --deployment $depl \
            "$node" -- bash -c    \
            '"cd /var/lib/cardano-node/
              ls -l         > files.stateDir
              ls -l ../keys > files.keys

             { systemctl status cardano-node;
               systemctl status cardano-tracer;
               systemctl status tx-generator;
             } > systemctl.'$node'.status  2>/dev/null;

             { systemctl stop cardano-node;
               systemctl stop cardano-tracer;
               systemctl stop tx-generator;
             } > /dev/null         2>&1;

             journalctl -u cardano-node   > stdout.'$node'.node;
             journalctl -u cardano-tracer > stdout.'$node'.tracer;
             journalctl -u tx-generator   > stdout.'$node'.generator;

             tar c stdout.* systemctl.* files.* --zstd"' | \
    ( mkdir "$debugdir"
      cd    "$debugdir"
      tar x --zstd
    )
}

nixops_wait_service() {
    local usage="USAGE:  nixops_wait_service RUNDIR HOSTNAME SERVICENAME TOLERANCE"
    local dir=${1:-run/current}
    local node=${2:-node-0}
    local service=${3:?$usage}
    local tolerance=${4:?$usage}

    progress_ne "nixops" "waiting for service ${service}@${node}: ..."
    while { tolerance=$((tolerance - 1));
            test $tolerance -ne 0 &&
            test ! -z "$(nixops ssh $node -- bash -c \
                        "systemctl -q is-active $service && echo yes")";
                }
    do echo -ne "\b\b\b"; printf "%03d" "$tolerance"; sleep 1
    done
    newline

    test $tolerance = 0
}
