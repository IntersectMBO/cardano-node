#!/usr/bin/env bash
# shellcheck disable=SC1090

app_usage() {
        cat <<EOF

Run this like:

   scripts/mainnet-via-fetcher.sh --epochs 3 --nix --profile

This works in two phases, prefetch and benchmark:

   Phase 1:  preload the prefetcher's ChainDB, up to desired limit.

   Phase 2:  run the second node as the benchmark, up to desired slot limit,
             while feeding it blocks from the preloaded local prefetcher,
             while also preventing said prefetcher from syncing mainnet further.

  App options (going BEFORE common options OR IGNORED):

    --normal / --silent         enable/disable tracing
    --epochs / --slots          at which point to stop
    --skip-prefetch             skip the prefetch phase
    --skip-benchmark            skip the benchmark phase

  Common options (going AFTER common options or LATTER ARE IGNORED):

    --profile                   enable profiling of the benchmarked node
    --nix / --cabal / --stack   pick your poison
    --help                      see for more common options

EOF
}

epoch_limit=3
mainnet_k=2160
mainnet_epoch_slots=$((10 * mainnet_k))
slot_limit=
skip_prefetch=
skip_benchmark=
via_fetcher_mode='mainnet-via-fetcher'
while test -n "$1"
do case "$1" in
           --normal )   via_fetcher_mode='mainnet-via-fetcher';;
           --silent )   via_fetcher_mode='mainnet-silent';;
           --epochs )   epoch_limit=$2; shift;;
           --slots )    slot_limit=$2; shift;;
           --skip-prefetch )
                        skip_prefetch=t;;
           --skip-benchmark )
                        skip_benchmark=t;;
           --app-help ) app_usage; exit 1;;
           * ) break;; esac; shift; done

. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh
. "$(dirname "$0")"/lib-node.sh

if test -z "${slot_limit}"
then slot_limit=$((epoch_limit * mainnet_epoch_slots))
     limit_desc=${epoch_limit}' epoch ('${slot_limit}' slots)'
else limit_desc=${slot_limit}' slots'
fi

shutdown() {
        pkill --pgroup $$ 2>/dev/null
}
## Terminate children, whatever they may be..
trap shutdown EXIT

prefetch_phase()
{
        echo "Prefetching up to ${limit_desc} worth of mainnet to local ChainDB.."
        local RUN_NODE_ARGS=(
                --no-profile
                --config-name     'mainnet-silent'
                --topology-name   'mainnet'
                --state           'mainnet'
                ## non-run_node args follow:
                --shutdown-on-slot-synced "${slot_limit}"
        )
        if run_node_quiet "${RUN_NODE_ARGS[@]}"
        then echo "Local ChainDB preloaded with mainnet chain up to ${limit_desc}"
        else
                 echo -e '\n' >&2
                 pgrep -fal 'cardano-node'
                 echo -e '\nMainnet prefetch failed, perhaps some unexpected "cardano-node" processes are running (see above)?\n' >&2
                 exit 1
        fi
}

benchmark_phase()
{
        rm -rf 'db/mainnet-via-fetcher'

        local SERVER_RUN_NODE_ARGS=(
                --no-profile
                --config-name     'mainnet-silent'
                --topology-name   'excommunicated'
                --state           'mainnet'
                --port            3002
        )
        BENCHMARKED_RUN_NODE_ARGS=(
                --config-name     "${via_fetcher_mode}"
                --topology-name   'mainnet-via-fetcher'
                --state           "${via_fetcher_mode}"
                --profile-suffix  "${slot_limit}slots.${via_fetcher_mode}"
                ## non-run_node args follow:
                --shutdown-on-slot-synced "${slot_limit}"
        )

        echo     'Starting local ChainSync server..'
        run_node_quiet "${SERVER_RUN_NODE_ARGS[@]}" &
        sleep 1

        echo     "Starting local ChainSync benchmark for ${limit_desc}.."
        run_node_quiet "${BENCHMARKED_RUN_NODE_ARGS[@]}"
}

###
### Main
###
prebuild "cardano-node"

#
#  Phase 1:  preload the prefetcher's ChainDB, up to desired limit.
#
test -z "${skip_prefetch}" &&
  prefetch_phase

#
#  Phase 2:  run the second node as the benchmark, up to desired slot limit,
#            while feeding it blocks from the local prefetcher,
#            while also preventing said prefetcher from syncing mainnet further.
#
test -z "${skip_benchmark}" &&
  benchmark_phase
