#!/bin/sh

test "$1" == "--help" && {
        cat <<EOF
Usage:  $(basename $0) [GENESIS-SUBCMD-ARG..]
EOF
      echo "" >&2; exit 1; }

start_future_offset="15 minutes"
start_time="$(date -d "now + ${start_future_offset}" +%s)"
protocol_params="$(dirname $0)/protocol-params.json"

protocol_magic=459045235
n_poors=128
n_delegates=7
total_balance=8000000000000000
delegate_share=2
avvm_entries=128
avvm_entry_balance=10000000000000
not_so_secret=2718281828

args=(
      --genesis-output-dir           "./genesis.${start_time}"
      --start-time                   "${start_time}"
      --protocol-parameters-file     "${protocol_params}"
      --k 2160
      --protocol-magic               ${protocol_magic}
      --n-poor-addresses             ${n_poors}
      --n-delegate-addresses         ${n_delegates}
      --total-balance                ${total_balance}
      --delegate-share               ${delegate_share}
      --use-hd-addresses             
      --avvm-entry-count             ${avvm_entries}
      --avvm-entry-balance           ${avvm_entry_balance}
      --secret-seed                  ${not_so_secret}
)

set -xe
RUNNER=${RUNNER:-cabal new-run --}
${RUNNER} cardano-cli byron-pbft genesis "${args[@]}" "$@"
