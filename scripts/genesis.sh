#!/bin/sh

start_future_offset="15 minutes"
start_time="$(date -d "now + ${start_future_offset}" +%s)"
protocol_params="$(dirname $0)/protocol-params.json"

protocol_magic=633343913
n_poor=128
n_delegate=7
total_balance=80000000000000
delegate_share=2
avvm_entries=128
avvm_entry_balance=10000000000
not_so_secret=2718281828

systemVersion="$1"; shift || true
case "$systemVersion" in
        byron-legacy | byron-pbft | --help ) true;;
        * ) echo "Usage:  $(basename $0) --byron-{legacy,pbft} [GENESIS-TOOL-ARG..]" >&2; exit 1;; esac

set -xe
cabal new-run --                                      \
      genesis-tool ${systemVersion} genesis           \
      --genesis-output-dir ./genesis.${systemVersion} \
      --start-time "${start_time}"                    \
      --protocol-parameters-file "${protocol_params}" \
      --k 2160                                        \
      --protocol-magic ${protocol_magic}              \
      --n-poor-addresses ${n_poor}                    \
      --n-delegate-addresses ${n_delegate}            \
      --total-balance ${total_balance}                \
      --delegate-share ${delegate_share}              \
      --use-hd-addresses                              \
      --avvm-entry-count ${avvm_entries}              \
      --avvm-entry-balance ${avvm_entry_balance}      \
      --secret-seed ${not_so_secret}                  \
      "$@"
