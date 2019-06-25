#!/bin/sh

start_future_offset="15 minutes"
start_time="$(date -d "now + ${start_future_offset}" +%s)"
protocol_params="$(dirname $0)/protocol-params.json"

protocol_magic=633343912
n_poor=128
n_rich=7
total_balance=80000000000000
richmen_share=2
avvm_entries=128
avvm_entry_balance=10000000000
not_so_secret=2718281828

set -xeu

cabal new-run --                                      \
      genesis-tool                                    \
      full-byron-genesis                              \
      --start-time "${start_time}"                    \
      --protocol-parameters-file "${protocol_params}" \
      --k 2160                                        \
      --protocol-magic ${protocol_magic}              \
      --n-poor-addresses ${n_poor}                    \
      --n-richmen-addresses ${n_rich}                 \
      --total-balance ${total_balance}                \
      --richmen-share ${richmen_share}                \
      --use-hd-addresses                              \
      --avvm-entry-count ${avvm_entries}              \
      --avvm-entry-balance ${avvm_entry_balance}      \
      --secret-seed ${not_so_secret}                  \
      --genesis-output-dir ./genesis.out
