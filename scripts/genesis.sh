#!/usr/bin/env bash

OUTPUTDIR=${1?scripts.genesis.Error: no output directory specified}

. $(dirname $0)/lib.sh
CLI="$(executable_quiet_runner cardano-cli)"

DATE=date
OS=$(uname -s)
if [ "$OS" = "Darwin" ]; then
  DATE=gdate
fi
start_future_offset="1 minute"
start_time="$(${DATE} -d "now + ${start_future_offset}" +%s)"
protocol_params="${scripts}/protocol-params.json"

parameter_k=2160
protocol_magic=459045235
n_poors=128
n_delegates=7
total_balance=8000000000000000
delegate_share=0.9
avvm_entries=128
avvm_entry_balance=10000000000000
not_so_secret=2718281828

tmpdir="`mktemp`.d"
args=(
      --genesis-output-dir           "${tmpdir}"
      --start-time                   "${start_time}"
      --protocol-parameters-file     "${protocol_params}"
      --k                            "${parameter_k}"
      --protocol-magic               "${protocol_magic}"
      --n-poor-addresses             "${n_poors}"
      --n-delegate-addresses         "${n_delegates}"
      --total-balance                "${total_balance}"
      --avvm-entry-count             "${avvm_entries}"
      --avvm-entry-balance           "${avvm_entry_balance}"
      --delegate-share               "${delegate_share}"
      --real-pbft
      --secret-seed                  "${not_so_secret}"
)

set -xe

${CLI} genesis "${args[@]}"

# move new genesis to configuration
GENHASH=`${CLI} print-genesis-hash --genesis-json "${tmpdir}/genesis.json" | tail -1`
mkdir -vp "${OUTPUTDIR}"
cp -iav ${tmpdir}/genesis.json "${OUTPUTDIR}"/
cp -iav ${tmpdir}/delegate-keys.*.key "${OUTPUTDIR}"/
cp -iav ${tmpdir}/delegation-cert.*.json "${OUTPUTDIR}"/

set -

echo $GENHASH > "${OUTPUTDIR}"/GENHASH
echo "genesis created with hash = ${OUTPUTDIR}"
echo "  in directory ${OUTPUTDIR}"
