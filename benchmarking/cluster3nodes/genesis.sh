#!/usr/bin/env bash

umask 077

SCRIPTDIR=$(dirname $0)
CONFIGDIR="${SCRIPTDIR}/configuration"

start_time=$(date '+%s')
protocol_params="${CONFIGDIR}/protocol-params.json"

parameter_k=2160
protocol_magic=459045235
n_poors=128
n_delegates=3
total_balance=8000000000000000
delegate_share=900000000000000
avvm_entries=128
avvm_entry_balance=10000000000000
not_so_secret=2718281828

tmpdir="`mktemp`.d"
args=(
      --genesis-output-dir           "${tmpdir}"
      --start-time                   "${start_time}"
      --protocol-parameters-file     "${protocol_params}"
      --k                            ${parameter_k}
      --protocol-magic               ${protocol_magic}
      --n-poor-addresses             ${n_poors}
      --n-delegate-addresses         ${n_delegates}
      --total-balance                ${total_balance}
      --delegate-share               ${delegate_share}
      --avvm-entry-count             ${avvm_entries}
      --avvm-entry-balance           ${avvm_entry_balance}
      --real-pbft
      --secret-seed                  ${not_so_secret}
)

set -xe
RUNNER=${RUNNER:-cabal v2-run -v0 --}

${RUNNER} cardano-cli genesis "${args[@]}" "$@"

# move new genesis to configuration
GENHASH=`${RUNNER} cardano-cli "${common[@]}" print-genesis-hash --genesis-json "${tmpdir}/genesis.json" | tail -1`
TARGETDIR="${CONFIGDIR}/${GENHASH:0:5}"
mkdir -vp "${TARGETDIR}"
cp -iav ${tmpdir}/genesis.json ${TARGETDIR}/
cp -iav ${tmpdir}/delegate-keys.*.key ${TARGETDIR}/
cp -iav ${tmpdir}/delegation-cert.*.json ${TARGETDIR}/

if [ -d ${CONFIGDIR}/latest-genesis ]; then
  rm ${CONFIGDIR}/latest-genesis
fi
ln -sf ${GENHASH:0:5} ${CONFIGDIR}/latest-genesis

set -

echo $GENHASH > ${TARGETDIR}/GENHASH
echo "genesis created with hash = ${GENHASH}"
echo "  in directory ${TARGETDIR}"
