#!/usr/bin/env bash

test "$1" == "--help" && {
        cat <<EOF
Usage:  $(basename $0) [GENESIS-SUBCMD-ARG..]
EOF
      echo "" >&2; exit 1; }

umask 077

CONFIGDIR="$(realpath ${SCRIPTDIR}/../configuration)"
. $(dirname $0)/lib-mode.sh
CLI="$(executable_quiet_runner cardano-cli)"

DATE=date
OS=$(uname -s)
if [ "$OS" = "Darwin" ]; then
  DATE=gdate
fi
start_future_offset="1 minute"
start_time="$(${DATE} -d "now + ${start_future_offset}" +%s)"
protocol_params="${SCRIPTDIR}/protocol-params.json"

parameter_k=2160
protocol_magic=459045235
n_poors=128
n_delegates=7
total_balance=8000000000000000
delegate_share=900000000000000
avvm_entries=128
avvm_entry_balance=10000000000000
not_so_secret=2718281828

tmpdir="`mktemp`.d"
common=(
        --log-config                 "${CONFIGDIR}/configuration-silent.yaml"
        --real-pbft
)
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
      --secret-seed                  ${not_so_secret}
)

set -xe

${CLI} "${common[@]}" genesis "${args[@]}" "$@"

# move new genesis to configuration
TARGETDIR="${CONFIGDIR}/genesis"
GENHASH=`${CLI} "${common[@]}" print-genesis-hash --genesis-json "${tmpdir}/genesis.json" | tail -1`
mkdir -vp "${TARGETDIR}"
cp -iav ${tmpdir}/genesis.json "${TARGETDIR}"/
cp -iav ${tmpdir}/delegate-keys.*.key "${TARGETDIR}"/
cp -iav ${tmpdir}/delegation-cert.*.json "${TARGETDIR}"/

set -

echo $GENHASH > "${TARGETDIR}"/GENHASH
echo "genesis created with hash = ${GENHASH}"
echo "  in directory ${TARGETDIR}"
