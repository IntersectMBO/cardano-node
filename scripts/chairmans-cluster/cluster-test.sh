#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2034

## Prerequisites:
#
#  1. Virtualisation enabled
#  2. Nix installed in the system
#  3. IOHK Hydra binary cache is added to the /etc/nix/nix.conf
#     ..otherwise the build might fail, or will take hours.
#  4. Nix 'kvm' feature is available.

if ! (grep 'hydra.iohk.io' /etc/nix/nix.conf >/dev/null || grep 'hydra.iohk.io' "$HOME"/.config/nix/nix.conf >/dev/null)
then cat <<EOF
ERROR: the IOHK Hydra binary cache is not registered with Nix.

Please ensure that your /etc/nix/nix.conf has:

  1. https://hydra.iohk.io in either the 'substituters' or the 'binary-caches' options
  2. hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= in the 'public-keys' option

EOF
     exit 1; fi

if ! (grep 'system-features =.*kvm' /etc/nix/nix.conf >/dev/null || grep 'system-features.*=.*kvm' "$HOME"/.config/nix/nix.conf >/dev/null)
then cat <<EOF
ERROR: the 'kvm' feature is not advertised by Nix.

Please ensure that your /etc/nix/nix.conf has 'kvm' in its 'system-features' option.

EOF
     exit 1; fi

scripts_dir=$(dirname "$0")/..

__COMMON_SRCROOT=$(realpath "${scripts_dir}/..")
. "${scripts_dir}"/common.sh
. "${scripts_dir}"/lib.sh
. "${scripts_dir}"/lib-cli.sh
###
###

mkdir -p 'logs'
mnemonic=$(generate_mnemonic)
logfile="logs/cluster.${mnemonic}.log"
rm -f cluster.log
ln -s ${logfile} cluster.log
announce() {
        cat <<EOF
###
###  Logfile:   ${logfile}
###  Commit:    $(git log -n1 --pretty=format:"%Cblue%H %Cred%cd %Creset%s")
###  Checkout:  ${status}
###
###  Mnemonic:  ${mnemonic}
###
EOF
}
announce
trap announce EXIT

interactive=false
nix-build -A nixosTests.chairmansCluster --show-trace --arg config "{ interactive = ${interactive}; }" "$@" 2>&1 |
        tee "${logfile}"

"${scripts_dir}"/cluster-log-split.sh '--print' "${logfile}"
