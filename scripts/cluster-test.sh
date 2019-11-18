#!/bin/sh

## Prerequisites:
#
#  1. Virtualisation enabled
#  2. Nix installed in the system
#  3. IOHK Hydra binary cache is added to the /etc/nix/nix.conf
#     ..otherwise the build might fail, or will take hours.
#  4. Nix 'kvm' feature is available.

if ! (grep 'hydra.iohk.io' /etc/nix/nix.conf >/dev/null || grep 'hydra.iohk.io' $HOME/.config/nix/nix.conf >/dev/null)
then cat <<EOF
ERROR: the IOHK Hydra binary cache is not registered with Nix.

Please ensure that your /etc/nix/nix.conf has:

  1. https://hydra.iohk.io in either the 'substituters' or the 'binary-caches' options
  2. hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= in the 'public-keys' option

EOF
     exit 1; fi

if ! (grep 'system-features =.*kvm' /etc/nix/nix.conf >/dev/null || grep 'system-features.*=.*kvm' $HOME/.config/nix/nix.conf >/dev/null)
then cat <<EOF
ERROR: the 'kvm' feature is not advertised by Nix.

Please ensure that your /etc/nix/nix.conf has 'kvm' in its 'system-features' option.

EOF
     exit 1; fi

while test -n "$1"
do case "$1" in
           --cls ) echo -en "\ec";; * ) break;; esac; shift; done
###
###
logfile="cluster.$(date +%s).$(git rev-parse HEAD | cut -c-16).log"
rm -f cluster.log
ln -s ${logfile} cluster.log
nix-build -A nixosTests.chairmansCluster --show-trace --arg interactive true "$@" 2>&1 |
        tee ${logfile}
