#!/bin/sh

function usage() {
        cat >&2 <<EOF
$1

Usage:
       $(basename $0)
           [--limit BLOCKS=20]
           ( --ip IPADDR | --cluster IPADDRMASK MIN MAX )
           INPUT-FILE
           COMMAND

Generates a .dot tree of blocks implied within a log emitted by a Cardano node,
(either Rewrite or Legacy) & shows it in Eye-of-Gnome.

Commands:

   dot             Output the raw .dot
   png [FILENAME]  Write out a dot-rendered .png file & open in EOG

EOF
}

limit=20
ip='10.1.0.2'
genesis_block_hash='12da51c484b5310fe26ca06ab24b94b323cde3698a0a50cb3f212abd08c2731e'
ebb_block_hash='d2cbcf912983d670f0cb20299372dccfa045a4314bccf4cc84846db2dc8ffcbf'
delegate_keydir=
delegation=
while test -n "$1"
do case "$1" in
           --limit )              limit="$2"; shift;;
           --ip )                 ip="$2"; shift;;
           --cluster )            ipmask="$2"; ipmin="$3"; ipmax="$4"; shift 3;;
           --genesis-block-hash ) genesis_block_hash="$2"; shift;;
           # --delegate-keydir ) delegate_keydir="$2"; delegation=$(mktemp -t XXXXXXXXX.dlg); shift;;
           --help | "--"* ) usage; exit 1;;
           * ) break;; esac; shift; done

input="${1:-cluster.log}"; shift || true
mode="${1:-png}"; shift || true

. $(dirname $0)/unlogging.sh

case "${mode}" in
headers-seen )
        seq 0 ${limit} | slot_numbers_loop;;
esac

declare -A ip_pids
if   test -n "${ipmask}"
then fill_ip_maps "${input}" ${ipmask} ${ipmin} ${ipmax}
     for i in $(seq ${ipmin} ${ipmax}); do
     ip=$(printf $ipmask $i)
     process_one_ip $ip "${input}" "${mode}" "$@"
     done
elif test -n "${ip}"
then fill_ip_maps_entry "${input}" "${ip}"
     process_one_ip $ip "${input}" "${mode}" "$@"
else usage "ERROR:  required option missing: --ip IPADDR or --cluster IPADDRMASK MIN MAX"
fi

case "${mode}" in
        ## View outputs, potentially multiple at once.
png )
        eog $(if test -n "${ipmask}"
              then ip_input_output "$(printf ${ipmask} '*')" ${input}
              else ip_input_output ${ip}                     ${input}; fi);;
esac
