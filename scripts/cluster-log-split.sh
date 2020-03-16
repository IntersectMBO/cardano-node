#!/bin/sh

usage() {
        cat <<EOF

Usage:

  $(basename $0) [FLAGS..] LOGFILE [KEYDIR] [OUTPUT-DIR]

Analyse Chairman's cluster LOGFILE, augmented with KEYDIR, containing
the set of 7 Byron legacy delegate private keys, and which defaults to
'configuration/mainnet-ci'.

Output results in OUTPUT-DIR, which defaults to LOGFILE, sans the '.log' suffix.

Flags:

  --print        Print deduced maps at the end of the run

  --help         Print this help & exit

EOF
}

print=
limit=16
while test -n "$1"
do case "$1" in
           --print )              print=t;;
           --limit )              limit="$2"; shift;;
           --help | "--"* ) usage; exit 1;;
           * ) break;; esac; shift; done

. scripts/unlogging.sh

logfile="$1"
test -f "${logfile}" -a -r "${logfile}" || {
        error "Arg 1 is not a readable logfile: '${logfile}'"
        usage
        exit 1
        }
keydir="${2:-$(dirname $0)/../configuration/mainnet-ci}"
test -d "${keydir}" -a -f "${keydir}/key6.sk" || {
        error "Arg 2 is not a directory containing Byron delegate key files: '${keydir}'"
        usage
        exit 1
        }
out="${3:-${logfile%%.log}}"

cardano_cli='cardano-cli/bin/cardano-cli'
prepare_dependencies() {
        if test ! -L 'cardano-cli' -o ! -x ${cardano_cli}
        then rm -f cardano-cli
             nix-build -o 'cardano-cli' -A haskellPackages.cardano-node.components.exes.cardano-cli; fi
}
prepare_dependencies

all_ids="$(seq 1 7) 9"
node_ids="$(seq 1 7)"

set -u
mkdir -p "${out}"

fgrep 'cardano-node_-start'        "${logfile}" > "${out}/log-rewrites"
fgrep 'byron-proxy-start'          "${logfile}" > "${out}/log-proxy"
fgrep 'cardano-node-legacy_-start' "${logfile}" > "${out}/log-legacies"

declare -a maps
reg_maps() {
        for x in $*; do maps+=($x); done
}
dump_maps() {
        for arr in ${maps[@]}
        do for k in $(eval "echo \${!${arr}[@]}")
           do eval "echo ${arr}[${k}]=\${${arr}[${k}]}"; done; done
}

###
###
###
declare -A ips ip_ids dirs
reg_maps   ips ip_ids dirs
for id in ${all_ids}
do ip='127.1.0.'$id
   ips[$id]=$ip
   ip_ids[$ip]=$id
   dirs[$id]=${out}/${id}
   mkdir -p                                     ${dirs[$id]}
   echo -n ${ip}                              > ${dirs[$id]}/ip
done

function get() {
        cat "${dirs[$1]}/$2"
}

reg_maps id_types id_pids id_filters
for id in ${all_ids}
do fill_id_maps_entry_for_ip "${logfile}" "${ips[$id]}"
   echo -n ${id_types[$id]}                   > ${dirs[$id]}/type
   echo -n ${id_pids[$id]}                    > ${dirs[$id]}/pid
   grep "${id_filters[$id]}" "${logfile}"     > ${dirs[$id]}/log || true
   cut -d: -f2- < ${dirs[$id]}/log | cut -c2- > ${dirs[$id]}/log-clean
done

if test "$(get 1 'type')" != 'legacy'
then error "First node is not a legacy, cannot reconstruct the delegation map."
     exit 1; fi

delegation_map="$(get 1 'log' | legacy_extract_genesis_delegation "")"
echo "$delegation_map" > "${out}/delegation-map"

declare -A id_key_hash id_key_base64 id_key_hex
declare -A id_leader_hex  leader_hex_id
declare -A id_leader_hash leader_hash_id
reg_maps   id_key_hash id_key_base64 id_key_hex
reg_maps   id_leader_hex  leader_hex_id
reg_maps   id_leader_hash leader_hash_id
for i in ${node_ids}
do props="$(legacy_key_properties ${keydir}/key$((i-1)).sk)"
   dir=${dirs[$i]}
     id_key_hash[$i]="$(echo "${props}" | cut -d\| -f1)"
   id_key_base64[$i]="$(echo "${props}" | cut -d\| -f2)"
      id_key_hex[$i]="$(echo "${props}" | cut -d\| -f3)"
   echo -n ${id_key_hash[$i]}                 > ${dirs[$i]}/key-hash
   echo -n ${id_key_base64[$i]}               > ${dirs[$i]}/key-base64
   echo -n ${id_key_hex[$i]}                  > ${dirs[$i]}/key-hex

   id_leader_hex[$i]=${id_key_hex[$i]:0:8}
   leader_hex_id[${id_leader_hex[$i]}]=$i

   echo -n ${id_leader_hex[$i]}               > ${dirs[$i]}/leader-hex

   id_leader_hash[$i]=${id_key_hash[$i]:0:8}
   leader_hash_id[${id_leader_hash[$i]}]=$i
   echo -n ${id_leader_hash[$i]}              > ${dirs[$i]}/leader-hash
done

## TODO: think of something better:
leader_hex_id['genesis']='0'
id_types['0']='proxy'

for id in ${all_ids}
do kind=${id_kinds[$id]}
   ${kind}_log_headers_seen_messages_raw        ${dirs[$id]}/log \
                                              > ${dirs[$id]}/headers-seen

   headers_seen_messages_to_internal          < ${dirs[$id]}/headers-seen \
                                              > ${dirs[$id]}/headers-seen-internal

   ${kind}_log_header_messages                  ${dirs[$id]}/log \
                                              > ${dirs[$id]}/headers

   ${kind}_header_messages_to_internal_raw      ${genesis_block_hash} \
                                              < ${dirs[$id]}/headers \
                                              > ${dirs[$id]}/headers-internal-raw

   ${kind}_merge_internal                     < ${dirs[$id]}/headers-internal-raw \
                                              > ${dirs[$id]}/headers-internal

   internal_to_dot "${limit}"                 < ${dirs[$id]}/headers-internal \
   | emit_dot                                 > ${dirs[$id]}/dot

   dot -T'png:cairo'                          < ${dirs[$id]}/dot \
                                              > ${dirs[$id]}/graph.png
done

test -n "${print}" &&
        dump_maps
