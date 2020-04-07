ebb_block_hash=d0433b13f4454997db3b67ba9aee45b1d91f00bfbaef1db5c5fa8bf2bb052413
genesis_block_hash=ec4fafee43d64b009eaae5353290167250b70694528ebbd479619673128e9cc2

function ip_input_output() {
        local ip="$1" input="$2"
        echo -n "${input}.${ip}.png"
}
function deduce_ip_properties () {
        local ip="$1" input="$2"
        local legacy_pid=$(cluster_log_ip_legacy_pid "${input}" "${ip}")
        local rewrite_pid=$(cluster_log_ip_rewrite_pid "${input}" "${ip}")
        local proxy_pid=$(cluster_log_ip_proxy_pid "${input}" "${ip}")
        if   test -n "${rewrite_pid}"
        then kind='rewrite'
             type='rewrite'
             pid=${rewrite_pid}
             filter="cardano-node_-start\[${pid}\]: ";
        elif test -n "${legacy_pid}"
        then kind='legacy'
             type='legacy'
             pid=${legacy_pid}
             filter="cardano-node-legacy_-start\[${pid}\]: "
        elif test -n "${proxy_pid}"
        then kind='legacy'
             type='proxy'
             pid=${proxy_pid}
             filter="byron-proxy-start\[${pid}\]: ";
        else fail "no PID for node IP address '${ip}' in ${input:-<stdin>}"
        fi
        echo -n "${pid}|${type}|${kind}|${filter}"
}
function fill_ip_maps () {
        local input="$1" ipmask="$2" ipmin="$3" ipmax="$4"
        local i=
        for i in $(seq ${ipmin} ${ipmax})
        do fill_id_maps_entry_for_ip "${input}" $(printf $ipmask $i); done
}

declare -A id_pids id_types id_kinds id_filters
function fill_id_maps_entry_for_ip () {
        local input="$1" ip="$2"
        local id=${ip_ids[$ip]}
        local props="$(deduce_ip_properties ${ip} ${input})"

        id_pids[$id]="$(echo    "${props}" | cut -d\| -f1)"
        id_types[$id]="$(echo   "${props}" | cut -d\| -f2)"
        id_kinds[$id]="$(echo   "${props}" | cut -d\| -f3)"
        id_filters[$id]="$(echo "${props}" | cut -d\| -f4)"
}

function process_one_ip () {
        local ip="$1" input="$2" mode="$3"; shift 3
        local kind=${ip_kinds[$ip]}
        local filter="${ip_filters[$ip]}"

        maybe_filter ${input} "${filter}" | case "${mode}" in
                cat )   cat;;
                less )  less -R;;
                grep )  egrep "$@";;
                headers-seen-raw )
                        ${kind}_log_headers_seen_messages_raw "";;
                headers-seen-internal )
                        ${kind}_log_headers_seen_messages_raw ""   |
                                headers_seen_messages_to_internal  |
                                sed "s/ ${ip_pids[$ip]} / ${ip} /";;
                headers-seen )
                        ${kind}_log_headers_seen_messages_raw ""   |
                                headers_seen_messages_to_internal  |
                                head -n ${limit}                   |
                                sed "s/ ${ip_pids[$ip]} / ${ip} /" |
                                ip_slot_timings_loop;;
                internal-raw )
                        ${kind}_log_header_messages "" |
                                ${kind}_header_messages_to_internal_raw ${genesis_block_hash};;
                internal )
                        ${kind}_log_header_messages "" |
                                ${kind}_header_messages_to_internal_raw ${genesis_block_hash} |
                                ${kind}_merge_internal;;
                dot )
                        local output="$(ip_input_output ${ip} ${input})"
                        log_dot "" "${kind}" "${limit}" "${genesis_block_hash}";;
                png )
                        local output="$(ip_input_output ${ip} ${input})"
                        log_dot "" "${kind}" "${limit}" "${genesis_block_hash}" |
                                dot -T"png:cairo" > "${output}";;

                * )
                        usage "ERROR:  bad subcommand '${mode}'";; esac
}

function emit_dot() {
        cat <<EOF
digraph forks {
  bgcolor="${color[bg]}"
  color="${color[fg]}"
  node [shape=record]
$(cat)
}
EOF
}

function log_dot() {
        local input="$1" kind="$2" limit="$3" genesis_block_hash="$4"
        ${kind}_log_header_messages "${input}" |
                ${kind}_header_messages_to_internal_raw ${genesis_block_hash} |
                ${kind}_merge_internal |
                emit_dot
}

function rewrite_log_headers_seen_messages_raw() {
        local input="$1"
        egrep 'TraceAddBlockEvent.SwitchedToChain|ChainSyncClientEvent.TraceDownloadedHeader' ${input} |
        grep -v 'genesis(origin)\|'$(echo $ebb_block_hash | cut -c1-7)
}
function headers_seen_messages_to_internal() {
        local processRewrite='s/^[^0-9]*\([0-9]*\.[0-9]\).*\[\([0-9]*\)\]:.*"tip":"\([0-9a-f]*\)@\([0-9]*\).*$/\1 \2 \3 \4/'
        sed -e "${processRewrite}" |
        sort --key 4 --numeric --unique
}

function rewrite_log_header_messages() {
        local input="$1"
        fgrep 'TraceAddBlockEvent.AddedToCurrentChain' ${input} |
          sed 's/^[^0-9]*\([0-9]*\)\.\(.*\)"slotNo"\(.*\)$/\2"stamp":\1,"slotNo"\3/' |
          cut -d\] -f5- | cut -c2- |
          jq '  .event.headers
              | map ( " \(.stamp) \(.hash) \(.prevhash) \(.slotNo) \(.delegate)" )
              | .[]
             ' |
          tr -d \" |
          sed 's/genesis 0 null/genesis -1 genesis/; s/pub://' |
          { read stamp hash prevhash slot delegate
            while test -n "${delegate}"
            do id=${leader_hex_id[${delegate}]}
               echo "${stamp} ${hash} ${prevhash} ${slot} ${delegate} ${id}"
               read stamp hash prevhash slot delegate; done; }
}

function rewrite_header_messages_to_internal_raw() {
        cat
}

function rewrite_merge_internal() {
        sort --key 4 --numeric --unique | uniq -f1
}

function legacy_log_headers_seen_messages_raw() {
        local input="$1"
        grep ':     \(hash\|slot\): ' ${input}        |
        grep -v "hash: ${ebb_block_hash}"             |
        sed -e 's/^[^0-9]*\([0-9]*\.[0-9]\).*\[\([0-9]*\)\]\(:.*hash:.*\)$/::\1 \2\n\3/' |
        cut -d':' -f3-                                |
        sed "s/ \([0-9]*\).. slot of.*epoch$/ \1/"    |
        sed 'N;N;s/\n//g'                             |
        sed -r "s/([0-9a-f]{7})[0-9a-f]{57}/\1/g"
}
function legacy_log_header_messages() {
        local input="$1"
        fgrep -B2 -A3 ':     slot:' ${input}
}

function legacy_header_messages_to_internal_raw() {
        local genesis_block_hash="$1"
        grep 'hash:\|previous block:\|slot:\|signature:' |
        sed -e 's/^[^0-9]*\([0-9]*\)\.\(.*hash:.*\)$/:: \1\n\2/' |
        cut -d':' -f3-                                |
        sed "s/${genesis_block_hash}/${genesis_block_hash}\npub:genesis/;
             s/ \([0-9]*\).. slot of.*epoch$/ \1/;
             s/ BlockPSignatureHeavy: Proxy signature.* dPk = pub:\([0-9a-f]*\) .*$/ \1/
             "    |
        sed 'N;N;N;N;s/\n//g' |
        { read stamp hash prevhash slot delegate
          while test -n "${delegate}"
          do id=${leader_hex_id[${delegate}]}
             echo "${stamp} ${hash} ${prevhash} ${slot} ${delegate} ${id}"
             read stamp hash prevhash slot delegate; done; }
}

function legacy_merge_internal() {
        sed -r "s/([0-9a-f]{16})[0-9a-f]{48}/\1/g"    |
        sort --key 4 --numeric --unique | uniq -f1
}

function legacy_key_properties() {
        ${cardano_cli} signing-key-public --byron-legacy --secret "$1" |
          grep '^    public key hash: \|^public key (base64): \|   public key (hex): ' |
          cut -c22- |
          { read hash; read base64; read hex
            echo -n "${hash}|${base64}|${hex}"
          }
}

function legacy_extract_genesis_delegation() {
        grep 'GenesisDelegation (stakeholder ids)' $1 |
                cut -d'[' -f6 | cut -d']' -f1 |
                sed 's/, /\n/g; s/ -> / /g'
}

function smallest_file_size_bytes() {
        stat -c "%s" "$@" | sort -n | head -n1 | xargs echo -n
}

function cluster_log_ip_rewrite_pid() {
        local input="$1" ip="$2"
        grep -e "--host-addr ${ip}$" "${input}" | head -n1 | cut -d'[' -f3 |cut -d']' -f1
}

function cluster_log_ip_proxy_pid() {
        local input="$1" ip="$2"
        grep -e "Starting cardano-byron-proxy.*--local-addr \[${ip}\]" "${input}" | head -n1 | cut -d'[' -f3 |cut -d']' -f1
}

function cluster_log_ip_legacy_pid() {
        local input="$1" ip="$2"
        grep "cardano-node-legacy_-start.*startNode, we are NodeId ${ip}:" "${input}" | head -n1 | cut -d'[' -f3 | cut -d']' -f1
}

function internal_to_dot() {
        local limit="$1"
        head -n${limit}                             |
        arrow_loop
}

declare -A color
color['bg']="#002b36"
color['fg']="#586e75"
color['arrlabel']="#b58900"
color['drawing']="#586e75"

declare -A type_color
type_color['legacy']="#859900"
type_color['rewrite']="#268bd2"
type_color['proxy']="#b58900"

function arrow_loop() {
        read timestamp next prev slot leader_key node_id
        while test -n "${next}"
        do if test ! -v id_types[$node_id]
           then echo "ERR: id_types[$node_id] undefined" >&2; fi
           node_color=${type_color[${id_types[$node_id]}]}
           echo "\"${next}\" [color=\"${color[drawing]}\", fontcolor=\"${node_color}\", xlabel=\"Slot ${slot}\n@ ${timestamp}\"]"
           echo "\"${next}\" -> \"${prev}\" [label=\"${leader_key} Id ${node_id}\", color=\"${color[drawing]}\", fontcolor=\"${color[arrlabel]}\"]"
           read timestamp next prev slot leader_key node_id
        done
}

function slot_numbers_loop() {
        read timestamp ip hash slot
        printf "%-10s " "ip\slot"
        expected_slot=0
        while test -n "${timestamp}"
        do printf " %-6s" ${expected_slot}
           expected_slot=$((${expected_slot}+1))
           read timestamp ip hash slot
        done
        echo
}

function ip_slot_timings_loop() {
        read timestamp ip hash slot
        printf "%-10s " "${ip}"
        expected_slot=0
        while test -n "${timestamp}"
        do for i in $(seq $expected_slot $((${slot} - 1)))
           do printf " %-6s" "---"; done
           printf    " %-6s" ${timestamp}
           # do printf " %2s:%-6s" ${i}    "---"; done
           # printf    " %2s:%-6s" ${slot} ${timestamp}
           expected_slot=$((${slot}+1))
           read timestamp ip hash slot
        done
        echo
}

function leader_pub_nodeid() {
        echo ${delegate_hex_id[$1]}
}

function maybe_filter() {
        if test -n "$2"
        then grep "$2" "$1"
        else cat "$1"; fi
}

function error() {
        echo "$1" >&2
}
function fail() {
        error "ERROR:  $1"
        exit 1
}
