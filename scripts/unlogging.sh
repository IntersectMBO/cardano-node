function ip_input_output() {
        local ip="$1" input="$2"
        echo -n "${input}.${ip}.png"
}
function log_ip_pid () {
        local ip="$1" input="$2"
        local legacy_pid=$(cluster_log_ip_legacy_pid "${input}" "${ip}")
        local rewrite_pid=$(cluster_log_ip_rewrite_pid "${input}" "${ip}")
        local proxy_pid=$(cluster_log_ip_proxy_pid "${input}" "${ip}")
        if   test -n "${rewrite_pid}"
        then kind='rewrite'
             pid=${rewrite_pid}
             filter="cardano-node_-start\[${pid}\]: ";
        elif test -n "${legacy_pid}"
        then kind='legacy'
             pid=${legacy_pid}
             filter="cardano-node-legacy_-start\[${pid}\]: "
        elif test -n "${proxy_pid}"
        then kind='legacy'
             pid=${proxy_pid}
             filter="byron-proxy-start\[${pid}\]: ";
        else fail "no PID for node IP address '${ip}' in ${input:-<stdin>}"
        fi
        echo -n "${pid}|${kind}|${filter}"
}
function fill_ip_maps () {
        local input="$1" ipmask="$2" ipmin="$3" ipmax="$4"
        local i=
        for i in $(seq ${ipmin} ${ipmax})
        do fill_ip_maps_entry "${input}" $(printf $ipmask $i); done
}
declare -A ip_pids
declare -A ip_kinds
declare -A ip_filters
function fill_ip_maps_entry () {
        local input="$1" ip="$2"
        local out="$(log_ip_pid ${ip} ${input})"
        ip_pids["$ip"]="$(echo    "${out}" | cut -d\| -f1)"
        ip_kinds["$ip"]="$(echo   "${out}" | cut -d\| -f2)"
        ip_filters["$ip"]="$(echo "${out}" | cut -d\| -f3)"
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

function log_dot() {
        local input="$1" kind="$2" limit="$3" genesis_block_hash="$4"
        cat <<EOF
digraph forks {
  bgcolor="${bg_color}"
  color="${fg_color}"
  node [shape=record]
$(${kind}_log_header_messages "${input}" |
  ${kind}_header_messages_to_internal_raw ${genesis_block_hash} |
  ${kind}_merge_internal |
  internal_to_dot ${limit}
 )
}
EOF
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
        fgrep 'ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock' ${input}
}

function rewrite_header_messages_to_internal_raw() {
        local genesis_block_hash="$1"
        processGenesis='s/^[^0-9]*\([0-9]*\)\..*( ebb: true, hash: \(.*\), previousHash: \(.*\)).*/\1 \2 \3 -1 Genesis/'
        processNormal='s/^[^0-9]*\([0-9]*\)\..*(hash: \(.*\), previousHash: \(.*\), slot: \([0-9]*\), issuer: \(pub:[a-f0-9]*\), delegate: pub:[a-f0-9]*).*/\1 \2 \3 \4 \5/'
        sed "${processGenesis}; ${processNormal}"
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
        fgrep -B2 -A2 ':     slot:' ${input}
}

function legacy_header_messages_to_internal_raw() {
        local genesis_block_hash="$1"
        grep 'hash:\|previous block:\|slot:\|leader:' |
        sed -e 's/^[^0-9]*\([0-9]*\)\.\(.*hash:.*\)$/:: \1\n\2/' |
        cut -d':' -f3-                                |
        sed "s/${genesis_block_hash}/${genesis_block_hash}\npub:genesis/" |
        sed "s/ \([0-9]*\).. slot of.*epoch$/ \1/"    |
        sed 'N;N;N;N;s/\n//g'
}

function legacy_merge_internal() {
        sed -r "s/([0-9a-f]{16})[0-9a-f]{48}/\1/g"    |
        sort --key 4 --numeric --unique | uniq -f1
}

function cluster_log_ip_rewrite_pid() {
        local input="$1" ip="$2"
        grep "I am Node NodeAddress {naHostAddress = NodeHostAddress {getAddress = Just ${ip}}," "${input}" | head -n1 | cut -d'[' -f3 |cut -d']' -f1
}

function cluster_log_ip_proxy_pid() {
        local input="$1" ip="$2"
        fgrep -e "--local-addr [${ip}]" "${input}" | head -n1 | cut -d'[' -f3 |cut -d']' -f1
}

function cluster_log_ip_legacy_pid() {
        local input="$1" ip="$2"
        grep "startNode, we are NodeId ${ip}:" "${input}" | head -n1 | cut -d'[' -f3 | cut -d']' -f1
}

function internal_to_dot() {
        local limit="$1"
        head -n${limit}                             |
        arrow_loop
}

function nodeid_color() {
        case $1 in
                0 | 1 | 2 | 3 ) echo -n "#859900";;
                4 | 5 | 6 )     echo -n "#268bd2";;
                * )             echo -n "#b58900";; esac
}

bg_color="#002b36"
fg_color="#586e75"
arrlabel_color="#b58900";
drawing_color="#586e75";

function arrow_loop() {
        read timestamp next prev slot leader_pub
        while test -n "${next}"
        do nodeid=$(leader_pub_nodeid ${leader_pub})
           node_color=$(nodeid_color ${nodeid})
           echo "\"${next}\" [color=\"${drawing_color}\", fontcolor=\"${node_color}\", xlabel=\"Slot ${slot}\n@ ${timestamp}\"]"
           echo "\"${next}\" -> \"${prev}\" [label=\"${leader_pub} Id ${nodeid}\", color=\"${drawing_color}\", fontcolor=\"${arrlabel_color}\"]"
           read timestamp next prev slot leader_pub
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

function delegation_map() {
        # if test -d "${delegate_keydir}"
        # then i=0
        #      for x in "${delegate_keydir}"/key*.sk
        #      do echo $x
        #         i=$((i+1)); done; fi

        ## Hardcoded for 'mainnet_ci_full' for now.
        cat <<EOF
0 pub:4a309750
1 pub:938abf5e
2 pub:feb19082
3 pub:8647d806
4 pub:f5345d73
5 pub:9a5aaa3b
6 pub:627771ab
EOF
}

function leader_pub_nodeid() {
        delegation_map | grep "$1" | cut -d' ' -f1
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
