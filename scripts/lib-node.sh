set -x
genesis="33873"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi
genesis_hash="$(cabal new-run -v0 -- cardano-cli --tracing-off --real-pbft print-genesis-hash --genesis-json ${genesis_file})"

function logcfg () {
        printf -- "--log-config configuration/log-config-${1}.yaml "
}
function dlgkey () {
        printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key " "$1"
}
function dlgcert () {
        printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json " "$1"
}
function commonargs() {
        printf -- "--slot-duration 2 "
}

function acceptorargs() {
        commonargs
        logcfg acceptor
        dlgkey 0
        dlgcert 0
}

function nodeargs () {
        local extra="$2"
        commonargs
        logcfg $1
        dlgkey $1
        dlgcert $1
        printf -- "--node-id $1 "
        printf -- "--port 300$1 "
        printf -- "--genesis-file ${genesis_file} "
        printf -- "--genesis-hash ${genesis_hash} "
        printf -- "--pbft-signature-threshold 0.7 "
        printf -- "--require-network-magic "
        printf -- "--database-path db "
        printf -- "--topology configuration/simple-topology.json "
        printf -- "${extra} "
}
