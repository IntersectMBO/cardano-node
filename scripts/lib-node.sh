set -x

genesis_hash="c0c757817d86660accdc45b9d18c1274d51d6427b92995944d014e0ff056cb3e"

genesis="$(echo ${genesis_hash} | cut -c-5 | xargs echo -n)"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi

function nodecfg () {
        printf -- "--config-yaml configuration/log-config-${1}.yaml "
}
function dlgkey () {
        printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key " "$1"
}
function dlgcert () {
        printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json " "$1"
}
function commonargs() {
        printf -- "--topology configuration/simple-topology.json "
        printf -- "--database-path ./db/ "
        printf -- "--genesis-file ${genesis_file} "
        printf -- "--socket-dir ./socket/ "
}

function acceptorargs() {
        commonargs
        nodecfg acceptor
        dlgkey 0
        dlgcert 0
}

function nodeargs () {
        local extra="$2"
        commonargs
        nodecfg $1
        dlgkey $1
        dlgcert $1
        printf -- "${extra} "
}
