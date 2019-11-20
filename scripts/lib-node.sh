set -x

genesis_hash="1c1cf538d599170f4ed8a4cf8fcb3ef2e530e30a6de15e74b4088c002797972e"

genesis="$(echo ${genesis_hash} | cut -c-5 | xargs echo -n)"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi

function nodecfg () {
        printf -- "--config configuration/log-config-${1}.yaml "
}
function dlgkey () {
        printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key " "$1"
}
function dlgcert () {
        printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json " "$1"
}
function commonargs() {
        printf -- "--genesis-file ${genesis_file} "
        printf -- "--socket-dir ./socket/ "
}

function acceptorargs() {
        commonargs
        nodecfg acceptor
        dlgkey 0
        dlgcert 0
        printf -- "--port 1234 "
        printf -- "--topology configuration/simple-topology-0.json "
        printf -- "--database-path ./db/ "
}

function nodeargs () {
        local extra="$2"
        commonargs
        nodecfg $1
        dlgkey $1
        dlgcert $1
        printf -- "${extra} "
        printf -- "--database-path ./db/db-$1/ "
        printf -- "--port 300$1 "
        printf -- "--topology configuration/simple-topology-$1.json "
}
