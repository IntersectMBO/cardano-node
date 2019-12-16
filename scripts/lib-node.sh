genesis_root="$(realpath configuration/genesis)"
genesis_file="${genesis_root}/genesis.json"

if test ! -f "${genesis_file}"
then if test -x ./scripts/genesis.sh
     then ./scripts/genesis.sh
     else echo "ERROR: could not locate genesis generator!">&1; exit 1
     fi
fi

genesis_hash=`cat "${genesis_root}/GENHASH"`

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
        printf -- "--topology configuration/simple-topology.json "
        printf -- "--database-path ./db/ "
        printf -- "--genesis-file ${genesis_file} "
        printf -- "--genesis-hash ${genesis_hash} "
        printf -- "--socket-dir ./socket/$1 "
}

function acceptorargs() {
        commonargs
        nodecfg acceptor
        dlgkey 0
        dlgcert 0
        printf -- "--port 1234 "
}

function nodeargs () {
        local extra="$2"
        commonargs
        nodecfg $1
        dlgkey $1
        dlgcert $1
        printf -- "${extra} "
        printf -- "--port 300$1 "
}
