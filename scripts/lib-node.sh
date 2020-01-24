root="${1:-$(dirname $0)/..}"

. "${root}"/scripts/lib.sh "${root}"

genesis_root="${configuration}/genesis"
genesis_file="${genesis_root}/genesis.json"

if test ! -f "${genesis_file}"
then if test -x ${scripts}/genesis.sh
     then ${scripts}/genesis.sh
     else echo "ERROR: could not locate genesis generator!">&1; exit 1
     fi
fi

genesis_hash=`cat "${genesis_root}/GENHASH"`

if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi

function nodecfg () {
        printf -- "--config ${configuration}/log-config-${1}${2}.yaml "
}
function dlgkey () {
        printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key " "$1"
}
function dlgcert () {
        printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json " "$1"
}
function commonargs() {
        printf -- "--topology ${configuration}/simple-topology.json "
        printf -- "--database-path ${root}/db/ "
        printf -- "--genesis-file ${genesis_file} "
        printf -- "--genesis-hash ${genesis_hash} "
        printf -- "--socket-dir ${root}/socket/$1 "
}

function nodeargs () {
        local id="$1"
        local flavor="$2"
        local extra="$3"
        commonargs
        nodecfg $id $flavor
        dlgkey $id
        dlgcert $id
        printf -- "${extra} "
        printf -- "--port 300$1 "
}
