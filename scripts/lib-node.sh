. $(dirname $0)/lib.sh

OUTPUTDIR=${1?scripts.lib-node.Error: no genesis directory specified}

genesis_root="${configuration}/${OUTPUTDIR}/genesis"
configuration_root="${configuration}/${OUTPUTDIR}"
genesis_file="${genesis_root}/genesis.json"

if [ -a "$genesis_file" ];
then :
elif test -x ${scripts}/genesis.sh
then ${scripts}/genesis.sh "${genesis_root}"
else echo "ERROR: could not locate genesis generator!">&1; exit 1
fi


genesis_hash=`cat "${genesis_root}/GENHASH"`

if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi

function nodecfg () {
        printf -- "--config ${configuration_root}/config-${1}.yaml "
}
function dlgkey () {
        printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key " "$1"
}
function dlgcert () {
        printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json " "$1"
}
function commonargs() {
        printf -- "--topology ${configuration_root}/topology-node-$1.json "
        printf -- "--database-path ${root}/db/db-$1 "
        printf -- "--socket-path ${root}/socket/$1 "
}

function nodeargs () {
        local id="$1"
        local extra="$3"
        commonargs $id
        nodecfg $id
        dlgkey $id
        dlgcert $id
        printf -- "${extra} "
        printf -- "--port 300$1 "
}
