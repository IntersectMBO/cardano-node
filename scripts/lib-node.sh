genesis_root="$(realpath configuration/genesis)"
genesis_file="${genesis_root}/genesis.json"

usage() {
        cat <<EOF
Usage:

  $(basename $0) [ --cabal | --stack | --stack-nix ] *

EOF
}

default_mode='cabal'
mode='default'
while test -n "$1"
do case "$1" in
           --cabal )              mode='cabal';;
           --stack )              mode='stack';;
           --stack-nix )          mode='stack-nix';;
           --help | "--"* ) usage; exit 1;;
           * ) break;; esac; shift; done
if   test "$mode" != 'default'
then true
elif test -d 'dist-newstyle'
then mode='cabal'
elif test -d '.stack-work'
then mode='stack'
else mode=$default_mode
fi

executable_runner() {
        case ${mode} in
                cabal )     echo "cabal v2-run -- exe:$1";;
                stack )     echo "stack exec -- $1";;
                stack-nix ) echo "stack exec --nix $1 --";;
                * ) echo "INTERNAL ERROR: unknown mode:  $mode" >&2; exit 1;;
        esac
}

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

function nodeargs () {
        local extra="$2"
        commonargs
        nodecfg $1
        dlgkey $1
        dlgcert $1
        printf -- "${extra} "
        printf -- "--port 300$1 "
}
