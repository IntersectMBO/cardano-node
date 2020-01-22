usage() {
        cat <<EOF
Usage:

  $(basename $0) [ --cabal | --stack | --stack-nix ] *

EOF
}

root="${1:-$(realpath $(dirname $0)/..)}"
configuration="${root}/configuration"
scripts="${root}/scripts"

default_mode='cabal'
mode='default'
while test -n "$1"
do case "$1" in
           --cabal )              mode='cabal';;
           --stack )              mode='stack';;
           --stack-nix )          mode='stack-nix';;
           --help ) usage; exit 1;;
           * ) break;; esac; shift; done
if   test "$mode" != 'default'
then true
elif test -d "${root}/"'dist-newstyle'
then mode='cabal'
elif test -d "${root}/"'.stack-work'
then mode='stack'
else mode=$default_mode
fi

executable_runner() {
        case ${mode} in
                cabal )     echo "cabal v2-run exe:$1 --";;
                stack )     echo "stack run $1 --";;
                stack-nix ) echo "stack run --nix $1 --";;
                * ) echo "INTERNAL ERROR: unknown mode:  $mode" >&2; exit 1;;
        esac
}

executable_quiet_runner() {
        case ${mode} in
                cabal )     echo "cabal v2-run -v0 exe:$1 --";;
                stack )     echo "stack --silent run $1 --";;
                stack-nix ) echo "stack --silent run --nix $1 --";;
                * ) echo "INTERNAL ERROR: unknown mode:  $mode" >&2; exit 1;;
        esac
}
