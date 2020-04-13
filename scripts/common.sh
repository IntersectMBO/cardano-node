#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154,SC2155,SC2034,SC2039,SC2240,SC2124
## Don't ask, the story is too sad -- function env sharing & tmux involved.
set -o allexport

. "$(dirname "$0")"/lib.sh "$(dirname "$0")"

usage() {
        cat <<EOF
Usage:

  $(basename "$0") COMMON-FLAGS.. APP-FLAGS.. APP-ARGS..

  Common flags:

    Method to run executables:
    --nix               nix-build default.nix -A haskellPackages.blah....
                          This is the default mode, unless -- read on:
    --cabal             cabal v2-run exe:...
                          Default _iff_ ./dist-newstyle exists.
    --stack             stack run ...
                          Default _iff_ ./.stack-work exists.
    --stack-nix         stack run --nix ...

    --cls               Clear the TTY before anything happens..

    --profile           Enable library & executable profiling, with
                         .prof and profiteur's .html output.
                        The Nix case works end-to-end, while cabal&stack
                        need to be manually set up to provide correspondingly
                        capable binaries before this works.
    --mnemonic-suffix SUFFIX
                        Profiling output will get an additional suffix

    --quiet             Don't print much.  The default
    --verbose           Be verbose about what's going on
    --debug             Be even more verbose
    --trace             Trace every shell statement

    --help              Print this common help
    --app-help          Print application help (if available)

EOF
}

## Part of common init, see below.
setup_recursion_and_verbosity() {
        lib_recursing=${lib_running}
        export lib_running=t

        if test -z "${verbose}" -a -n "${DEFAULT_VERBOSE}"
        then verbose=t
        fi
        if test -z "${debug}" -a -n "${DEFAULT_DEBUG}"
        then debug=t
        fi
        export DEFAULT_VERBOSE=${verbose}
        export DEFAULT_DEBUG=${debug}
}

## Part of common init, see below.
setup_executables() {
        ## This:
        ##   1. decides the mode for executable invocation
        ##   2. performs mode-specific initialisation

        if   test "$mode" != 'default'
        then true
        elif test -n "${SCRIPTS_LIB_SH_RECURSE_MODE}"
        then mode=${SCRIPTS_LIB_SH_RECURSE_MODE}
             dprint "inheriting ${SCRIPTS_LIB_SH_RECURSE_MODE} mode in recursed call"
        elif test -d "${root}/"'dist-newstyle'
        then mode='cabal'
        elif test -d "${root}/"'.stack-work'
        then mode='stack'
        else mode=$default_mode
        fi
        export SCRIPTS_LIB_SH_RECURSE_MODE=${mode}
        vprint_top "executable mode: ${mode}"

        if test -n "${profile}"
        then vprint_top "cardano-node profiling enabled"
             export COMMON_NODE_PROFILING=t
        else export COMMON_NODE_PROFILING=
        fi

        case ${mode} in
                nix ) setup_nix;;
        esac
}

setup_nix() {
        if test -z "${profile}"
        then defaultnix_config=''
        else defaultnix_config='{haskellNix={profiling=true;};}'
        fi
        dprint "default.nix config: ${defaultnix_config}"

        defaultnix_args="--argstr gitrev $(git rev-parse HEAD)"
        if test -n "${defaultnix_config}"
        then defaultnix_args+=" --arg config ${defaultnix_config}"
        fi
}

###
### Common main
###

##   1. set up global state
##   2. handle common CLI args, and yield upon seeing
##      anything we have no idea about, thereby allowing for
##      downstream, case-specific parsing of remaining args.
##   3. resolve verbosity and recursion issues

self=$(basename "$0")
export root="$(realpath "$(dirname "$0")"/..)"
export configuration="${root}/configuration"
export scripts="${root}/scripts"

export default_mode='nix'
export mode='default'
export profile=
export verbose=
export debug=
export mnemonic_suffix=${mnemonic_suffix:-}
export force_genesis=

while test -n "$1"
do case "$1" in
           --nix )                mode='nix';;
           --cabal )              mode='cabal';;
           --stack )              mode='stack';;
           --stack-nix )          mode='stack-nix';;

           --cls )                echo -en "\ec";;

           --profile )            profile=t;;
           --mnemonic-suffix )    mnemonic_suffix=$2; shift;;

           --force-genesis )      force_genesis=t;;

           --quiet )              verbose=;;
           --verbose )            verbose=t;;
           --debug )              debug=t; verbose=t;;
           --trace )              debug=t; verbose=t; set -x;;

           --help )               usage; exit 1;;
           * ) break;; esac; shift; done

setup_recursion_and_verbosity
setup_executables

vprint_top "process group id (PGID): $$"
vprint_top "to list the process tree:  pstree -Tulap $$"
vprint_top "..or, interactively:       watch --interval=1 \"pstree -Tulap $$\""
