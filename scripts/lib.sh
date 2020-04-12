#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2034,SC2154,SC2039,SC1007,SC2207,SC2145,SC2155,SC2206
## Don't import this file directly,
## unless you provide it the path to 'scripts/' as first argument.

#set -e

. "$1"/lib-nix.sh

##
## This depends on the setup done by scripts/common.sh
##

vprint() {
        if test -n "${verbose}${debug}"; then echo "-- $*" >&2; fi
}
export -f vprint

vprint_top() {
        ## This only prints if either in debug mode,
        ## or ran from the top-level shell process.
        if test -z "${lib_recursing}" -o -n "${debug}"; then vprint "$@"; fi
}
export -f vprint_top

dprint() {
        if test -n "${debug}"; then echo "-- $*" >&2; fi
}
export -f dprint

oprint() {
        echo "-- $*" >&2
}
export -f oprint

prebuild() {
        vprint "prebuilding the \"$1\" executable in \"${mode}\" mode.."
        run "$1" --help >/dev/null || true
}
export -f prebuild

run() {
        if test -n "${verbose}"
        then run_verbose "$@"
        else run_quiet   "$@"
        fi
}
export -f run

run_verbose() {
        actually_run "$@"
}
export -f run_verbose

run_quiet()
{
        ## This nightmare below (minus the last line) does one simple thing:
        ##   ..it ensures the --build-extra argument has the mode-specific argument
        ##   that muffles output from the mode-appropriate build tool.
        local bld_extra=
        while test -n "$1"
        do case "$1" in
           --build-extra )    bld_extra=$2; shift;;
           * ) break;; esac; shift; done
        local new_bld_extra=$(
                case ${mode} in
                        nix )               echo -n '--no-build-output --quiet';;
                        cabal )             echo -n '-v0';;
                        stack | stack-nix ) echo -n '--silent';; esac;
                echo -n " ${bld_extra}";)

        actually_run --build-extra "${new_bld_extra}" "$@"
}
export -f run_quiet

actually_run()
{
        local bld_extra=
        local profile= profile_suffix= profile_file=
        local rtsargs=
        while test -n "$1"
        do case "$1" in
           --profile )           profile=t;;
           --profile-suffix )    profile_suffix=.$2; shift;;
           --build-extra )       bld_extra="$2"; shift;;
           * ) break;; esac; shift; done

        if test -n "${profile}"
        then profile_file="$(generate_mnemonic)${profile_suffix}"
             rtsargs='+RTS -p -s -po'${profile_file}' -RTS'
        else rtsargs=''; fi
        dprint "RTS args: ${rtsargs}"

        local exe="$1"; shift

        case ${mode} in
        nix )       X=(run_nix_executable "$exe" "${bld_extra}" ${rtsargs} "$@");;
        cabal )     X=(cabal v2-run ${bld_extra} exe:$exe --    ${rtsargs} "$@");;
        stack )     X=(stack ${bld_extra} run $exe        --    ${rtsargs} "$@");;
        stack-nix ) X=(stack ${bld_extra} run --nix $exe  --    ${rtsargs} "$@");;
        * ) echo "INTERNAL ERROR: unknown mode:  $mode" >&2; return 1;;
        esac

        vprint "${X[@]}"
        "${X[@]}"
        local status=$?

        if test -n "${profile_file}" -a -x "$(command -v profiteur 2>/dev/null)"
        then profiteur "${profile_file}.prof"
        fi

        return ${status}
}
export -f actually_run

##
## Misc stuff, too few to be worth splitting out
##
generate_mnemonic()
{
        local mnemonic="$(nix-shell -p diceware --run 'diceware --no-caps --num 2 --wordlist en_eff -d-')"
        local timestamp="$(date +%s)"
        local commit="$(git rev-parse HEAD | cut -c-16)"
        local status=''

        if git diff --quiet --exit-code
        then status=pristine
        else status=modified
        fi

        echo "${timestamp}.${commit}.${status}.${mnemonic}"
}
export -f generate_mnemonic
