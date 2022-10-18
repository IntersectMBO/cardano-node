progress "workbench"  "cabal-inside-nix-shell mode enabled, calling cardano-* via '$(white cabal run)' (instead of using Nix store) $*"

export WORKBENCH_PROFILED=

while test $# -gt 0
do case "$1" in
       --profiled ) progress "workbench" "enabling $(white profiled) mode"
                    export WORKBENCH_PROFILED='true';;
       * ) break;; esac; shift; done

export WBRTSARGS=${WORKBENCH_PROFILED:+-xc}
export WBFLAGS_RTS=${WBRTSARGS:++RTS $WBRTSARGS -RTS}
export WBFLAGS_CABAL=${WORKBENCH_PROFILED:+--enable-profiling}

function workbench-prebuild-executables()
{
    local local_changes=

    git diff --exit-code --quiet && echo -n ' ' || echo -n '[31mlocal changes + '
    git --no-pager log -n1 --alternate-refs --pretty=format:"%Cred%cr %Cblue%h %Cgreen%D %Cblue%s%Creset" --color
    echo

    echo "workbench:  prebuilding executables (because of useCabalRun)"
    unset NIX_ENFORCE_PURITY
    for exe in cardano-node cardano-cli cardano-topology cardano-tracer tx-generator locli
    do echo "workbench:    $(blue prebuilding) $(red $exe)"
       cabal -v0 build ${WBFLAGS_CABAL} -- exe:$exe 2>&1 >/dev/null |
           { grep -v 'exprType TYPE'; true; } || return 1
    done
    echo
}

function cardano-cli() {
    cabal -v0 run exe:cardano-cli ${WBFLAGS_RTS} -- "$@"
}

function cardano-node() {
    cabal -v0 run exe:cardano-node ${WBFLAGS_RTS} -- "$@"
}

function cardano-topology() {
    cabal -v0 run exe:cardano-topology ${WBFLAGS_RTS} -- "$@"
}

function cardano-tracer() {
    cabal -v0 run exe:cardano-tracer ${WBFLAGS_RTS} -- "$@"
}

function locli() {
    cabal -v0 build ${WBFLAGS_CABAL} exe:locli
    set-git-rev \
        $(git rev-parse HEAD) \
        $(find ./dist-newstyle/build/ -type f -name locli) || true
    cabal -v0 exec  ${WBFLAGS_CABAL} locli -- ${WBFLAGS_RTS} "$@"
}

function tx-generator() {
    cabal -v0 run exe:tx-generator ${WBFLAGS_RTS} -- "$@"
}

export WORKBENCH_CABAL_MODE=t

export -f cardano-cli cardano-node cardano-topology cardano-tracer locli tx-generator
