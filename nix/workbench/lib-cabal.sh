echo 'workbench:  cabal-inside-nix-shell mode enabled, calling cardano-* via 'cabal run' (instead of using Nix store)' >&2

function workbench-prebuild-executables()
{
    local local_changes=

    git diff --exit-code --quiet && echo -n ' ' || echo -n '[31mlocal changes + '
    git --no-pager log -n1 --alternate-refs --pretty=format:"%Cred%cr %Cblue%h %Cgreen%D %Cblue%s%Creset" --color
    echo

    echo -n "workbench:  prebuilding executables (because of useCabalRun): "
    unset NIX_ENFORCE_PURITY
    for exe in cardano-cli cardano-node cardano-topology
    do echo -n "$exe "
       cabal -v0 build -- exe:$exe 2>&1 >/dev/null |
           { grep -v 'Temporary modify'; true; } || return 1
    done
    echo
}

function cardano-cli() {
    cabal -v0 run exe:cardano-cli -- "$@"
}

function cardano-node() {
    cabal -v0 run exe:cardano-node -- "$@"
}

function cardano-topology() {
    cabal -v0 run exe:cardano-topology -- "$@"
}

function locli() {
    cabal -v0 build exe:locli
    set-git-rev \
        $(git rev-parse HEAD) \
        $(find ./dist-newstyle/build/ -type f -name locli)
    cabal -v0 exec      locli -- "$@"
}

function tx-generator() {
    cabal -v0 run exe:tx-generator -- "$@"
}

export WORKBENCH_CABAL_MODE=t

export -f cardano-cli cardano-node cardano-topology locli tx-generator

workbench-prebuild-executables
