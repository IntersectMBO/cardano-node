progress "workbench"  "cabal-inside-nix-shell mode enabled, calling cardano-* via '$(white cabal run)' (instead of using Nix store); $(red lib-cabal.sh) flags: $(yellow $*)"

if test ! -v WB_PROFILED; then export WB_PROFILED= ; fi

while test $# -gt 0
do case "$1" in
       --profiled ) progress "workbench" "enabling $(white profiled) mode"
                    export WB_PROFILED='true';;
       * ) break;; esac; shift; done

export WB_RTSARGS=${WB_PROFILED:+-xc}
export WB_FLAGS_RTS=${WB_RTSARGS:++RTS $WB_RTSARGS -RTS}
export WB_FLAGS_CABAL=${WB_PROFILED:+--enable-profiling}
WB_TIME=(
    time
    -f "{ \"wall_clock_s\":       %e\n, \"user_cpu_s\":         %U\n, \"sys_cpu_s\":          %S\n, \"avg_cpu_pct\":       \"%P\"\n, \"rss_peak_kb\":        %M\n, \"signals_received\":   %k\n, \"ctxsw_involuntary\":  %c\n, \"ctxsw_volunt_waits\": %w\n, \"pageflt_major\":      %F\n, \"pageflt_minor\":      %R\n, \"swaps\":              %W\n, \"io_fs_reads\":        %I\n, \"io_fs_writes\":       %O\n, \"cmdline\":           \"%C\"\n, \"exit_code\":          %x }"
    -o kernel-resource-summary.json
)
export WB_NODE_EXECPREFIX="eval ${WB_TIME[*]@Q}"


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
       cabal -v0 build ${WB_FLAGS_CABAL} -- exe:$exe 2>&1 >/dev/null |
           { grep -v 'exprType TYPE'; true; } || return 1
    done
    echo
}

function cardano-cli() {
                          cabal -v0 run   ${WB_FLAGS_CABAL} exe:cardano-cli      -- ${WB_FLAGS_RTS} "$@"
}

function cardano-node() {
    ${WB_NODE_EXECPREFIX} cabal -v0 run   ${WB_FLAGS_CABAL} exe:cardano-node     -- ${WB_FLAGS_RTS} "$@"
}

function cardano-topology() {
                          cabal -v0 run   ${WB_FLAGS_CABAL} exe:cardano-topology -- ${WB_FLAGS_RTS} "$@"
}

function cardano-tracer() {
                          cabal -v0 run   ${WB_FLAGS_CABAL} exe:cardano-tracer   -- ${WB_FLAGS_RTS} "$@"
}

function locli() {
    cabal -v0 build ${WB_FLAGS_CABAL} exe:locli
    set-git-rev \
        $(git rev-parse HEAD) \
        $(find ./dist-newstyle/build/ -type f -name locli) || true
                          cabal -v0 exec  ${WB_FLAGS_CABAL}     locli            -- ${WB_FLAGS_RTS} "$@"
}

function tx-generator() {
                          cabal -v0 run   ${WB_FLAGS_CABAL} exe:tx-generator     -- ${WB_FLAGS_RTS} "$@"
}

export WB_MODE_CABAL=t

export -f cardano-cli cardano-node cardano-topology cardano-tracer locli tx-generator
