progress "workbench"  "cabal-inside-nix-shell mode enabled, calling cardano-* via '$(white cabal run)' (instead of using Nix store); $(red lib-cabal.sh) flags: $(yellow $*)"

while test $# -gt 0
do case "$1" in
       --profiling-time )     export WB_PROFILING='time';           WB_RTSARGS=-p;;
       --profiling-space )    export WB_PROFILING='space-cost';     WB_RTSARGS=-hc;;
       --profiling-heap )     export WB_PROFILING='space-heap';     WB_RTSARGS=-hT;;
       --profiling-module )   export WB_PROFILING='space-module';   WB_RTSARGS=-hm;;
       --profiling-retainer ) export WB_PROFILING='space-retainer'; WB_RTSARGS=-hr;;
       --profiling-type )     export WB_PROFILING='space-type';     WB_RTSARGS=-hy;;
       * ) break;; esac;
   progress "workbench" "enabling $(red profiling mode):  $(white $WB_PROFILING)"
   shift; done

if test ! -v WB_PROFILING || test "$WB_PROFILING" = 'none'
then export WB_PROFILING='none' WB_FLAGS_CABAL=
else export WB_FLAGS_CABAL='--enable-profiling --builddir dist-profiled'; fi

if test ! -v WB_RTSARGS;   then export WB_RTSARGS= ; fi
export WB_FLAGS_RTS=${WB_RTSARGS:++RTS $WB_RTSARGS -RTS}

WB_TIME=(
    time
    -f "{ \"wall_clock_s\":       %e\n, \"user_cpu_s\":         %U\n, \"sys_cpu_s\":          %S\n, \"avg_cpu_pct\":       \"%P\"\n, \"rss_peak_kb\":        %M\n, \"signals_received\":   %k\n, \"ctxsw_involuntary\":  %c\n, \"ctxsw_volunt_waits\": %w\n, \"pageflt_major\":      %F\n, \"pageflt_minor\":      %R\n, \"swaps\":              %W\n, \"io_fs_reads\":        %I\n, \"io_fs_writes\":       %O\n, \"cmdline\":           \"%C\"\n, \"exit_code\":          %x }"
    -o kernel-resource-summary.json
)
export WB_NODE_EXECPREFIX="eval ${WB_TIME[*]@Q}"


function workbench-prebuild-executables()
{
    eval $muffle_trace_set_exit
    msg "prebuilding executables (because of useCabalRun)"
    git diff --exit-code --quiet && echo -n ' ' || echo -n "$(yellow local changes +) "
    git --no-pager log -n1 --alternate-refs --pretty=format:"%Cred%cr %Cblue%h %Cgreen%D %Cblue%s%Creset" --color
    newline
    newline

    unset NIX_ENFORCE_PURITY
    for exe in cardano-node cardano-topology cardano-tracer tx-generator locli
    do echo "workbench:  $(blue prebuilding) $(red $exe)"
       verbose "exec"                         "cabal build ${WB_FLAGS_CABAL} -- exe:$exe"
       cabal $(test -z "${verbose:-}" && echo '-v0') build ${WB_FLAGS_CABAL} -- exe:$exe || return 1
    done
    echo
    eval $restore_trace
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
        $(cabal list-bin locli) || true
                          cabal -v0 exec  ${WB_FLAGS_CABAL}     locli            -- ${WB_FLAGS_RTS} "$@"
}

function tx-generator() {
                          cabal -v0 run   ${WB_FLAGS_CABAL} exe:tx-generator     -- ${WB_FLAGS_RTS} "$@"
}

export WB_MODE_CABAL=t

export -f cardano-node cardano-topology cardano-tracer locli tx-generator
