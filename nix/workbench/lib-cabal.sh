progress "workbench"  "cabal-inside-nix-shell mode enabled, calling cardano-* via '$(white cabal run)' (instead of using Nix store); $(red lib-cabal.sh) flags: $(red WB_PROFILEDBUILD):$(white ${WB_PROFILEDBUILD:-no}) $(red WB_PROFILINGINFOTABLE):$(white ${WB_PROFILINGINFOTABLE:-no})"

# Check if we build with profiling.
if test -n "${WB_PROFILEDBUILD:-}" && test "${WB_PROFILEDBUILD}" = 'yes'
then
  # These flags apply only to the executable target.
  # The dependencies should already be included with profiling in `shell.nix`.
  export WB_FLAGS_CABAL='--enable-profiling --builddir dist-profiled'
else
  export WB_FLAGS_CABAL=""
fi
# Check if we build with info-table.
if test -n "${WB_PROFILINGINFOTABLE:-}" && test "${WB_PROFILINGINFOTABLE}" = 'yes'
then
  # These flags apply only to the executable target.
  # The options should already be applied to the dependencies in `shell.nix`.
  export WB_FLAGS_CABAL="${WB_FLAGS_CABAL} --ghc-options=\"-finfo-table-map\" --ghc-options=\"-fdistinct-constructor-tables\""
fi

# If RTS args envar not empty, append the extra RTS parameters to `cabal run`.
if test -z "${WB_RTSARGS:-}"
then
  export WB_FLAGS_RTS=
else
  export WB_FLAGS_RTS="+RTS ${WB_RTSARGS} -RTS"
fi

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
    # Executables with profiling support.
    for exe in cardano-node cardano-tracer tx-generator locli
    do echo "workbench:  $(blue prebuilding) $(red $exe)"
        verbose "exec"                         "cabal build ${WB_FLAGS_CABAL} -- exe:$exe"
        cabal $(test -z "${verbose:-}" && echo '-v0') build ${WB_FLAGS_CABAL} -- exe:$exe || return 1
    done
    # Executables without profiling support.
    for exe in cardano-profile cardano-topology
    do echo "workbench:  $(blue prebuilding) $(red $exe)"
       verbose "exec"                         "cabal build                    -- exe:$exe"
       cabal $(test -z "${verbose:-}" && echo '-v0') build                    -- exe:$exe || return 1
    done
    echo
    eval $restore_trace
}

function cardano-node() {
    ${WB_NODE_EXECPREFIX} cabal -v0 run ${WB_FLAGS_CABAL} exe:cardano-node   -- ${WB_FLAGS_RTS} "$@"
}

function cardano-tracer() {
                          cabal -v0 run ${WB_FLAGS_CABAL} exe:cardano-tracer -- ${WB_FLAGS_RTS} "$@"
}

function locli() {
                          cabal -v0 run ${WB_FLAGS_CABAL} exe:locli          -- ${WB_FLAGS_RTS} "$@"
}

function tx-generator() {
                          cabal -v0 run ${WB_FLAGS_CABAL} exe:tx-generator   -- ${WB_FLAGS_RTS} "$@"
}

function cardano-profile() {
                          cabal -v0 run                   exe:cardano-profile                   "$@"
}

function cardano-topology() {
                          cabal -v0 run                   exe:cardano-topology                  "$@"
}

export WB_MODE_CABAL=t

export -f cardano-node cardano-tracer locli tx-generator cardano-profile cardano-topology
