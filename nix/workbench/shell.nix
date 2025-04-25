{ pkgs
, lib
, haskellLib
, project
##
, setLocale, haveGlibcLocales
##
, workbench-runner
, workbenchDevMode ? false
##
, withHoogle ? true
}:

with lib;

project.shellFor {
  name = "workbench-shell";

  shellHook =
    let inherit (workbench-runner) backend profiling;
    in
    ''
    while test $# -gt 0
    do shift; done       ## Flush argv[]

    . nix/workbench/lib.sh

    ${workbench-runner.workbench-envars}

    progress "profile name"            $WB_SHELL_PROFILE
    progress "backend name"            $WB_BACKEND
    progress "deployment name"         $WB_DEPLOYMENT_NAME
    progress "params"                  'useCabalRun=${toString backend.useCabalRun} workbenchDevMode=${toString workbenchDevMode} profiling=${toString profiling}'
    progress "WB_SHELL_PROFILE_DATA="  $WB_SHELL_PROFILE_DATA
    progress "WB_BACKEND_DATA="        $WB_BACKEND_DATA
    progress "WB_LOCLI_DB="            $WB_LOCLI_DB
    progress "WB_CREATE_TESTNET_DATA=" $WB_CREATE_TESTNET_DATA
    progress "WB_MODULAR_GENESIS="     $WB_MODULAR_GENESIS

    function parse_git_branch() {
        git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/(\1)/p'
    }
    export PS1="\n\[\033[1;32m\][nix-shell:\w]\[\033[01;36m\]\$(parse_git_branch)\[\033[0m\]\$ "
    ''
    + optionalString workbenchDevMode
    ''
    export WB_CARDANO_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
    export WB_EXTRA_FLAGS=

    function wb() {
      $WB_CARDANO_NODE_REPO_ROOT/nix/workbench/wb $WB_EXTRA_FLAGS "$@"
    }
    export -f wb
    ''
    + optionalString backend.useCabalRun
    ''
    . nix/workbench/lib-cabal.sh ${optionalString (profiling != "none") "--profiling-${profiling}"}
    cabal update
    ''
    +
    ''
    function workbench_atexit() {
        if test -n "$(wb backend is-running run/current)"
        then stop-cluster
        fi
    }
    trap workbench_atexit EXIT
    ${setLocale}
    echo "
      Commands:
        * nix flake lock --update-input <iohkNix|haskellNix> - update nix build input
        * cardano-cli - used for key generation and other operations tasks
        * wb - cluster workbench
        * start-cluster - start a local development cluster
        * stop-cluster - stop a local development cluster
        * restart-cluster - restart the last cluster run (in 'run/current')
                            (WARNING: logs & node DB will be wiped clean)
    "
    ''
  ;

  inherit withHoogle;

  # The workbench shell uses cabalWrapped, which removes the `source-repository-package` stanzas
  # from `cabal.project`. The idea is to use prebuilt ones provided by haskell.nix. However,
  # haskell.nix is clever enough to not include `source-repository-package`s in the shell
  # package db, because it knows that cabal will rebuild them. So you just end up with nothing!
  # We can work around this by overriding haskell.nix's selection of which packages the shell
  # is prepared for, so that it *doesn't* include the `source-repository-package` ones
  # (the default is *local* packages which includes them, we select *project* pacakges which doesn't)
  packages = ps: builtins.attrValues (haskellLib.selectProjectPackages ps);

  tools = {
  };

  # These programs will be available inside the nix-shell.
  nativeBuildInputs = with pkgs; with haskellPackages; with cardanoNodePackages; [
    db-analyser
    cardano-cli.passthru.noGitRev
    pkgs.graphviz
    graphmod
    jq
    weeder
    nix
    (pkgs.pkg-config or pkgconfig)
    pkgs.profiteur
    profiterole
    ghc-prof-flamegraph
    sqlite-interactive
    tmux
    pkgs.cairo
    pkgs.dyff
    pkgs.git
    pkgs.hlint
    pkgs.moreutils
    pkgs.time
    pkgs.util-linux
    workbench-runner.workbench-interactive-start
    workbench-runner.workbench-interactive-stop
    workbench-runner.workbench-interactive-restart
  ]
  # Backend dependent packages take precedence.
  ++ workbench-runner.backend.extraShellPkgs
  ++ [
      # Publish
      bench-data-publish
      # Debugging
      postgresql
      # Performance report generation
      em
  ]
  ++ lib.optional haveGlibcLocales pkgs.glibcLocales
  ## Cabal run flag
  # Include the packages (defined in `lib-cabal.sh`) or the tools to build them.
  ++ lib.optionals ( workbench-runner.backend.useCabalRun) [
       cabal-install
       ghcid
       haskellBuildUtils
       pkgs.cabal-plan
     ]
  ++ lib.optionals (!workbench-runner.backend.useCabalRun) [
       cardano-node.passthru.noGitRev
       cardano-profile
       cardano-topology
       cardano-tracer
       locli
       locli-quick
       tx-generator
     ]
  # Include the workbench as a derivation or use the sources directly ?
  ++ lib.optionals (!workbenchDevMode) [ workbench.workbench ]
  ;
}
