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
    let inherit (workbench-runner) profiling useCabalRun;
    in
    ''
    while test $# -gt 0
    do shift; done       ## Flush argv[]

    . nix/workbench/lib.sh

    ${workbench-runner.workbench-envars}

    progress "profile name"            $WB_SHELL_PROFILE
    progress "backend name"            $WB_BACKEND
    progress "profiling"               'profiledBuild=${if profiling.profiledBuild or false then "yes" else "no"} profilingType=${profiling.profilingType or ""}'
    progress "params"                  'useCabalRun=${toString useCabalRun} workbenchDevMode=${toString workbenchDevMode}'
    progress "deployment name"         $WB_DEPLOYMENT_NAME
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
    + optionalString useCabalRun
    ''
    . nix/workbench/lib-cabal.sh
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
  nativeBuildInputs =
     (with pkgs; [
       cairo
       dyff
       git
       graphviz
       hlint
       jq
       moreutils
       nix
       (pkgs.pkg-config or pkgconfig)
       profiteur
       sqlite-interactive
       time
       tmux
       util-linux
     ])
  ++ (with pkgs.haskellPackages; [
       ghc-prof-flamegraph
       graphmod
       profiterole
       weeder
     ])
  # Packages in need of a newer versions compared to flake's nixpkgs.
  # Pinning "nixos-25.11" to avoid cache misses when entering the shell.
  # To update use `curl -L https://channels.nixos.org/nixos-25.11/git-revision`
  ++ (with (builtins.getFlake "github:NixOS/nixpkgs/999ca0e5484922624254294ea1adc2b90081579e").legacyPackages.${pkgs.system}; [
       # Will be removed once nixpkgs is bumped to a suitable version.
       typst
     ])
  ++
  ## Cabal run flag:
  # Include the packages or the tools to build them (see `lib-cabal.sh`).
  (if !workbench-runner.useCabalRun
   then
     (with project.exes; [
       # A `notGitRev` version, faster to enter a workbench after a new commit.
       cardano-node
       cardano-profile
       cardano-topology
       cardano-tracer
       locli
       # A `notGitRev` version, faster to enter a workbench after a new commit.
       tx-generator
     ])
   else
     (with pkgs; [
       pkgs.cabal-install
       pkgs.ghcid
       pkgs.haskellBuildUtils
       pkgs.cabal-plan
     ])
  )
  ++ (with project.hsPkgs; [
      # A `notGitRev` version, faster to enter a workbench after a new commit.
      cardano-cli.components.exes.cardano-cli
      ouroboros-consensus-cardano.components.exes.db-analyser
     ])
  ++ (with workbench-runner; [
       workbench-interactive-start
       workbench-interactive-stop
       workbench-interactive-restart
     ])
  # Backend dependent packages take precedence.
  ++ workbench-runner.extraShellPkgs
  ++ [
      # Publish
      pkgs.bench-data-publish
      # Debugging
      pkgs.postgresql
  ]
  ++ lib.optional haveGlibcLocales pkgs.glibcLocales
  ## Include useful profiling helper programs.
  ++ [
       # For the legacy prog.hp format.
       # Which has been deprecated in favour of eventlog based profiling.
       pkgs.haskellPackages.hp2pretty
       pkgs.haskellPackages.hp2html
       pkgs.haskellPackages.eventlog2html
     ]
  # Include the workbench as a derivation or use the sources directly ?
  ++ lib.optionals (!workbenchDevMode) [ workbench.workbench ]
  ;
}
