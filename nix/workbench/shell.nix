{ pkgs
, lib
, haskellLib
, project
##
, setLocale, haveGlibcLocales, commandHelp
##
, cardano-mainnet-mirror
##
, workbench-runner
, workbenchDevMode ? false
##
, withHoogle ? true
, withMainnet ? true
}:

with lib;

let

    # recover CHaP location from cardano's project
    chap = project.args.inputMap."https://chap.intersectmbo.org/";

    # build plan as computed by nix
    nixPlanJson = project.plan-nix.json;

in project.shellFor {
  name = "workbench-shell";

  shellHook =
    let inherit (workbench-runner) profileName profileData backend backendData profiling;
    in
    ''
    while test $# -gt 0
    do shift; done       ## Flush argv[]

    . nix/workbench/lib.sh

    export WB_BACKEND=${backend.name}
    export WB_BACKEND_DATA=${backendData}
    export WB_CREATE_TESTNET_DATA=''${WB_CREATE_TESTNET_DATA:-0}
    export WB_DEPLOYMENT_NAME=''${WB_DEPLOYMENT_NAME:-$(basename $(pwd))}
    export WB_MODULAR_GENESIS=''${WB_MODULAR_GENESIS:-0}
    export WB_SHELL_PROFILE=${profileName}
    export WB_SHELL_PROFILE_DATA=${profileData}

    progress "profile name"            $WB_SHELL_PROFILE
    progress "backend name"            $WB_BACKEND
    progress "deployment name"         $WB_DEPLOYMENT_NAME
    progress "params"                  'useCabalRun=${toString backend.useCabalRun} workbenchDevMode=${toString workbenchDevMode} profiling=${toString profiling}'
    progress "WB_BACKEND_DATA="        $WB_BACKEND_DATA
    progress "WB_CREATE_TESTNET_DATA=" $WB_CREATE_TESTNET_DATA
    progress "WB_MODULAR_GENESIS="     $WB_MODULAR_GENESIS
    progress "WB_SHELL_PROFILE_DATA="  $WB_SHELL_PROFILE_DATA

    function parse_git_branch() {
        git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/(\1)/p'
    }
    export PS1="\n\[\033[1;32m\][nix-shell:\w]\[\033[01;36m\]\$(parse_git_branch)\[\033[0m\]\$ "
    ''
    + optionalString workbenchDevMode
    ''
    export WB_CARDANO_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
    export WB_CHAP_PATH=${chap}
    export WB_NIX_PLAN=${nixPlanJson}
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
    export CARDANO_NODE_SOCKET_PATH=run/current/node-0/node.socket

    function workbench_atexit() {
        if test -n "$(wb backend is-running run/current)"
        then stop-cluster
        fi
    }
    trap workbench_atexit EXIT
    ''
    + optionalString withMainnet
    ''
    export CARDANO_MAINNET_MIRROR=${cardano-mainnet-mirror.outputs.defaultPackage.x86_64-linux.outPath}
    ''
    + ''
    ${setLocale}
    ${commandHelp}
    '';

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
    pkgs.pstree
    pkgs.time
    workbench-interactive-start
    workbench-interactive-stop
    workbench-interactive-restart
  ]
  # Backend packages take precendence.
  ++ workbench-runner.backend.extraShellPkgs
  ++ [
      # Publish
      bench-data-publish
      # Publish tunnel
      yq nomad vault-bin norouter socat
      # Debugging
      postgresql
      # Performance report generation
      em
  ]
  ++ lib.optional haveGlibcLocales pkgs.glibcLocales
  ++ lib.optionals (!workbench-runner.backend.useCabalRun) [ cardano-profile cardano-topology cardano-cli locli ]
  ++ lib.optionals (!workbenchDevMode) [ workbench.workbench ]
  ;
}
