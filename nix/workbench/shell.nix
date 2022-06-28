{ pkgs
, lib
, haskellLib
, project
##
, setLocale, haveGlibcLocales, commandHelp
##
, cardano-mainnet-mirror
##
, profileName
, workbenchDevMode ? false
, useCabalRun ? false
##
, profiled ? false
, withHoogle ? true
, withMainnet ? true
}:

with lib;

let cluster = pkgs.supervisord-workbench-for-profile {
      inherit profileName useCabalRun profiled;
    };

    shellHook = { workbenchDevMode, useCabalRun, profiled, profileName, withMainnet }: ''
      while test $# -gt 0
      do shift; done       ## Flush argv[]

      echo 'workbench shellHook:  workbenchDevMode=${toString workbenchDevMode} useCabalRun=${toString useCabalRun} profiled=${toString profiled} profileName=${profileName}'
      export WB_BACKEND=supervisor
      export WB_SHELL_PROFILE=${profileName}

      ${optionalString
        workbenchDevMode
        ''
      export WB_CARDANO_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
      export WB_EXTRA_FLAGS=

      function wb() {
        $WB_CARDANO_NODE_REPO_ROOT/nix/workbench/wb $WB_EXTRA_FLAGS "$@"
      }
      export -f wb
        ''}

      ${optionalString
        useCabalRun
        ''
      . nix/workbench/lib.sh
      . nix/workbench/lib-cabal.sh ${optionalString profiled "--profiled"}
        ''}

      export CARDANO_NODE_SOCKET_PATH=run/current/node-0/node.socket

      function workbench_atexit() {
          if wb backend is-running
          then stop-cluster
          fi
      }
      trap workbench_atexit EXIT

      ${optionalString
        withMainnet
        ''
      export CARDANO_MAINNET_MIRROR=${cardano-mainnet-mirror.outputs.defaultPackage.x86_64-linux.outPath}
        ''}

      ${setLocale}
      ${commandHelp}
      '';

in project.shellFor {
  name = "workbench-shell";

  shellHook = shellHook { inherit workbenchDevMode useCabalRun profiled profileName withMainnet; };

  inherit withHoogle;

  packages = ps: builtins.attrValues (haskellLib.selectProjectPackages ps);

  tools = {
    haskell-language-server = {
      version = "latest";
      inherit (project) index-state;
    };
  };

  # These programs will be available inside the nix-shell.
  nativeBuildInputs = with pkgs; with haskellPackages; with cardanoNodePackages; [
    cardano-ping
    cabalWrapped
    db-analyser
    ghcid
    haskellBuildUtils
    pkgs.graphviz
    weeder
    nixWrapped
    pkgconfig
    profiteur
    profiterole
    python3Packages.supervisor
    ghc-prof-flamegraph
    sqlite-interactive
    tmux
    pkgs.git
    pkgs.hlint
    pkgs.moreutils
    pkgs.pstree
    pkgs.time
    cluster.interactive-start
    cluster.interactive-stop
    cluster.interactive-restart
  ] ++ lib.optional haveGlibcLocales pkgs.glibcLocales
  ## Workbench's main script is called directly in dev mode.
  ++ lib.optionals (!useCabalRun)
    [
      cardano-cli
      cardano-node
      cardano-topology
      cardano-tracer
      locli
      tx-generator
    ]
  ++ lib.optionals (!workbenchDevMode)
    [
      cluster.workbench.workbench
    ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;

} // { inherit shellHook;
     }
