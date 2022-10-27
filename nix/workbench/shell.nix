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

let cluster = pkgs.nomad-workbench-for-profile {
      inherit profileName useCabalRun profiled;
    };

    useCabalRun = false;

    inherit (cluster) backend profile;

    shellHook = { workbenchDevMode, useCabalRun, profiled, profileName, withMainnet }: ''
      while test $# -gt 0
      do shift; done       ## Flush argv[]

      echo 'workbench shellHook:  workbenchDevMode=${toString workbenchDevMode} useCabalRun=${toString useCabalRun} profiled=${toString profiled} profileName=${profileName}'
      export WB_BACKEND=${backend.name}
      export WB_SHELL_PROFILE=${profileName}
      export WB_SHELL_PROFILE_DIR=${profile}

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

  # The workbench shell uses cabalWrapped, which removes the `source-repository-package` stanzas
  # from `cabal.project`. The idea is to use prebuilt ones provided by haskell.nix. However,
  # haskell.nix is clever enough to not include `source-repository-package`s in the shell
  # package db, because it knows that cabal will rebuild them. So you just end up with nothing!
  # We can work around this by overriding haskell.nix's selection of which packages the shell
  # is prepared for, so that it *doesn't* include the `source-repository-package` ones
  # (the default is *local* packages which includes them, we select *project* pacakges which doesn't)
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
    db-analyser
    pkgs.graphviz
    graphmod
    weeder
    nixWrapped
    pkgconfig
    profiteur
    profiterole
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
  ++ lib.optionals (!useCabalRun) [cardano-topology cardano-cli locli]
  ++ backend.extraShellPkgs
  ++ lib.optionals (!workbenchDevMode)
    [
      cluster.workbench.workbench
    ]
    ;

} // { inherit shellHook;
     }
