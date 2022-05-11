let defaultCustomConfig = import ./nix/custom-config.nix defaultCustomConfig;
# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
in
{ withHoogle ? defaultCustomConfig.withHoogle
, profileName ? defaultCustomConfig.localCluster.profileName
, autoStartCluster ? defaultCustomConfig.localCluster.autoStartCluster
, autoStartClusterArgs ? defaultCustomConfig.localCluster.autoStartClusterArgs
, workbenchDevMode ? defaultCustomConfig.localCluster.workbenchDevMode
, customConfig ? {
    inherit withHoogle;
    localCluster =  {
      inherit autoStartCluster autoStartClusterArgs profileName workbenchDevMode;
    };
  }
, pkgs ? import ./nix customConfig
}:
with pkgs;
let
  inherit (pkgs) customConfig;
  inherit (customConfig) withHoogle localCluster;
  inherit (localCluster) autoStartCluster autoStartClusterArgs profileName workbenchDevMode;
  inherit (pkgs.haskell-nix) haskellLib;
  commandHelp =
    ''
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
    '';
  # Test cases will assume a UTF-8 locale and provide text in this character encoding.
  # So force the character encoding to UTF-8 and provide locale data.
  setLocale =
    ''
      export LANG="en_US.UTF-8"
    '' + lib.optionalString haveGlibcLocales ''
      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
    '';

  haveGlibcLocales = pkgs.glibcLocales != null && stdenv.hostPlatform.libc == "glibc";

  shell =
    let cluster = pkgs.supervisord-workbench-for-profile
      { inherit profileName;
        useCabalRun = true;
      };
    in cardanoNodeProject.shellFor {
    name = "cluster-shell";

    inherit withHoogle;

    packages = ps: builtins.attrValues (haskellLib.selectProjectPackages ps);

    tools = {
      haskell-language-server = {
        version = "latest";
        inherit (cardanoNodeProject) index-state;
      };
    };

    # These programs will be available inside the nix-shell.
    nativeBuildInputs = with haskellPackages; with cardanoNodePackages; [
      cardano-ping
      cabalWrapped
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
      cluster.interactive-start
      cluster.interactive-stop
      cluster.interactive-restart
    ] ++ lib.optional haveGlibcLocales pkgs.glibcLocales
    ## Workbench's main script is called directly in dev mode.
    ++ lib.optionals (!workbenchDevMode)
    [
      cluster.workbench.workbench
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    shellHook = ''
      echo 'nix-shell top-level shellHook:  withHoogle=${toString withHoogle} profileName=${profileName} autoStartCluster=${toString autoStartCluster} workbenchDevMode=${toString workbenchDevMode}'

      ${with customConfig.localCluster;
        (import ./nix/workbench/shell.nix { inherit lib workbenchDevMode profileName; useCabalRun = true; }).shellHook}

      ${lib.optionalString autoStartCluster ''
      function atexit() {
          if wb backend is-running
          then echo "workbench:  stopping cluster (because 'autoStartCluster' implies this):"
               stop-cluster
          fi
      }
      trap atexit EXIT
      ''}

      ${setLocale}

      ${lib.optionalString autoStartCluster ''
      echo "workbench:  starting cluster (because 'autoStartCluster' is true):"
      start-cluster ${autoStartClusterArgs}
      ''}

      ${commandHelp}

      set +e
    '';
  };

  devops =
    let cluster = pkgs.supervisord-workbench-for-profile
      { profileName = "devops-alzo";
        useCabalRun = false;
      };
    in cardanoNodeProject.shellFor {
    name = "devops-shell";

    packages = _: [];

    nativeBuildInputs = with cardanoNodePackages; [
      nixWrapped
      cardano-cli
      bech32
      cardano-ping
      cardano-node
      pkgs.graphviz
      python3Packages.supervisor
      python3Packages.ipython
      cluster.interactive-start
      cluster.interactive-stop
      cluster.interactive-restart
      cardanolib-py
      cluster.workbench.workbench
      pstree
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      ${with customConfig.localCluster;
        (import ./nix/workbench/shell.nix { inherit lib workbenchDevMode profileName; useCabalRun = false; }).shellHook}

      # Socket path default to first node launched by "start-cluster":
      export CARDANO_NODE_SOCKET_PATH=$(wb backend get-node-socket-path ${cluster.stateDir})

      ${setLocale}

      # Unless using specific network:
      ${lib.optionalString (__hasAttr "network" customConfig) ''
        export CARDANO_NODE_SOCKET_PATH="$PWD/state-node-${customConfig.network}/node.socket"
        ${lib.optionalString (__hasAttr "utxo" pkgs.commonLib.cardanoLib.environments.${customConfig.network}) ''
          # Selfnode and other test clusters have public secret keys that we pull from iohk-nix
          echo "To access funds use UTXO_SKEY and UTXO_VKEY environment variables"
          export UTXO_SKEY="${pkgs.commonLib.cardanoLib.environments.${customConfig.network}.utxo.signing}"
          export UTXO_VKEY="${pkgs.commonLib.cardanoLib.environments.${customConfig.network}.utxo.verification}"
        ''}

      ''}

      echo "NOTE: you may need to use a github access token if you hit rate limits with nix flake update:"
      echo '      edit ~/.config/nix/nix.conf and add line `access-tokens = "github.com=23ac...b289"`'
      ${commandHelp}

      ${lib.optionalString autoStartCluster ''
      echo "workbench:  starting cluster (because 'autoStartCluster' is true):"
      start-cluster
      ''}
    '';
  };

  dev = cardanoNodeProject.shell;

in

 shell // { inherit devops; inherit dev;}
