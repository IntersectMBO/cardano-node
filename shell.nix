let defaultCustomConfig = import ./nix/custom-config.nix defaultCustomConfig;
# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
in
{ withHoogle ? defaultCustomConfig.withHoogle
, profileName ? defaultCustomConfig.localCluster.profileName
, backendName ? defaultCustomConfig.localCluster.backendName
, useCabalRun ? true
, workbenchDevMode ? defaultCustomConfig.localCluster.workbenchDevMode
, workbenchStartArgs ? defaultCustomConfig.localCluster.workbenchStartArgs
# to use profiled build of haskell dependencies:
, profiling ? "none"
, customConfig ? {
    inherit profiling withHoogle;
    localCluster =  {
      inherit profileName backendName useCabalRun workbenchDevMode workbenchStartArgs;
    };
  }
, pkgs ? import ./nix customConfig
, cardano-mainnet-mirror ? __getFlake "github:input-output-hk/cardano-mainnet-mirror/nix"
}:
with pkgs;
let
  inherit (pkgs) customConfig;
  inherit (customConfig) withHoogle localCluster;
  inherit (localCluster) profileName workbenchDevMode workbenchStartArgs;
  inherit (pkgs.haskell-nix) haskellLib;

  profilingEff =
    if    profiling == "none"
       || profiling == "time"
       || profiling == "time-detail"
       || profiling == "space-cost"
       || profiling == "space-heap"
       || profiling == "space-module"
       || profiling == "space-retainer"
       || profiling == "space-type"
    then profiling
    else throw "FATAL:  WB_PROFILING must be one of:  none, time, time-detail, space-cost, space-heap, space-module, space-retainer, space-type";
  project = if profilingEff != "none" then cardanoNodeProject.profiled else cardanoNodeProject;

  ## The default shell is defined by flake.nix: (cardanoNodeProject = flake.project.${final.system})
  inherit (project) shell;

  ## XXX: remove this once people retrain their muscle memory:
  dev = project.shell;

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

  workbench-shell =
    with customConfig.localCluster;
      import ./nix/workbench/shell.nix
        { inherit pkgs lib haskellLib project;
          inherit setLocale haveGlibcLocales commandHelp;
          inherit cardano-mainnet-mirror;
          inherit workbenchDevMode;
          inherit withHoogle;
          workbench-runner = pkgs.workbench-runner
            { inherit profileName backendName useCabalRun;
              profiling = profilingEff;
            };
        };

  devops =
    let profileName = "devops-bage";
        workbench-runner = pkgs.workbench-runner
          {
            inherit profileName;
            backendName = "supervisor";
            useCabalRun = false;
            profiling = profilingEff;
          };
        devopsShell = with customConfig.localCluster;
          import ./nix/workbench/shell.nix
            { inherit pkgs lib haskellLib project;
              inherit setLocale haveGlibcLocales commandHelp;
              inherit cardano-mainnet-mirror;
              inherit workbench-runner workbenchDevMode;
              inherit withHoogle;
              withMainnet = false;
            };
    in project.shellFor {
    name = "devops-shell";

    packages = _: [];

    nativeBuildInputs = with cardanoNodePackages; [
      nix
      cardano-cli
      bech32
      cardano-node
      cardano-topology
      cardano-tracer
      locli
      tx-generator
      pkgs.graphviz
      python3Packages.supervisor
      python3Packages.ipython
      cardanolib-py
      pstree
      pkgs.time
      workbench.workbench
      workbench-interactive-start
      workbench-interactive-stop
      workbench-interactive-restart
    ];

    # Disable build tools for all of hsPkgs (would include duplicates for cardano-cli, cardano-node, etc.)
    allToolDeps = false;

    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      ${devopsShell.shellHook}

      # Socket path default to first node launched by "start-cluster":
      export CARDANO_NODE_SOCKET_PATH=$(wb backend get-node-socket-path ${workbench-runner.stateDir} 'node-0')

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

    '';
  };

in

 shell // { inherit workbench-shell; inherit devops; inherit dev; }
