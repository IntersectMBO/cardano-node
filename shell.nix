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
}:
with pkgs;
let
  inherit (pkgs) customConfig;
  inherit (customConfig) withHoogle localCluster;
  inherit (localCluster) profileName workbenchDevMode workbenchStartArgs;
  inherit (pkgs.haskell-nix) haskellLib;

  profilingEff =
    if    profiling == "none"
       # Time and allocation profiling.
       ################################
       ### `-p`: produces a standard time profile report.
       || profiling == "time"
       ### `-P`: produces a more detailed report containing the actual time and
       ### allocation data as well (not used much.).
       || profiling == "time-detail"
       # RTS options for heap profiling
       ################################
       # There are several different kinds of heap profile that can be
       # generated. All the different profile types yield a graph of live heap
       # against time, but they differ in how the live heap is broken down into
       # bands. The following RTS options select which break-down to use:
       ### `-hb` (Requires -prof):
       ### Breaks down the graph by biography.
       || profiling == "space-bio"
       ### `-hd` (Requires -prof):
       ### Breaks down the graph by closure description. For actual data, the
       ### description is just the constructor name, for other closures it is a
       ### compiler-generated string identifying the closure.
       || profiling == "space-closure"
       ### `-hc` (Requires -prof):
       ### Breaks down the graph by the cost-centre stack which produced the
       ### data.
       || profiling == "space-cost"
       ### `-hT` (This does not require the profiling runtime):
       ### Breaks down the graph by heap closure type.
       || profiling == "space-heap"
       ### `-hi` (This does not require the profiling runtime):
       ### Break down the graph by the address of the info table of a closure.
       ### For this to produce useful output the program must have been compiled
       ### with -finfo-table-map but it does not require the profiling runtime.
       || profiling == "space-info"
       ### `-hm` (Requires -prof).
       ### Break down the live heap by the module containing the code which
       ### produced the data.
       || profiling == "space-module"
       ### `-hr` (Requires -prof):
       ### Break down the graph by retainer set.
       || profiling == "space-retainer"
       ### `-hy` (Requires -prof):
       ### Breaks down the graph by type. For closures which have function type
       ### or unknown/polymorphic type, the string will represent an
       ### approximation to the actual type.
       || profiling == "space-type"
    then profiling
    else throw "FATAL:  WB_PROFILING must be one of: none, time, time-detail, space-bio, space-closure, space-cost, space-heap, space-info, space-module, space-retainer, space-type";

  project = if profilingEff != "none"
            then cardanoNodeProject.profiled
            else cardanoNodeProject
  ;

  ## The default shell is defined by flake.nix: (cardanoNodeProject = flake.project.${final.system})
  inherit (project) shell;

  ## XXX: remove this once people retrain their muscle memory:
  dev = project.shell;

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
          inherit setLocale haveGlibcLocales;
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
              inherit setLocale haveGlibcLocales;
              inherit workbench-runner workbenchDevMode;
              inherit withHoogle;
            };
    in project.shellFor {
    name = "devops-shell";

    packages = _: [];

    nativeBuildInputs = with cardanoNodePackages; [
      alejandra
      nix
      cardano-cli
      bech32
      cardano-node
      cardano-profile
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
      pkgs.util-linux
      workbench.workbench
      git
      graphviz
      jq
      moreutils
      procps
      workbench-runner.workbench-interactive-start
      workbench-runner.workbench-interactive-stop
      workbench-runner.workbench-interactive-restart
    ];

    # Disable build tools for all of hsPkgs (would include duplicates for cardano-cli, cardano-node, etc.)
    allToolDeps = false;

    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      ${devopsShell.shellHook}

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

    '';
  };

in

 shell // { inherit workbench-shell; inherit devops; inherit dev; }
