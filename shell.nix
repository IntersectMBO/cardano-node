let defaultCustomConfig = import ./nix/custom-config.nix defaultCustomConfig;
# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
in
{ # Compile with `--enable-profiling` or not.
  profiledBuild ? defaultCustomConfig.profiledBuild
  # For example "space-info+eventlog+info-table".
, profilingType ? defaultCustomConfig.profilingType
, withHoogle ? defaultCustomConfig.withHoogle
, profileName ? defaultCustomConfig.localCluster.profileName
, backendName ? defaultCustomConfig.localCluster.backendName
, useCabalRun ? true
, workbenchDevMode ? defaultCustomConfig.localCluster.workbenchDevMode
, workbenchStartArgs ? defaultCustomConfig.localCluster.workbenchStartArgs
, customConfig ? {
    inherit profiledBuild profilingType withHoogle;
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

  # An attrset containing the parsed profiling options.
  profiling =
    let
      # Split nix-shell "profilingType" argument in "PROFILING_MODE+MODIFIERS".
      profilingTypeParts = lib.strings.splitString "+" profilingType;
      # Get and check the first part of the "profilingType" nix-shell argument.
      profilingTypeParam =
        let
          # The available profiling modes and if a profiled runtime is required.
          options =
            {
              # Time and allocation profiling.
              ################################
              ################################

              ### `-p`: produces a standard time profile report.
              "time" = true;
              ### `-P`: produces a more detailed report containing the actual
              ### time and allocation data as well (not used much.).
              "time-detail" = true;

              # RTS options for heap profiling
              ################################
              ################################

              # There are several different kinds of heap profile that can be
              # generated. All the different profile types yield a graph of live
              # heap against time, but they differ in how the live heap is
              # broken down into bands. The following RTS options select which
              # break-down to use:

              ### `-hb` (Requires -prof):
              ### Breaks down the graph by biography.
              "space-bio" = true;
              ### `-hd` (Requires -prof):
              ### Breaks down the graph by closure description. For actual data,
              ### the description is just the constructor name, for other
              ### closures it is a compiler-generated string identifying the
              ### closure.
              "space-closure" = true;
              ### `-hc` (Requires -prof):
              ### Breaks down the graph by the cost-centre stack which produced
              ### the data.
              "space-cost" = true;
              ### `-hT` (This does not require the profiling runtime):
              ### Breaks down the graph by heap closure type.
              "space-heap" = false;
              ### `-hi` (This does not require the profiling runtime):
              ### Break down the graph by the address of the info table of a
              ### closure. For this to produce useful output the program must
              ### have been compiled with -finfo-table-map but it does not
              ### require the profiling runtime.
              "space-info" = false;
              ### `-hm` (Requires -prof).
              ### Break down the live heap by the module containing the code
              ### which produced the data.
              "space-module" = true;
              ### `-hr` (Requires -prof):
              ### Break down the graph by retainer set.
              "space-retainer" = true;
              ### `-hy` (Requires -prof):
              ### Breaks down the graph by type. For closures which have
              ### function type or unknown/polymorphic type, the string will
              ### represent an approximation to the actual type.
              "space-type" = true;
            }
          ;
          requestedPrefix =
            if (builtins.length profilingTypeParts > 0)
            then builtins.elemAt profilingTypeParts 0
            else "none" # For easier handling keep the default as "none".
          ;
        in
          if requestedPrefix == null || requestedPrefix == "" || requestedPrefix == "none"
          then "none" # For easier handling keep the default as "none".
          else
            # Check if the profiling option exists.
            if  !(__hasAttr requestedPrefix options)
            then throw "FATAL:  WB_PROFILING must be one of: none, ${toString (pkgs.lib.strings.intersperse "," (__attrNames options))}"
            else
              # Check is the profiling options needs a profiled runtime.
              let needsProfiledBuild = options."${requestedPrefix}";
              in if needsProfiledBuild && !profiledBuild
                 then throw "FATAL: profiling type \"${requestedPrefix}\" needs a profiled runtime"
                 else requestedPrefix
      ;
      # Get and check the modifiers of the "profilingType" nix-shell argument.
      profilingTypeModifiers =
        let
          # The available modifiers.
          options = [ "eventlog" "info-table" ];
          requestedModifiers =
            if (builtins.length profilingTypeParts >= 2)
            then builtins.tail profilingTypeParts
            else [] # For easier handling keep the default as an empty list.
          ;
        in
          if builtins.all (m: builtins.elem m options) requestedModifiers
          then requestedModifiers
          else throw "FATAL: The WB_PROFILING modifiers (PROFILING_MODE+MODIFIER) must be of: none, ${toString (pkgs.lib.strings.intersperse "," options)}"
      ;
    in
      { inherit profiledBuild profilingType;
        inherit profilingTypeParam;
        eventlog =  builtins.elem "eventlog" profilingTypeModifiers;
        infoTable = builtins.elem "info-table" profilingTypeModifiers;
      }
  ;

  # Based on the profiling mode requested get the needed haskell.nix project.
  project =
    let
      module = {pkgs, lib, ...}:
        if profiling.infoTable
        then
          { modules = [
              { # For all reinstalled packages.
                # Same as `cabal.project`:
                ### package *
                ###   ghc-options: -finfo-table-map -fdistinct-constructor-tables
                ghcOptions = [ "-finfo-table-map" "-fdistinct-constructor-tables" ];
              }
            ];
          }
        else {}
      ;
    in
      # Even when using `useCabalRun` we want to enter a shell were all the
      # dependencies are built with the requested profiling/info-table
      # combination. If not Cabal will re-build all dependencies with profiling
      # or only add "info-table" to the executable/target.
      # Note: `base` won't include "info-table" unless you build GHC with it.
      if profiledBuild
      then cardanoNodeProject.profiled.appendModule( module )
      else cardanoNodeProject.appendModule( module )
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
            { inherit profiling;
              inherit profileName backendName useCabalRun;
            };
        };

  devops =
    let profileName = "devops-bage";
        workbench-runner = pkgs.workbench-runner
          { inherit profiling;
            inherit profileName;
            backendName = "supervisor";
            useCabalRun = false;
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
