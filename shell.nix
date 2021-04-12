# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? true
, clusterProfile ? "default-mary"
, customConfig ? import ./custom-config.nix // { profileName = clusterProfile; }

, autoStartCluster ? false
, useCabalRun      ? false
, workbenchDevMode ? false
, workbenchConfig ? import ./workbench-config.nix //
  { inherit useCabalRun workbenchDevMode; }
, pkgs ? import ./nix {
    inherit config sourcesOverride customConfig workbenchConfig;
  }
}:
with pkgs;
let
  commandHelp =
    ''
      echo "
        Commands:
          * nix flake update --update-input <iohkNix|haskellNix> - update imput
          * cardano-cli - used for key generation and other operations tasks
          * wb - cluster workbench
          * start-cluster - start a local development cluster
          * stop-cluster - stop a local development cluster

      "
    '';

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # NOTE: due to some cabal limitation,
  #  you have to remove all `source-repository-package` entries from cabal.project
  #  after entering nix-shell for cabal to use nix provided dependencies for them.
  clusterCabal = mkCluster (lib.recursiveUpdate customConfig (workbenchConfig // { useCabalRun = true; }));
  clusterNix   = mkCluster (lib.recursiveUpdate customConfig (workbenchConfig // { useCabalRun = false; }));
  nixWapped = writeShellScriptBin "nix" ''
    if [[ "$@" == "flake show"* ]] || [[ "$@" == "flake check"* ]]; then
      echo 'Temporary override `supported-systems.nix` original content to be able to use `nix flake show|check` on dev machines (workaround for https://github.com/NixOS/nix/issues/4265)'
      SYSTEMS="$(${git}/bin/git rev-parse --show-toplevel)/supported-systems.nix"
      BACKUP="$(mktemp)"
      mv "$SYSTEMS" "$BACKUP"
      echo '[ "${system}" ]' > "$SYSTEMS"
      function atexit() {
          mv "$BACKUP" "$SYSTEMS"
      }
      trap atexit EXIT
    fi
    GC_DONT_GC=1 ${nixFlakes}/bin/nix "$@"
  '';
  cabalWapped = writeShellScriptBin "cabal" ''
    # Temporary modify `cabal.project` for local builds..
    PROJECT="$(${git}/bin/git rev-parse --show-toplevel)/cabal.project"
    BACKUP="$(mktemp)"
    cp -a "$PROJECT" "$BACKUP"
    sed -ni '1,/--- 8< ---/ p' $PROJECT
    function atexit() {
        mv "$BACKUP" "$PROJECT"
    }
    trap atexit EXIT
    ${cabal}/bin/cabal "$@"
  '';

  shell = cardanoNodeProject.shellFor {
    name = "cabal-dev-shell";

    inherit withHoogle;

    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    tools = {
      hlint = {
        version = "latest";
        inherit (cardanoNodeProject) index-state;
      };
      haskell-language-server = {
        version = "latest";
        inherit (cardanoNodeProject) index-state;
      };
    };

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cardano-ping
      cabalWapped
      ghcid
      weeder
      nixWapped
      pkgconfig
      profiteur
      profiterole
      python3Packages.supervisor
      ghc-prof-flamegraph
      sqlite-interactive
      tmux
      pkgs.git
    ]
    ## Workbench's main script is called directly in dev mode.
    ++ lib.optionals (!workbenchDevMode)
    [
      pkgs.workbench.workbench
    ]
    ## Local cluster not available on Darwin,
    ## because psmisc fails to build on Big Sur.
    ++ lib.optionals (!stdenv.isDarwin)
    [
      clusterCabal.start
      clusterCabal.stop
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    shellHook = ''
      function atexit() {
          ${lib.optionalString autoStartCluster ''
          if wb local supervisord-running
          then echo "workbench:  stopping cluster (because 'autoStartCluster' implies this):"
               stop-cluster
          fi''}
      }
      trap atexit EXIT
      ${lib.optionalString (autoStartCluster && useCabalRun) ''
      unset NIX_ENFORCE_PURITY
      ''}

      ${pkgs.workbench.shellHook}

      ${lib.optionalString autoStartCluster ''
      echo "workbench:  starting cluster (because 'autoStartCluster' is true):"
      start-cluster
      ''}

      ${commandHelp}

      set +e
    '';
  };

  devops =
    stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      nixWapped
      cardano-cli
      bech32
      cardano-node
      python3Packages.supervisor
      python3Packages.ipython
      clusterNix.start
      clusterNix.stop
      cardanolib-py
      pkgs.workbench.workbench
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      wb explain-mode

      source <(cardano-cli --bash-completion-script cardano-cli)
      source <(cardano-node --bash-completion-script cardano-node)

      # Socket path default to first node launched by "start-cluster":
      export CARDANO_NODE_SOCKET_PATH=$(wb local get-node-socket-path ${clusterNix.stateDir})

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

in

 shell // { inherit devops; }
