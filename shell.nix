# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, autoStartCluster ? false
, sourcesOverride ? {}
, withHoogle ? true
, clusterProfile ? "default-mary"
, customConfig ? { profileName = clusterProfile; }
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:
with pkgs;
let
  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # NOTE: due to some cabal limitation,
  #  you have to remove all `source-repository-package` entries from cabal.project
  #  after entering nix-shell for cabal to use nix provided dependencies for them.
  clusterCabal = mkCluster (lib.recursiveUpdate customConfig { useCabalRun = true; });
  clusterNix   = mkCluster (lib.recursiveUpdate customConfig { useCabalRun = false; });
  shell = cardanoNodeHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    inherit withHoogle;

    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabal-install
      cardano-ping
      ghcid
      hlint
      weeder
      nix
      niv
      pkgconfig
      profiteur
      profiterole
      python3Packages.supervisor
      ghc-prof-flamegraph
      sqlite-interactive
      tmux
      pkgs.git
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
      echo "Setting 'cabal.project' for local builds.."
      ./scripts/cabal-inside-nix-shell.sh

      function atexit() {
          echo "Reverting 'cabal.project' to the index version.."
          ./scripts/cabal-inside-nix-shell.sh --restore
      }
      trap atexit EXIT

      ${lib.optionalString autoStartCluster ''
      echo "Starting cluster (because 'auto-start-cluster' is true):"
      start-cluster
      ''}
    '';
  };

  devops =
    stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      cabal-install
      niv
      cardano-cli
      bech32
      cardano-node
      python3Packages.supervisor
      python3Packages.ipython
      clusterNix.start
      clusterNix.stop
      cardanolib-py
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      source <(cardano-cli --bash-completion-script cardano-cli)
      source <(cardano-node --bash-completion-script cardano-node)

      # Socket path default to first BFT node launched by "start-cluster":
      export CARDANO_NODE_SOCKET_PATH=$PWD/${clusterNix.stateDir}/node-0.socket
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

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package
        * cardano-cli - used for key generation and other operations tasks
        * start-cluster - start a local development cluster
        * stop-cluster - stop a local development cluster

      "

      ${lib.optionalString autoStartCluster ''
      echo "Starting cluster (because 'auto-start-cluster' is true):"
      start-cluster
      ''}
    '';
  };

in

 shell // { inherit devops; }
