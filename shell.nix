# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, customConfig ? {}
, sourcesOverride ? {}
, withHoogle ? true
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
  shell = cardanoNodeHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabal-install
      ghcid
      hlint
      weeder
      nix
      niv
      pkgconfig
      profiteur
      profiterole
      ghc-prof-flamegraph
      sqlite-interactive
      tmux
      git
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;
  };

  devops = let
    cluster = mkCluster customConfig;
    inherit hfcCluster;
  in
    stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      niv
      cardano-cli
      bech32
      cardano-node
      python3Packages.supervisor
      python3Packages.ipython
      cluster.start
      cluster.stop
      hfcCluster.start
      hfcCluster.stop
      cardanolib-py
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      source <(cardano-cli --bash-completion-script cardano-cli)
      source <(cardano-node --bash-completion-script cardano-node)

      # Socket path default to first BFT node launched by "start-cluster":
      export CARDANO_NODE_SOCKET_PATH=$PWD/${cluster.baseEnvConfig.stateDir}/bft1.socket
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
        * start-cluster-hfc - start a local development cluster for testing hfc
        * stop-cluster-hfc - stop a local development cluster for testing hfc

      "
    '';
  };

in

 shell // { inherit devops; }
