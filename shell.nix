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
  shell = cardanoNodeHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    # If shellFor local packages selection is wrong,
    # then list all local packages then include source-repository-package that cabal complains about:
    packages = ps: with ps; [
       ps.cardano-node
       ps.cardano-config
       ps.cardano-cli
    ];

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

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      niv
      cardanoNodeHaskellPackages.cardano-cli.components.exes.cardano-cli
      cardanoNodeHaskellPackages.cardano-node.components.exes.cardano-node
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      source <(cardano-cli --bash-completion-script cardano-cli)
      source <(cardano-node --bash-completion-script cardano-node)

      ${lib.optionalString (__hasAttr "network" customConfig) ''
        export CARDANO_NODE_SOCKET_PATH=$(realpath ./state-node-${customConfig.network}/node.socket)

      ''}

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package
        * cardano-cli - used for key generation and other operations tasks

      "
    '';
  };

in

 shell // { inherit devops; }
