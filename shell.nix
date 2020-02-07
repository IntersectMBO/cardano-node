# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? false
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
       # in theory we should only have the above two packages (or better, they should be auto-detected),
       # but due to source-repository-package declarations being considered as local packages by cabal, we need the following packages as well.
       # cf. https://github.com/haskell/cabal/issues/6249 and https://github.com/haskell/cabal/issues/5444
       ps.cardano-sl-x509
       ps.ekg-prometheus-adapter
       ps.ouroboros-consensus
       ps.ouroboros-network
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
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };

in

 shell // { inherit devops; }
