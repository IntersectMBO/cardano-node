# our packages overlay
pkgs: _: with pkgs;
  let
    compiler = config.haskellNix.compiler or "ghc865";
  in {
  cardanoNodeHaskellPackages = import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
  };
  cardanoNodeProfiledHaskellPackages = import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    profiling = true;
  };
  cardanoNodeAssertedHaskellPackages = import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    assertedPackages = [
      "ouroboros-consensus"
      "ouroboros-consensus-cardano"
      "ouroboros-consensus-byron"
      "ouroboros-consensus-shelley"
      "ouroboros-network"
      "network-mux"
    ];
  };

  #Grab the executable component of our package.
  inherit (cardanoNodeHaskellPackages.cardano-node.components.exes) cardano-node chairman;
  inherit (cardanoNodeHaskellPackages.cardano-cli.components.exes) cardano-cli;
  inherit (cardanoNodeHaskellPackages.bech32.components.exes) bech32;
  cardano-node-profiled = cardanoNodeProfiledHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-asserted = cardanoNodeAssertedHaskellPackages.cardano-node.components.exes.cardano-node;

  # expose the db-converter from the ouroboros-network we depend on
  inherit (cardanoNodeHaskellPackages.ouroboros-consensus-byron.components.exes) db-converter;

  mkCluster = callPackage ./supervisord-cluster;
  hfcCluster = callPackage ./supervisord-cluster/hfc {};
  cardanolib-py = callPackage ./cardanolib-py {};

  inherit ((haskell-nix.hackage-package {
    name = "hlint";
    version = "3.1.6";
    compiler-nix-name = compiler;
    inherit (cardanoNodeHaskellPackages) index-state;
  }).components.exes) hlint;
}
