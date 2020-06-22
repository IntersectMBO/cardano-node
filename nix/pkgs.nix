# our packages overlay
pkgs: _: with pkgs; {
  cardanoNodeHaskellPackages = import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
  };
  cardanoNodeProfiledHaskellPackages = import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    profiling = true;
  };

  #Grab the executable component of our package.
  inherit (cardanoNodeHaskellPackages.cardano-node.components.exes) cardano-node chairman;
  inherit (cardanoNodeHaskellPackages.cardano-cli.components.exes) cardano-cli;
  cardano-node-profiled = cardanoNodeProfiledHaskellPackages.cardano-node.components.exes.cardano-node;

  # expose the db-converter from the ouroboros-network we depend on
  inherit (cardanoNodeHaskellPackages.ouroboros-consensus-byron.components.exes) db-converter;

  mkCluster = callPackage ./supervisord-cluster;
}
