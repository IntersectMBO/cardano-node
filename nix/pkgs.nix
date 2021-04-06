# our packages overlay
pkgs: _: with pkgs;
  let
    compiler = config.haskellNix.compiler or "ghc8104";
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
  cardanoNodeEventlogHaskellPackages = import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    eventlog = true;
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
  inherit (cardanoNodeHaskellPackages.cardano-node.components.exes) cardano-node;
  inherit (cardanoNodeHaskellPackages.cardano-cli.components.exes) cardano-cli;
  inherit (cardanoNodeHaskellPackages.cardano-topology.components.exes) cardano-topology;
  inherit (cardanoNodeHaskellPackages.bech32.components.exes) bech32;
  inherit (cardanoNodeHaskellPackages.cardano-submit-api.components.exes) cardano-submit-api;
  cardano-node-profiled = cardanoNodeProfiledHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-eventlogged = cardanoNodeEventlogHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-asserted = cardanoNodeAssertedHaskellPackages.cardano-node.components.exes.cardano-node;

  # expose the db-converter and cardano-ping from the ouroboros-network we depend on
  inherit (cardanoNodeHaskellPackages.ouroboros-consensus-byron.components.exes) db-converter;
  inherit (cardanoNodeHaskellPackages.network-mux.components.exes) cardano-ping;

  mkCluster = cfg: callPackage ./supervisord-cluster cfg;
  cardanolib-py = callPackage ./cardanolib-py {};

  clusterTests = import ./supervisord-cluster/tests { inherit pkgs; };

  inherit ((haskell-nix.hackage-package {
    name = "hlint";
    version = "3.1.6";
    compiler-nix-name = compiler;
    inherit (cardanoNodeHaskellPackages) index-state;
  }).components.exes) hlint;
}
