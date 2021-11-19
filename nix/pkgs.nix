# our packages overlay
final: prev: with final;
  let
    compiler-nix-name = config.haskellNix.compiler or "ghc8107";
  in {
  cardanoNodeProject = import ./haskell.nix {
    inherit compiler-nix-name
      lib
      haskell-nix
      buildPackages
      gitrev
      ;
  };
  cardanoNodeHaskellPackages = cardanoNodeProject.hsPkgs;
  cardanoNodeProfiledHaskellPackages = (import ./haskell.nix {
    inherit compiler-nix-name
      lib
      haskell-nix
      buildPackages
      gitrev
      ;
    profiling = true;
  }).hsPkgs;
  cardanoNodeEventlogHaskellPackages = (import ./haskell.nix {
    inherit compiler-nix-name
      lib
      haskell-nix
      buildPackages
      gitrev
      ;
    eventlog = true;
  }).hsPkgs;
  cardanoNodeAssertedHaskellPackages = (import ./haskell.nix {
    inherit compiler-nix-name
      lib
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
  }).hsPkgs;

  #Grab the executable component of our package.
  inherit (cardanoNodeHaskellPackages.cardano-node.components.exes) cardano-node;
  inherit (cardanoNodeHaskellPackages.cardano-cli.components.exes) cardano-cli;
  inherit (cardanoNodeHaskellPackages.cardano-topology.components.exes) cardano-topology;
  inherit (cardanoNodeHaskellPackages.tx-generator.components.exes) tx-generator;
  inherit (cardanoNodeHaskellPackages.locli.components.exes) locli;
  inherit (cardanoNodeHaskellPackages.bech32.components.exes) bech32;
  inherit (cardanoNodeHaskellPackages.cardano-submit-api.components.exes) cardano-submit-api;
  cardano-node-profiled = cardanoNodeProfiledHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-eventlogged = cardanoNodeEventlogHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-asserted = cardanoNodeAssertedHaskellPackages.cardano-node.components.exes.cardano-node;
  tx-generator-profiled = cardanoNodeProfiledHaskellPackages.tx-generator.components.exes.tx-generator;
  plutus-scripts = callPackage ./plutus-scripts.nix { plutus-builder = cardanoNodeHaskellPackages.plutus-example.components.exes.plutus-example; };

  locli-profiled = cardanoNodeProfiledHaskellPackages.locli.components.exes.locli;

  # expose the db-converter and cardano-ping from the ouroboros-network we depend on
  inherit (cardanoNodeHaskellPackages.ouroboros-consensus-byron.components.exes) db-converter;
  inherit (cardanoNodeHaskellPackages.network-mux.components.exes) cardano-ping;

  cabal = haskell-nix.tool compiler-nix-name "cabal" {
    version = "latest";
    inherit (cardanoNodeProject) index-state;
  };

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    inherit (cardanoNodeProject) index-state;
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    inherit (cardanoNodeProject) index-state;
  };

  cardanolib-py = callPackage ./cardanolib-py {};

  scripts = lib.recursiveUpdate (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; });

  dockerImage = let
    defaultConfig = {
      stateDir = "/data";
      dbPrefix = "db";
      socketPath = "/ipc/node.socket";
    };
  in callPackage ./docker {
    exe = "cardano-node";
    scripts = import ./scripts.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
    script = "node";
  };

  submitApiDockerImage = let
    defaultConfig = {
      socketPath = "/node-ipc/node.socket";
      listenAddress = "0.0.0.0";
    };
  in callPackage ./docker/submit-api.nix {
    exe = "cardano-submit-api";
    scripts = import ./scripts-submit-api.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
    script = "submit-api";
  };

  # NixOS tests run a node and submit-api and validate it listens
  nixosTests = import ./nixos/tests {
    inherit pkgs;
  };

  clusterTests = import ./supervisord-cluster/tests { inherit pkgs; };

  # Disable failing python uvloop tests
  python38 = prev.python38.override {
    packageOverrides = pythonFinal: pythonPrev: {
      uvloop = pythonPrev.uvloop.overrideAttrs (attrs: {
        disabledTestPaths = [ "tests/test_tcp.py" "tests/test_sourcecode.py" "tests/test_dns.py" ];
      });
    };
  };
}
