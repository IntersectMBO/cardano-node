{ pkgs ? (import <nixpkgs> {})
, commonLib
, supportedSystems ? [ "x86_64-linux" ]
, interactive ? false
}:

with pkgs;
with pkgs.lib;

 let
  forAllSystems = genAttrs supportedSystems;
  importTest = fn: args: system: let
    imported = import fn;
    test = import (pkgs.path + "/nixos/tests/make-test.nix") imported;
  in test ({
    inherit system;
  } // args);
  callTest = fn: args: forAllSystems (system: hydraJob (importTest fn args system));
in rec {
  # only tests that port is open since the test can't access network to actually sync
  # cardanoNodeEdge  = callTest ./cardano-node-edge.nix { inherit commonLib; };

  # Subsumes what cardanoNodeEdge does
  chairmansCluster = callTest ./chairmans-cluster.nix {
    inherit commonLib interactive;
  };
}
