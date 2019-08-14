{
  commonLib,
  customConfig,
  nixTools
}:

let
  cardanoNode = nixTools.nix-tools.exes.cardano-node;

in {
  node = import ./cardano-node-scripts.nix {
    inherit commonLib cardanoNode customConfig;
  };
}
