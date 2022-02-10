############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-node ? { outPath = ./.; rev = "abcdef"; }
}:

let
  inherit (import ./nix/flake-compat.nix {
    src = cardano-node;
  }) defaultNix;

  jobs = defaultNix.hydraJobs;

in
jobs
