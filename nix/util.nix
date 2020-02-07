{ haskell-nix }:

with haskell-nix.haskellLib;
{

  inherit selectProjectPackages;

  inherit (extra)
    collectComponents'
    collectChecks;

}
