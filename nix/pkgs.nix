# our packages overlay
pkgs: _: with pkgs; {
  cardanoNodeHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
  cardanoNodeProfiledHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
    profiling = true;
  };
}
