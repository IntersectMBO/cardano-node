with import ./lib.nix;
with pkgs;

let
  stack-pkgs = import ./.stack.nix;
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

in haskell.lib.buildStackProject {
  name = "cardano-ledger-env";
  buildInputs = [ zlib openssl gmp libffi git systemd haskellPackages.happy ];
  ghc = haskell.packages.${compiler}.ghc;
}
