
with import ../lib.nix {};

pkgs.haskell.lib.buildStackProject {
  name = "cardano-node-stack-env";
  buildInputs = with pkgs; [ zlib openssl gmp libffi git systemd haskellPackages.happy ];
  ghc = (import ../shell.nix {}).ghc;
}
