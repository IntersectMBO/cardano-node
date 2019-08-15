with import ../../lib.nix;
with pkgs;

let
  stack-hpc-coveralls = iohkNix.stack-hpc-coveralls;
  stackRebuild = runCommand "stack-rebuild" {} ''
    ${haskellPackages.ghcWithPackages (ps: [ps.turtle ps.safe ps.transformers])}/bin/ghc -o $out ${./rebuild.hs}
  '';

  buildTools =
    [ git nix gnumake stack gnused gnutar coreutils stack-hpc-coveralls systemd ];

in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath buildTools}
    exec ${stackRebuild} "$@"
  ''
