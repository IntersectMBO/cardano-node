{ target, pkgs, nix-tools }:

let
  commonLib = import ./lib.nix;
  pkgsCross = commonLib.iohkNix.getPkgs { crossSystem = pkgs.lib.systems.examples.mingwW64; };
in pkgs.runCommandCC "daedalus-bridge" {} ''
  mkdir -pv $out
  cd $out
  cp ${nix-tools.cexes.cardano-launcher.cardano-launcher}/bin/cardano-launcher* .
  cp ${nix-tools.cexes.cardano-node.cardano-node}/bin/cardano-node* .
  ${pkgs.lib.optionalString (target == "x86_64-windows") ''
    cp ${pkgsCross.libffi}/bin/libffi-6.dll .
    cp ${pkgsCross.openssl.out}/lib/libeay32.dll .
  ''}
  ${pkgs.lib.optionalString (target == "x86_64-linux") ''
    chmod +w -R .
    for bin in cardano-node cardano-launcher; do
      strip $bin
      patchelf --shrink-rpath $bin
    done
  ''}
''
