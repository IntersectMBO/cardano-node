{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}:
let

   # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  src = pkgs.lib.cleanSourceWith {
    src = pkgs.lib.cleanSource ../.;
    filter = name: type: type != "unknown";
  };


  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.extras haskell.hackage).compiler.nix-name;
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    pkg-def-extras = [
      stack-pkgs.extras
      iohk-extras.${compiler}
    ];
    modules = [
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module

       {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # Add source filtering to local packages
        packages.cardano-node.src = src;
      }
    ];
  };

 in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
