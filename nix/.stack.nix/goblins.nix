{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "goblins"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "nick@topos.org.uk";
      author = "Nicholas Clarke";
      homepage = "https://github.com/input-output-hk/goblins";
      url = "";
      synopsis = "Genetic algorithm based randomised testing";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bimap)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.extra)
          (hsPkgs.hedgehog)
          (hsPkgs.lens)
          (hsPkgs.mmorph)
          (hsPkgs.monad-control)
          (hsPkgs.moo)
          (hsPkgs.random)
          (hsPkgs.template-haskell)
          (hsPkgs.th-utilities)
          (hsPkgs.transformers)
          (hsPkgs.tree-diff)
          (hsPkgs.typerep-map)
          ];
        };
      tests = {
        "goblin-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.hedgehog)
            (hsPkgs.goblins)
            (hsPkgs.temporary)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/goblins";
      rev = "26d35ad52fe9ade3391532dbfeb2f416f07650bc";
      sha256 = "17p5x0hj6c67jkdqx0cysqlwq2zs2l87azihn1alzajy9ak6ii0b";
      });
    }