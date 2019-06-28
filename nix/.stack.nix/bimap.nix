{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "bimap"; version = "0.4.0"; };
      license = "BSD-3-Clause";
      copyright = "Stuart Cook and contributors 2008, Joel Williamson 2015";
      maintainer = "Joel Williamson <joel@joelwilliamson.ca>";
      author = "Stuart Cook and contributors 2008, Joel Williamson 2015";
      homepage = "https://github.com/joelwilliamson/bimap";
      url = "";
      synopsis = "Bidirectional mapping between two key types";
      description = "A data structure representing a bidirectional mapping between two\nkey types. Each value in the bimap is associated with exactly one\nvalue of the opposite type.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.containers)
          (hsPkgs.deepseq)
          (hsPkgs.exceptions)
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6.1") (hsPkgs.ghc-prim);
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.exceptions)
            (hsPkgs.QuickCheck)
            (hsPkgs.template-haskell)
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6.1") (hsPkgs.ghc-prim);
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/joelwilliamson/bimap";
      rev = "013a0639c222b3f5572ae20ce364815e5d8f2156";
      sha256 = "1nlpmx4i42a2c6xij6hrj8byvzc0d9qqvwhy2040cqy9xsihqg7x";
      });
    }