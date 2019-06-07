{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "bimap"; version = "0.3.3"; };
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
      rev = "997fbb38b08dec14d225d064dac05b0a85f4ceae";
      sha256 = "0wacks3f94rpms1ghg6lp8jw73rr3kz6zcw5c39drhnvv1dzc8sz";
      });
    }