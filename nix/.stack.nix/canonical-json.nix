{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "canonical-json"; version = "0.5.0.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2015-2017 Well-Typed LLP";
      maintainer = "duncan@well-typed.com, edsko@well-typed.com";
      author = "Duncan Coutts, Edsko de Vries";
      homepage = "https://github.com/well-typed/canonical-json";
      url = "";
      synopsis = "Canonical JSON for signing and hashing JSON values";
      description = "An implementation of Canonical JSON.\n\n<http://wiki.laptop.org/go/Canonical_JSON>\n\nThe \\\"canonical JSON\\\" format is designed to provide\nrepeatable hashes of JSON-encoded data. It is designed\nfor applications that need to hash, sign or authenitcate\nJSON data structures, including embedded signatures.\n\nCanonical JSON is parsable with any full JSON parser, and\nit allows whitespace for pretty-printed human readable\npresentation, but it can be put into a canonical form\nwhich then has a stable serialised representation and\nthus a stable hash.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.parsec)
          (hsPkgs.pretty)
          ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.canonical-json)
            (hsPkgs.aeson)
            (hsPkgs.vector)
            (hsPkgs.unordered-containers)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "http://github.com/nc6/canonical-json";
      rev = "a9dc9b893649bc2e2a770ab22d278a780f7e3a3c";
      sha256 = "0alwbi9xqaj6fmwzs6lr2drqrnhlnp13d9k2qkl5ga7h4grz9zcr";
      });
    }