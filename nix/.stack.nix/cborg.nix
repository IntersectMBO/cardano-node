{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { optimize-gmp = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cborg"; version = "0.2.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2015-2017 Duncan Coutts,\n2015-2017 Well-Typed LLP,\n2015 IRIS Connect Ltd";
      maintainer = "duncan@community.haskell.org, ben@smart-cactus.org";
      author = "Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "Concise Binary Object Representation";
      description = "This package (formerly @binary-serialise-cbor@) provides an efficient\nimplementation of the Concise Binary Object Representation (CBOR), as\nspecified by [RFC 7049](https://tools.ietf.org/html/rfc7049).\n\nIf you are looking for a library for serialisation of Haskell values,\nhave a look at the [serialise](/package/serialise) package, which is\nbuilt upon this library.\n\nAn implementation of the standard bijection between CBOR and JSON is provided\nby the [cborg-json](/package/cborg-json) package. Also see [cbor-tool](/package/cbor-tool)\nfor a convenient command-line utility for working with CBOR data.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs.array)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.deepseq)
          (hsPkgs.ghc-prim)
          (hsPkgs.half)
          (hsPkgs.primitive)
          (hsPkgs.text)
          ] ++ (pkgs.lib).optional (flags.optimize-gmp) (hsPkgs.integer-gmp)) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs.fail)
          (hsPkgs.semigroups)
          ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.array)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.text)
            (hsPkgs.cborg)
            (hsPkgs.aeson)
            (hsPkgs.base64-bytestring)
            (hsPkgs.base16-bytestring)
            (hsPkgs.deepseq)
            (hsPkgs.fail)
            (hsPkgs.half)
            (hsPkgs.QuickCheck)
            (hsPkgs.scientific)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.vector)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/well-typed/cborg";
      rev = "80fbe0ee5e67a5622e2cb9eaa9d8594a2214322d";
      sha256 = "1khd1v9yh6jdkcvzvknvhxpc1qvxvww0pp7c43w4hbvdyhs1q8wh";
      });
    postUnpack = "sourceRoot+=/cborg; echo source root reset to \$sourceRoot";
    }