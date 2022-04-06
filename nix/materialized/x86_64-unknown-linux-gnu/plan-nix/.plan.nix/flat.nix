{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "flat"; version = "0.4.5"; };
      license = "BSD-3-Clause";
      copyright = "Copyright: (c) 2016-2021 Pasqualino `Titto` Assini";
      maintainer = "tittoassini@gmail.com";
      author = "Pasqualino `Titto` Assini";
      homepage = "http://quid2.org";
      url = "";
      synopsis = "Principled and efficient bit-oriented binary serialization.";
      description = "Reference implementation of `flat`, a principled and efficient binary serialization format.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "stack.yaml"
        "stack-6.35.yaml"
        "stack-9.21.yaml"
        "README.md"
        "CHANGELOG"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = if compiler.isEta && true
          then [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ]
          else [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            ];
        buildable = true;
        modules = [
          "Data/ByteString/Convert"
          "Data/FloatCast"
          "Data/ZigZag"
          "Flat"
          "Flat/Bits"
          "Flat/Class"
          "Flat/Decoder"
          "Flat/Decoder/Prim"
          "Flat/Decoder/Run"
          "Flat/Decoder/Strict"
          "Flat/Decoder/Types"
          "Flat/Encoder"
          "Flat/Encoder/Prim"
          "Flat/Encoder/Size"
          "Flat/Encoder/Strict"
          "Flat/Encoder/Types"
          "Flat/Endian"
          "Flat/Filler"
          "Flat/Instances"
          "Flat/Instances/Array"
          "Flat/Instances/Base"
          "Flat/Instances/ByteString"
          "Flat/Instances/Containers"
          "Flat/Instances/DList"
          "Flat/Instances/Mono"
          "Flat/Instances/Test"
          "Flat/Instances/Text"
          "Flat/Instances/Unordered"
          "Flat/Instances/Util"
          "Flat/Instances/Vector"
          "Flat/Memory"
          "Flat/Run"
          "Flat/Repr"
          "Flat/Tutorial"
          "Flat/Types"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "spec" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."quickcheck-text" or (errorHandler.buildDepError "quickcheck-text"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if compiler.isEta && true
            then [
              (hsPkgs."array" or (errorHandler.buildDepError "array"))
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
              (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
              (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
              (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
              (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
              (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
              (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
              (hsPkgs."text" or (errorHandler.buildDepError "text"))
              ]
            else [
              (hsPkgs."array" or (errorHandler.buildDepError "array"))
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
              (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
              (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
              (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
              (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
              (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
              (hsPkgs."text" or (errorHandler.buildDepError "text"))
              (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
              ]);
          buildable = true;
          modules = [
            "Test/Data"
            "Test/Data/Arbitrary"
            "Test/Data/Flat"
            "Test/Data/Values"
            "Test/Data2"
            "Test/Data2/Flat"
            "Test/E"
            "Test/E/Arbitrary"
            "Test/E/Flat"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        "doc-static" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          modules = [
            "DocTest"
            "DocTest/Data/FloatCast"
            "DocTest/Data/ZigZag"
            "DocTest/Flat/Bits"
            "DocTest/Flat/Decoder/Prim"
            "DocTest/Flat/Endian"
            "DocTest/Flat/Instances/Array"
            "DocTest/Flat/Instances/Base"
            "DocTest/Flat/Instances/ByteString"
            "DocTest/Flat/Instances/Containers"
            "DocTest/Flat/Instances/DList"
            "DocTest/Flat/Instances/Mono"
            "DocTest/Flat/Instances/Text"
            "DocTest/Flat/Instances/Unordered"
            "DocTest/Flat/Instances/Vector"
            "DocTest/Flat/Tutorial"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "DocTests.hs" ];
          };
        "Repr" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "FlatRepr.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "14";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "14";
      rev = "minimal";
      sha256 = "";
      };
    }