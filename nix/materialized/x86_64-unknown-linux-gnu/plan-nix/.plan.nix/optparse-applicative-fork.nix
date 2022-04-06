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
    flags = { process = true; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "optparse-applicative-fork";
        version = "0.16.1.0";
        };
      license = "BSD-3-Clause";
      copyright = "(c) 2012-2017 Paolo Capriotti <paolo@capriotti.io>";
      maintainer = "huw.campbell@gmail.com";
      author = "Paolo Capriotti, Huw Campbell";
      homepage = "https://github.com/pcapriotti/optparse-applicative-fork";
      url = "";
      synopsis = "Utilities and combinators for parsing command line options";
      description = "optparse-applicative-fork is a haskell library for parsing options\non the command line, and providing a powerful applicative\ninterface for composing them.\n\noptparse-applicative-fork takes care of reading and validating the\narguments passed to the command line, handling and reporting\nerrors, generating a usage line, a comprehensive help screen,\nand enabling context-sensitive bash, zsh, and fish completions.\n\nSee the included README for detailed instructions and examples,\nwhich is also available on github\n<https://github.com/pcapriotti/optparse-applicative-fork>.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "CHANGELOG.md"
        "README.md"
        "tests/alt.err.txt"
        "tests/cabal.err.txt"
        "tests/carry.err.txt"
        "tests/commands.err.txt"
        "tests/commands_header.err.txt"
        "tests/commands_header_full.err.txt"
        "tests/dropback.err.txt"
        "tests/hello.err.txt"
        "tests/helponempty.err.txt"
        "tests/helponemptysub.err.txt"
        "tests/long_equals.err.txt"
        "tests/formatting.err.txt"
        "tests/nested.err.txt"
        "tests/optional.err.txt"
        "tests/nested_optional.err.txt"
        "tests/subparsers.err.txt"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          ] ++ (pkgs.lib).optional (flags.process) (hsPkgs."process" or (errorHandler.buildDepError "process"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ];
        buildable = true;
        modules = [
          "Options/Applicative"
          "Options/Applicative/Arrows"
          "Options/Applicative/BashCompletion"
          "Options/Applicative/Builder"
          "Options/Applicative/Builder/Completer"
          "Options/Applicative/Builder/Internal"
          "Options/Applicative/Common"
          "Options/Applicative/Extra"
          "Options/Applicative/Help"
          "Options/Applicative/Help/Ann"
          "Options/Applicative/Help/Chunk"
          "Options/Applicative/Help/Core"
          "Options/Applicative/Help/Levenshtein"
          "Options/Applicative/Help/Pretty"
          "Options/Applicative/Help/Style"
          "Options/Applicative/Help/Types"
          "Options/Applicative/NonEmpty"
          "Options/Applicative/Types"
          "Options/Applicative/Internal"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          modules = [
            "Examples/Alternatives"
            "Examples/Cabal"
            "Examples/Commands"
            "Examples/Formatting"
            "Examples/Hello"
            ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "test.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "0";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "0";
      rev = "minimal";
      sha256 = "";
      };
    }