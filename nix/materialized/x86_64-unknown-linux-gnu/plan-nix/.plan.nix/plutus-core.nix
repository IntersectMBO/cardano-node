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
      specVersion = "3.0";
      identifier = { name = "plutus-core"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Plutus Core Team";
      homepage = "";
      url = "";
      synopsis = "Language library for Plutus Core";
      description = "Pretty-printer, parser, and typechecker for Plutus Core.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cost-model/data/builtinCostModel.json"
        "cost-model/data/cekMachineCosts.json"
        "cost-model/data/benching.csv"
        "cost-model/data/*.R"
        "plutus-core/test/CostModelInterface/defaultCostModelParams.json"
        ];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."composition-prelude" or (errorHandler.buildDepError "composition-prelude"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."dom-lt" or (errorHandler.buildDepError "dom-lt"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
          ];
        buildable = true;
        modules = [
          "PlutusCore/Analysis/Definitions"
          "PlutusCore/Builtin/HasConstant"
          "PlutusCore/Builtin/KnownKind"
          "PlutusCore/Builtin/KnownType"
          "PlutusCore/Builtin/KnownTypeAst"
          "PlutusCore/Builtin/Meaning"
          "PlutusCore/Builtin/Polymorphism"
          "PlutusCore/Builtin/Runtime"
          "PlutusCore/Builtin/TypeScheme"
          "PlutusCore/Core/Instance"
          "PlutusCore/Core/Instance/Eq"
          "PlutusCore/Core/Instance/Pretty"
          "PlutusCore/Core/Instance/Pretty/Classic"
          "PlutusCore/Core/Instance/Pretty/Common"
          "PlutusCore/Core/Instance/Pretty/Default"
          "PlutusCore/Core/Instance/Pretty/Plc"
          "PlutusCore/Core/Instance/Pretty/Readable"
          "PlutusCore/Core/Instance/Recursive"
          "PlutusCore/Core/Instance/Scoping"
          "PlutusCore/Core/Plated"
          "PlutusCore/Core/Type"
          "PlutusCore/DeBruijn/Internal"
          "PlutusCore/Default/Builtins"
          "PlutusCore/Default/Universe"
          "PlutusCore/Eq"
          "PlutusCore/Evaluation/Machine/ExBudgetingDefaults"
          "PlutusCore/InlineUtils"
          "PlutusCore/Parser/ParserCommon"
          "PlutusCore/Pretty/Classic"
          "PlutusCore/Pretty/ConfigName"
          "PlutusCore/Pretty/Default"
          "PlutusCore/Pretty/Plc"
          "PlutusCore/Pretty/PrettyConst"
          "PlutusCore/Pretty/Readable"
          "PlutusCore/Pretty/Utils"
          "PlutusCore/Size"
          "PlutusCore/TypeCheck"
          "PlutusCore/TypeCheck/Internal"
          "PlutusIR/Analysis/Dependencies"
          "PlutusIR/Analysis/Size"
          "PlutusIR/Analysis/Usages"
          "PlutusIR/Compiler/Datatype"
          "PlutusIR/Compiler/Error"
          "PlutusIR/Compiler/Let"
          "PlutusIR/Compiler/Lower"
          "PlutusIR/Compiler/Provenance"
          "PlutusIR/Compiler/Recursion"
          "PlutusIR/Compiler/Types"
          "PlutusIR/Normalize"
          "PlutusIR/TypeCheck/Internal"
          "UntypedPlutusCore/Analysis/Definitions"
          "UntypedPlutusCore/Analysis/Usages"
          "UntypedPlutusCore/Core"
          "UntypedPlutusCore/Core/Instance"
          "UntypedPlutusCore/Core/Instance/Eq"
          "UntypedPlutusCore/Core/Instance/Flat"
          "UntypedPlutusCore/Core/Instance/Pretty"
          "UntypedPlutusCore/Core/Instance/Pretty/Classic"
          "UntypedPlutusCore/Core/Instance/Pretty/Default"
          "UntypedPlutusCore/Core/Instance/Pretty/Plc"
          "UntypedPlutusCore/Core/Instance/Pretty/Readable"
          "UntypedPlutusCore/Core/Instance/Recursive"
          "UntypedPlutusCore/Core/Plated"
          "UntypedPlutusCore/Evaluation/Machine/Cek/CekMachineCosts"
          "UntypedPlutusCore/Evaluation/Machine/Cek/ExBudgetMode"
          "UntypedPlutusCore/Evaluation/Machine/Cek/EmitterMode"
          "UntypedPlutusCore/Mark"
          "UntypedPlutusCore/Rename/Internal"
          "UntypedPlutusCore/Simplify"
          "UntypedPlutusCore/Size"
          "UntypedPlutusCore/Subst"
          "UntypedPlutusCore/Transform/ForceDelay"
          "UntypedPlutusCore/Transform/Inline"
          "Data/Aeson/Flatten"
          "Data/Aeson/THReader"
          "Data/Functor/Foldable/Monadic"
          "Universe/Core"
          "GHC/Natural/Extras"
          "PlutusCore"
          "PlutusCore/Check/Normal"
          "PlutusCore/Check/Scoping"
          "PlutusCore/Check/Uniques"
          "PlutusCore/Check/Value"
          "PlutusCore/Builtin"
          "PlutusCore/Builtin/Debug"
          "PlutusCore/Builtin/Elaborate"
          "PlutusCore/Builtin/Emitter"
          "PlutusCore/Core"
          "PlutusCore/Data"
          "PlutusCore/DataFilePaths"
          "PlutusCore/DeBruijn"
          "PlutusCore/Default"
          "PlutusCore/Error"
          "PlutusCore/Evaluation/Machine/BuiltinCostModel"
          "PlutusCore/Evaluation/Machine/Ck"
          "PlutusCore/Evaluation/Machine/CostModelInterface"
          "PlutusCore/Evaluation/Machine/ExBudget"
          "PlutusCore/Evaluation/Machine/ExMemory"
          "PlutusCore/Evaluation/Machine/Exception"
          "PlutusCore/Evaluation/Machine/MachineParameters"
          "PlutusCore/Evaluation/Result"
          "PlutusCore/Examples/Builtins"
          "PlutusCore/Examples/Data/Data"
          "PlutusCore/Examples/Data/InterList"
          "PlutusCore/Examples/Data/List"
          "PlutusCore/Examples/Data/Pair"
          "PlutusCore/Examples/Data/Shad"
          "PlutusCore/Examples/Data/TreeForest"
          "PlutusCore/Examples/Data/Vec"
          "PlutusCore/Examples/Everything"
          "PlutusCore/Flat"
          "PlutusCore/FsTree"
          "PlutusCore/Mark"
          "PlutusCore/MkPlc"
          "PlutusCore/Name"
          "PlutusCore/Normalize"
          "PlutusCore/Normalize/Internal"
          "PlutusCore/Parser"
          "PlutusCore/Pretty"
          "PlutusCore/Quote"
          "PlutusCore/Rename"
          "PlutusCore/Rename/Internal"
          "PlutusCore/Rename/Monad"
          "PlutusCore/StdLib/Data/Bool"
          "PlutusCore/StdLib/Data/ChurchNat"
          "PlutusCore/StdLib/Data/Data"
          "PlutusCore/StdLib/Data/Function"
          "PlutusCore/StdLib/Data/Integer"
          "PlutusCore/StdLib/Data/List"
          "PlutusCore/StdLib/Data/Nat"
          "PlutusCore/StdLib/Data/Pair"
          "PlutusCore/StdLib/Data/ScottList"
          "PlutusCore/StdLib/Data/ScottUnit"
          "PlutusCore/StdLib/Data/Sum"
          "PlutusCore/StdLib/Data/Unit"
          "PlutusCore/StdLib/Everything"
          "PlutusCore/StdLib/Meta"
          "PlutusCore/StdLib/Meta/Data/Function"
          "PlutusCore/StdLib/Meta/Data/Tuple"
          "PlutusCore/StdLib/Type"
          "PlutusCore/Subst"
          "PlutusIR"
          "PlutusIR/Analysis/RetainedSize"
          "PlutusIR/Compiler"
          "PlutusIR/Compiler/Definitions"
          "PlutusIR/Compiler/Names"
          "PlutusIR/Core"
          "PlutusIR/Core/Instance"
          "PlutusIR/Core/Instance/Flat"
          "PlutusIR/Core/Instance/Pretty"
          "PlutusIR/Core/Instance/Scoping"
          "PlutusIR/Core/Plated"
          "PlutusIR/Core/Type"
          "PlutusIR/Error"
          "PlutusIR/Mark"
          "PlutusIR/MkPir"
          "PlutusIR/Parser"
          "PlutusIR/Purity"
          "PlutusIR/Subst"
          "PlutusIR/Transform/Beta"
          "PlutusIR/Transform/DeadCode"
          "PlutusIR/Transform/Inline"
          "PlutusIR/Transform/LetFloat"
          "PlutusIR/Transform/LetMerge"
          "PlutusIR/Transform/RecSplit"
          "PlutusIR/Transform/NonStrict"
          "PlutusIR/Transform/Rename"
          "PlutusIR/Transform/Substitute"
          "PlutusIR/Transform/ThunkRecursions"
          "PlutusIR/Transform/Unwrap"
          "PlutusIR/TypeCheck"
          "UntypedPlutusCore"
          "UntypedPlutusCore/DeBruijn"
          "UntypedPlutusCore/Evaluation/Machine/Cek"
          "UntypedPlutusCore/Evaluation/Machine/Cek/Internal"
          "UntypedPlutusCore/Parser"
          "UntypedPlutusCore/Rename"
          "UntypedPlutusCore/MkUPlc"
          "UntypedPlutusCore/Check/Scope"
          "UntypedPlutusCore/Check/Uniques"
          "UntypedPlutusCore/Core/Type"
          "Crypto"
          "Data/ByteString/Hash"
          "Data/SatInt"
          "Prettyprinter/Custom"
          "ErrorCode"
          "PlutusPrelude"
          "Universe"
          ];
        hsSourceDirs = [
          "plutus-core/src"
          "plutus-core/stdlib"
          "plutus-core/examples"
          "plutus-ir/src"
          "untyped-plutus-core/src"
          "prelude"
          "common"
          ];
        };
      sublibs = {
        "plutus-core-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            (hsPkgs."lazy-search" or (errorHandler.buildDepError "lazy-search"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
            (hsPkgs."size-based" or (errorHandler.buildDepError "size-based"))
            (hsPkgs."some" or (errorHandler.buildDepError "some"))
            (hsPkgs."Stream" or (errorHandler.buildDepError "Stream"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "PlutusCore/Generators/Internal/Denotation"
            "PlutusCore/Generators/Internal/Dependent"
            "PlutusCore/Generators/Internal/Entity"
            "PlutusCore/Generators/Internal/TypeEvalCheck"
            "PlutusCore/Generators/Internal/TypedBuiltinGen"
            "PlutusCore/Generators/Internal/Utils"
            "PlutusCore/Test"
            "PlutusCore/Generators"
            "PlutusCore/Generators/AST"
            "PlutusCore/Generators/Interesting"
            "PlutusCore/Generators/NEAT/Common"
            "PlutusCore/Generators/NEAT/Spec"
            "PlutusCore/Generators/NEAT/Term"
            "PlutusCore/Generators/NEAT/Type"
            "PlutusCore/Generators/Test"
            "PlutusIR/Test"
            "PlutusIR/Generators/AST"
            "Test/Tasty/Extras"
            ];
          hsSourceDirs = [ "testlib" ];
          };
        "index-envs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
            ];
          buildable = true;
          modules = [ "Data/DeBruijnEnv" "Data/RandomAccessList/SkewBinary" ];
          hsSourceDirs = [ "index-envs/src" ];
          };
        };
      exes = {
        "plc" = {
          depends = [
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            ];
          buildable = true;
          modules = [ "Common" "Parsers" ];
          hsSourceDirs = [ "executables" ];
          mainPath = [ "plc/Main.hs" ];
          };
        "uplc" = {
          depends = [
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            ];
          buildable = true;
          modules = [ "Common" "Parsers" ];
          hsSourceDirs = [ "executables" ];
          mainPath = [ "uplc/Main.hs" ];
          };
        "pir" = {
          depends = [
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            ];
          buildable = true;
          modules = [ "Common" "Parsers" ];
          hsSourceDirs = [ "executables" ];
          mainPath = [ "pir/Main.hs" ];
          };
        "traceToStacks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          modules = [ "Common" ];
          hsSourceDirs = [ "executables/traceToStacks" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "satint-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            ];
          buildable = true;
          hsSourceDirs = [ "plutus-core/satint-test" ];
          mainPath = [ "TestSatInt.hs" ];
          };
        "plutus-core-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
            (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
            ];
          buildable = true;
          modules = [
            "Check/Spec"
            "CostModelInterface/Spec"
            "Evaluation/Machines"
            "Evaluation/Spec"
            "Names/Spec"
            "Normalization/Check"
            "Normalization/Type"
            "Pretty/Readable"
            "TypeSynthesis/Spec"
            ];
          hsSourceDirs = [ "plutus-core/test" ];
          mainPath = [ "Spec.hs" ];
          };
        "plutus-ir-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [ "NamesSpec" "ParserSpec" "TransformSpec" "TypeSpec" ];
          hsSourceDirs = [ "plutus-ir/test" ];
          mainPath = [ "Spec.hs" ];
          };
        "untyped-plutus-core-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Evaluation/Builtins"
            "Evaluation/Builtins/Common"
            "Evaluation/Builtins/Definition"
            "Evaluation/Builtins/MakeRead"
            "Evaluation/FreeVars"
            "Evaluation/Golden"
            "Evaluation/Machines"
            "Transform/Simplify"
            "DeBruijn/Common"
            "DeBruijn/Scope"
            "DeBruijn/Spec"
            "DeBruijn/UnDeBruijnify"
            ];
          hsSourceDirs = [ "untyped-plutus-core/test" ];
          mainPath = [ "Spec.hs" ];
          };
        "traceToStacks-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          modules = [ "Common" ];
          hsSourceDirs = [ "executables/traceToStacks" ];
          mainPath = [ "TestGetStacks.hs" ];
          };
        "index-envs-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          hsSourceDirs = [ "index-envs/test" ];
          mainPath = [ "TestRAList.hs" ];
          };
        };
      benchmarks = {
        "cost-model-budgeting-bench" = {
          depends = [
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."criterion-measurement" or (errorHandler.buildDepError "criterion-measurement"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Common"
            "CriterionExtensions"
            "Generators"
            "Benchmarks/Bool"
            "Benchmarks/ByteStrings"
            "Benchmarks/CryptoAndHashes"
            "Benchmarks/Data"
            "Benchmarks/Integers"
            "Benchmarks/Lists"
            "Benchmarks/Misc"
            "Benchmarks/Nops"
            "Benchmarks/Pairs"
            "Benchmarks/Strings"
            "Benchmarks/Tracing"
            "Benchmarks/Unit"
            ];
          hsSourceDirs = [ "cost-model/budgeting-bench" ];
          };
        "update-cost-model" = {
          depends = [
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          modules = [ "CostModelCreation" ];
          hsSourceDirs = [ "cost-model/create-cost-model" ];
          };
        "cost-model-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          modules = [ "TH" "CostModelCreation" ];
          hsSourceDirs = [ "cost-model/test" "cost-model/create-cost-model" ];
          };
        "index-envs-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
            ];
          buildable = true;
          hsSourceDirs = [ "index-envs/bench" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "11";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "11";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/plutus-core; echo source root reset to $sourceRoot";
    }