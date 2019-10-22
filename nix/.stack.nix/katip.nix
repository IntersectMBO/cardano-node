{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { lib-werror = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "katip"; version = "0.8.3.0"; };
      license = "BSD-3-Clause";
      copyright = "Soostone Inc, 2015-2017";
      maintainer = "michael.xavier@soostone.com";
      author = "Ozgun Ataman, Michael Xavier";
      homepage = "https://github.com/Soostone/katip";
      url = "";
      synopsis = "A structured logging framework.";
      description = "Katip is a structured logging framework. See README.md for more details.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.auto-update)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.either)
          (hsPkgs.safe-exceptions)
          (hsPkgs.hostname)
          (hsPkgs.old-locale)
          (hsPkgs.string-conv)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.transformers-compat)
          (hsPkgs.unordered-containers)
          (hsPkgs.monad-control)
          (hsPkgs.mtl)
          (hsPkgs.transformers-base)
          (hsPkgs.resourcet)
          (hsPkgs.scientific)
          (hsPkgs.microlens)
          (hsPkgs.microlens-th)
          (hsPkgs.semigroups)
          (hsPkgs.unliftio-core)
          (hsPkgs.stm)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.katip)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.tasty)
            (hsPkgs.tasty-golden)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.template-haskell)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.time-locale-compat)
            (hsPkgs.directory)
            (hsPkgs.regex-tdfa)
            (hsPkgs.unordered-containers)
            (hsPkgs.microlens)
            (hsPkgs.containers)
            (hsPkgs.stm)
            (hsPkgs.safe-exceptions)
            ];
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.blaze-builder)
            (hsPkgs.katip)
            (hsPkgs.criterion)
            (hsPkgs.unix)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.deepseq)
            (hsPkgs.async)
            (hsPkgs.filepath)
            (hsPkgs.safe-exceptions)
            (hsPkgs.directory)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/Soostone/katip.git";
      rev = "cf60f0e14640ae9702c111bb51aeca7eed50ad48";
      sha256 = "0izdcz7mq3wq4hi1cj6lql17f0389l7ak0if2nwrphg752pa76rv";
      });
    postUnpack = "sourceRoot+=/katip; echo source root reset to \$sourceRoot";
    }