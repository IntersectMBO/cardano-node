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
    flags = { development = false; external-libsodium-vrf = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-crypto-praos"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Crypto primitives from libsodium";
      description = "VRF (and KES, tba) primitives from libsodium.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "cbits/crypto_vrf.h"
        "cbits/vrf03/crypto_vrf_ietfdraft03.h"
        "cbits/vrf03/vrf_ietfdraft03.h"
        "cbits/vrf10_batchcompat/crypto_vrf_ietfdraft10.h"
        "cbits/vrf10_batchcompat/vrf_ietfdraft10.h"
        "cbits/private/common.h"
        "cbits/private/quirks.h"
        "cbits/private/ed25519_ref10.h"
        "cbits/private/hash_to_curve.h"
        "cbits/private/ed25519_ref10_fe_25_5.h"
        "cbits/private/ed25519_ref10_fe_51.h"
        "cbits/private/fe_25_5/constants.h"
        "cbits/private/fe_25_5/base.h"
        "cbits/private/fe_25_5/base2.h"
        "cbits/private/fe_25_5/fe.h"
        "cbits/private/fe_51/constants.h"
        "cbits/private/fe_51/base.h"
        "cbits/private/fe_51/base2.h"
        "cbits/private/fe_51/fe.h"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
          ];
        buildable = true;
        modules = [
          "Cardano/Crypto/VRF/Praos"
          "Cardano/Crypto/VRF/PraosBatchCompat"
          "Cardano/Crypto/RandomBytes"
          ];
        cSources = (pkgs.lib).optionals (!flags.external-libsodium-vrf) [
          "cbits/crypto_vrf.c"
          "cbits/vrf03/convert.c"
          "cbits/vrf03/keypair.c"
          "cbits/vrf03/prove.c"
          "cbits/vrf03/verify.c"
          "cbits/vrf03/vrf.c"
          "cbits/vrf10_batchcompat/convert.c"
          "cbits/vrf10_batchcompat/verify.c"
          "cbits/vrf10_batchcompat/keypair.c"
          "cbits/vrf10_batchcompat/prove.c"
          "cbits/vrf10_batchcompat/vrf.c"
          "cbits/private/hash_to_curve.c"
          "cbits/private/ed25519_ref10.c"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "3";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "3";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/cardano-crypto-praos; echo source root reset to $sourceRoot";
    }