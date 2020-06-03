############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-node ? { outPath = ./.; rev = "abcdef"; }


# Function arguments to pass to the project
, projectArgs ? {
    inherit sourcesOverride;
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-node.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling (default: linux)
, supportedCrossSystems ? [ (builtins.head supportedSystems) ]

# Cross compilation to Windows is currently only supported on linux.
, windowsBuild ? builtins.elem "x86_64-linux" supportedCrossSystems

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-node;
  gitrev = cardano-node.rev;
};

with pkgs.lib;

let
  makeScripts = cluster: let
    getScript = name: {
      x86_64-linux = (pkgsFor "x86_64-linux").scripts.${cluster}.${name};
      x86_64-darwin = (pkgsFor "x86_64-darwin").scripts.${cluster}.${name};
    };
  in {
    node = getScript "node";
  };
  dockerImageArtifact = let
    image = (pkgsFor (builtins.head  supportedSystems)).dockerImage;
    wrapImage = image: pkgs.runCommand "${image.name}-hydra" {} ''
      mkdir -pv $out/nix-support/
      cat <<EOF > $out/nix-support/hydra-build-products
      file dockerimage-${image.name} ${image}
      EOF
    '';
  in wrapImage image;
  mkPins = inputs: pkgs.runCommand "ifd-pins" {} ''
    mkdir $out
    cd $out
    ${lib.concatMapStringsSep "\n" (input: "ln -sv ${input.value} ${input.key}") (lib.attrValues (lib.mapAttrs (key: value: { inherit key value; }) inputs))}
  '';
  makeRelease = cluster: {
    name = cluster;
    value = {
      scripts = makeScripts cluster;
    };
  };
  extraBuilds = {
    # only build nixos tests on first supported system (linux)
    inherit (pkgsFor (builtins.head  supportedSystems)) nixosTests;
    cardano-deployment = pkgs.iohkNix.cardanoLib.mkConfigHtml { inherit (pkgs.iohkNix.cardanoLib.environments) mainnet testnet ff; };
  } // (builtins.listToAttrs (map makeRelease [
    "mainnet"
    "staging"
    "shelley_staging_short"
    "shelley_staging"
    "testnet"
  ]));

  # restrict supported systems to a subset where tests (if exist) are required to pass:
  testsSupportedSystems = intersectLists supportedSystems [ "x86_64-linux" "x86_64-darwin" ];
  # Recurse through an attrset, returning all derivations in a list matching test supported systems.
  collectJobs' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectJobs = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectJobs' package)
    ) ds);

  # Remove build jobs for which cross compiling does not make sense.
  filterJobsCross = filterAttrs (n: _: n != "dockerImage" && n != "shell");

  inherit (systems.examples) mingwW64 musl64;

  inherit (pkgs.commonLib) sources nixpkgs;

  jobs = {
    inherit dockerImageArtifact;
    native = mapTestOn (__trace (__toJSON (packagePlatforms project)) (packagePlatforms project));
    # TODO: fix broken evals
    #musl64 = mapTestOnCross musl64 (packagePlatformsCross (filterJobsCross project));
    ifd-pins = mkPins {
      inherit (sources) iohk-nix "haskell.nix";
      inherit nixpkgs;
      inherit (pkgs.haskell-nix) hackageSrc stackageSrc;
    };
  } // (optionalAttrs windowsBuild {
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross (filterJobsCross project));
    cardano-node-win64 = import ./nix/windows-release.nix {
      inherit pkgs project;
      cardano-node = head (collectJobs jobs.${mingwW64.config}.cardano-node);
      chairman = head (collectJobs jobs.${mingwW64.config}.chairman);
    };
  }) // extraBuilds // (mkRequiredJob (concatLists [
      (collectJobs jobs.native.checks)
      (optionals windowsBuild (collectJobs jobs.${mingwW64.config}.checks))
      (collectJobs jobs.native.benchmarks)
      (collectJobs jobs.native.cardano-node)
      (optionals windowsBuild (collectJobs jobs.${mingwW64.config}.cardano-node))
      (optional windowsBuild jobs.cardano-node-win64)
      (map (cluster: collectJobs jobs.${cluster}.scripts.node.${head supportedSystems}) [ "mainnet" "testnet" "staging" ])
      (collectJobs jobs.nixosTests.chairmansCluster)
      [
        jobs.dockerImageArtifact
      ]
    ]));

in jobs
