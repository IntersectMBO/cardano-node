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

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

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
    image = (pkgsFor "x86_64-linux").dockerImage;
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
    # only build nixos tests for linux
    inherit (pkgsFor "x86_64-linux") nixosTests;
    cardano-deployment = pkgs.iohkNix.cardanoLib.mkConfigHtml { inherit (pkgs.iohkNix.cardanoLib.environments) mainnet testnet ff; };
  } // (builtins.listToAttrs (map makeRelease [
    "mainnet"
    "staging"
    "shelley_staging_short"
    "shelley_staging"
    "testnet"
  ]));

  testsSupportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  # Recurse through an attrset, returning all test derivations in a list.
  collectTests' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the test derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectTests = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectTests' package)
    ) ds);

  # Remove build jobs for which cross compiling does not make sense.
  filterJobsCross = filterAttrs (n: _: n != "dockerImage" && n != "shell");

  inherit (systems.examples) mingwW64 musl64;

  inherit (pkgs.commonLib) sources nixpkgs;

  jobs = {
    inherit dockerImageArtifact;
    cardano-node-win64 = import ./nix/windows-release.nix {
      inherit pkgs project;
      cardano-node = jobs.x86_64-w64-mingw32.cardano-node.x86_64-linux;
      chairman = jobs.x86_64-w64-mingw32.chairman.x86_64-linux;
    };
    native = mapTestOn (__trace (__toJSON (packagePlatforms project)) (packagePlatforms project));
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross (filterJobsCross project));
    # TODO: fix broken evals
    #musl64 = mapTestOnCross musl64 (packagePlatformsCross (filterJobsCross project));
    ifd-pins = mkPins {
      inherit (sources) iohk-nix "haskell.nix";
      inherit nixpkgs;
      #hackageSrc = (import pkgs.path (import sources."haskell.nix")).haskell-nix.hackageSrc;
      #stackageSrc = (import pkgs.path (import sources."haskell.nix")).haskell-nix.stackageSrc;
    };
  } // extraBuilds // (mkRequiredJob (
      collectTests jobs.native.checks ++
      collectTests jobs."${mingwW64.config}".checks ++
      collectTests jobs.native.benchmarks ++ [
      jobs.native.cardano-node.x86_64-darwin
      jobs.native.cardano-node.x86_64-linux
      jobs."${mingwW64.config}".cardano-node.x86_64-linux
      jobs.cardano-node-win64
      jobs.dockerImageArtifact

      (map (cluster: jobs.${cluster}.scripts.node.x86_64-linux) [ "mainnet" "testnet" "staging" ])

      jobs.nixosTests.chairmansCluster.x86_64-linux
    ]));

in jobs
