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
  nixosTests = (import ./. {}).nixosTests;
  getArchDefault = system: let
    table = {
      x86_64-linux = import ./. { system = "x86_64-linux"; };
      x86_64-darwin = import ./. { system = "x86_64-darwin"; };
      x86_64-windows = import ./. { system = "x86_64-linux"; crossSystem = "x86_64-windows"; };
    };
  in table.${system};
  default = getArchDefault builtins.currentSystem;
  makeScripts = cluster: let
    getScript = name: {
      x86_64-linux = (getArchDefault "x86_64-linux").scripts.${cluster}.${name};
      x86_64-darwin = (getArchDefault "x86_64-darwin").scripts.${cluster}.${name};
    };
  in {
    node = getScript "node";
  };
  dockerImageArtifact = let
    image = (getArchDefault "x86_64-linux").dockerImage;
    wrapImage = image: pkgs.runCommand "${image.name}-hydra" {} ''
      mkdir -pv $out/nix-support/
      cat <<EOF > $out/nix-support/hydra-build-products
      file dockerimage-${image.name} ${image}
      EOF
    '';
  in wrapImage image;

  mainnetConfig = let
    inherit ((import ./. {}).scripts.mainnet) topologyFile nodeConfigFile;
    wrapConfig = name: file: pkgs.runCommand name {} ''
      echo "Making $out/nix-support"
      mkdir -p $out/nix-support
      echo
      echo "Creating build product $out/nix-support/hydra-build-products"
      echo "file none ${file}" > $out/nix-support/hydra-build-products
      echo
      echo "Contents of $out/nix-support/hydra-build-products:"
      cat $out/nix-support/hydra-build-products
      echo
      echo "ls of the out: $out"
      ls -lah $out
      echo
      echo "ls of the file: ${file}"
      ls -lah ${file}
      echo
      echo "Cat-ing the file: ${file}"
      cat ${file}
    '';
  in {
    mainnetTopology = wrapConfig "topology.yaml" topologyFile;
    mainnetNodeConfig = wrapConfig "config.json" nodeConfigFile;
  };

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
  extraBuilds = let
    # only build nixos tests for linux
    default = getArchDefault "x86_64-linux";
  in {
    inherit nixosTests;
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

  sources = import ./nix/sources.nix;

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
      inherit (import "${sources.iohk-nix}/nix/sources.nix" {}) nixpkgs;
      #hackageSrc = (import pkgs.path (import sources."haskell.nix")).haskell-nix.hackageSrc;
      #stackageSrc = (import pkgs.path (import sources."haskell.nix")).haskell-nix.stackageSrc;
    };
    inherit (mainnetConfig) mainnetTopology mainnetNodeConfig;
  } // extraBuilds // (mkRequiredJob (
      collectTests jobs.native.checks ++
      collectTests jobs."${mingwW64.config}".checks ++
      collectTests jobs.native.benchmarks ++ [
      jobs.native.cardano-node.x86_64-darwin
      jobs.native.cardano-node.x86_64-linux
      jobs."${mingwW64.config}".cardano-node.x86_64-linux
      jobs.cardano-node-win64
      jobs.dockerImageArtifact
      jobs.mainnetTopology
      jobs.mainnetNodeConfig

      (map (cluster: jobs.${cluster}.scripts.node.x86_64-linux) [ "mainnet" "testnet" "staging" ])

      jobs.nixosTests.chairmansCluster.x86_64-linux
    ]));

in jobs
