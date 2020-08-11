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
    inherit (pkgsFor (builtins.head  supportedSystems));
    # Environments listed in Network Configuration page
    cardano-deployment = pkgs.iohkNix.cardanoLib.mkConfigHtml { inherit (pkgs.iohkNix.cardanoLib.environments) mainnet testnet mainnet_candidate_4; };
  } // (builtins.listToAttrs (map makeRelease [
    # Environments we want to build scripts for on hydra
    "mainnet"
    "mainnet_candidate"
    "staging"
    "shelley_qa"
    "shelley_staging"
    "shelley_testnet"
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

  nonDefaultBuildSystems = tail supportedSystems;

  # Paths or prefixes of paths of derivations to build only on the default system (ie. linux on hydra):
  onlyBuildOnDefaultSystem = [ ["checks" "hlint"] ["dockerImage"] ];
  # Paths or prefix of paths for which cross-builds (mingwW64, musl64) are disabled:
  noCrossBuild = [ ["shell"] ]
    ++ onlyBuildOnDefaultSystem;
  noMusl64Build = [ ["checks"] ["tests"] ["benchmarks"] ["haskellPackages"] ]
    ++ noCrossBuild;

  # Remove build jobs for which cross compiling does not make sense.
  filterProject = noBuildList: mapAttrsRecursiveCond (a: !(isDerivation a)) (path: value:
    if (isDerivation value && (any (p: take (length p) path == p) noBuildList)) then null
    else value
  ) project;

  inherit (systems.examples) mingwW64 musl64;

  inherit (pkgs.commonLib) sources nixpkgs;

  jobs = {
    inherit dockerImageArtifact;
    native =
      let filteredBuilds = mapAttrsRecursiveCond (a: !(isList a)) (path: value:
        if (any (p: take (length p) path == p) onlyBuildOnDefaultSystem) then filter (s: !(elem s nonDefaultBuildSystems)) value else value)
        (packagePlatforms project);
      in (mapTestOn (__trace (__toJSON filteredBuilds) filteredBuilds));
    musl64 = mapTestOnCross musl64 (packagePlatformsCross (filterProject noMusl64Build));
    ifd-pins = mkPins {
      inherit (sources) iohk-nix "haskell.nix";
      inherit nixpkgs;
      inherit (pkgs.haskell-nix) hackageSrc stackageSrc;
    };
    cardano-node-macos = import ./nix/binary-release.nix {
      inherit pkgs project;
      platform = "macos";
      exes = filter (p: p.system == "x86_64-darwin") (collectJobs jobs.native.exes);
    };
    cardano-node-linux = import ./nix/binary-release.nix {
      inherit pkgs project;
      platform = "linux";
      exes = filter (p: p.system == "x86_64-linux") (collectJobs jobs.musl64.exes);
    };
  } // (optionalAttrs windowsBuild {
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross (filterProject noCrossBuild));
    cardano-node-win64 = import ./nix/binary-release.nix {
      inherit pkgs project;
      platform = "win64";
      exes = collectJobs jobs.${mingwW64.config}.exes;
    };
  }) // extraBuilds // (mkRequiredJob (concatLists [
      (collectJobs jobs.native.checks)
      (collectJobs jobs.native.benchmarks)
      (collectJobs jobs.native.exes)
      (optional windowsBuild jobs.cardano-node-win64)
      (optionals windowsBuild (collectJobs jobs.${mingwW64.config}.checks))
      (map (cluster: collectJobs jobs.${cluster}.scripts.node.${head supportedSystems}) [ "mainnet" "testnet" "staging" "shelley_qa" "shelley_testnet" ])
      [
        jobs.cardano-node-linux
        jobs.cardano-node-macos
        jobs.dockerImageArtifact
      ]
    ]));

in jobs
