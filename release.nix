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

# Build for linux
, linuxBuild ? builtins.elem "x86_64-linux" supportedSystems

# Build for macos
, macosBuild ? builtins.elem "x86_64-darwin" supportedSystems

# Cross compilation to Windows is currently only supported on linux.
, windowsBuild ? builtins.elem "x86_64-linux" supportedCrossSystems

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:
with pkgs.lib;

let
  linuxRelease = (import pkgs.iohkNix.release-lib) {
    inherit pkgs;
    supportedSystems = [ "x86_64-linux" ];
    supportedCrossSystems = filter (s: s == "x86_64-linux" && windowsBuild) supportedCrossSystems;
    inherit scrubJobs projectArgs;
    packageSet = import cardano-node;
    gitrev = cardano-node.rev;
  };
  macosRelease = (import pkgs.iohkNix.release-lib) {
    inherit pkgs;
    supportedSystems = [ "x86_64-darwin" ];
    supportedCrossSystems = filter (s: s == "x86_64-darwin" && windowsBuild) supportedCrossSystems;
    inherit scrubJobs projectArgs;
    packageSet = import cardano-node;
    gitrev = cardano-node.rev;
  };
  windowsRelease = if (elem "x86_64-linux" supportedCrossSystems) then linuxRelease else macosRelease;
  archs = filterAttrs (n: _: elem n supportedSystems) {
    x86_64-linux = linuxRelease.pkgsFor "x86_64-linux";
    x86_64-darwin = macosRelease.pkgsFor "x86_64-darwin";
  };
  makeScripts = cluster: let
    getScript = name: optionalAttrs linuxBuild {
      x86_64-linux = archs.x86_64-linux.scripts.${cluster}.${name};
    } // (optionalAttrs macosBuild {
      x86_64-darwin = archs.x86_64-darwin.scripts.${cluster}.${name};
    });
  in {
    node = getScript "node";
  };
  dockerImageArtifact = let
    image = archs.${builtins.head supportedSystems}.dockerImage;
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
    ${concatMapStringsSep "\n" (input: "ln -sv ${input.value} ${input.key}") (attrValues (mapAttrs (key: value: { inherit key value; }) inputs))}
  '';
  makeRelease = cluster: {
    name = cluster;
    value = {
      scripts = makeScripts cluster;
    };
  };

  # Environments we want to build scripts for on hydra
  environments = [ "mainnet" "testnet" "staging" "shelley_qa" "launchpad" "allegra" ];

  extraBuilds = {
    # Environments listed in Network Configuration page
    cardano-deployment = pkgs.iohkNix.cardanoLib.mkConfigHtml { inherit (pkgs.iohkNix.cardanoLib.environments) mainnet testnet launchpad allegra; };
  } // (builtins.listToAttrs (map makeRelease environments));

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
  onlyBuildOnDefaultSystem = [
    ["checks" "hlint"] ["dockerImage"] ["clusterTests"] ["nixosTests"]
  ];
  # Paths or prefix of paths for which cross-builds (mingwW64, musl64) are disabled:
  noCrossBuild = [
    ["shell"] ["cardano-ping"] ["roots"]
  ] ++ onlyBuildOnDefaultSystem;
  noMusl64Build = [ ["checks"] ["tests"] ["benchmarks"] ["haskellPackages"] ["plan-nix"] ]
    ++ noCrossBuild;

  # Remove build jobs for which cross compiling does not make sense.
  filterProject = noBuildList: project: mapAttrsRecursiveCond (a: !(isDerivation a)) (path: value:
    if (isDerivation value && (any (p: take (length p) path == p) noBuildList)) then null
    else value
  ) project;

  inherit (systems.examples) mingwW64 musl64;

  inherit (pkgs.commonLib) sources nixpkgs;

  jobs = {
    inherit dockerImageArtifact;
    ifd-pins = mkPins {
      inherit (sources) iohk-nix "haskell.nix";
      inherit nixpkgs;
      inherit (pkgs.haskell-nix) hackageSrc stackageSrc;
    };
  } // (optionalAttrs linuxBuild (with linuxRelease; {
    linux =
      let filteredBuilds = mapAttrsRecursiveCond (a: !(isList a)) (path: value:
        if (any (p: take (length p) path == p) onlyBuildOnDefaultSystem) then filter (s: !(elem s nonDefaultBuildSystems)) value else value)
        (packagePlatforms project);
      in (mapTestOn (__trace (__toJSON filteredBuilds) filteredBuilds)) // {
        # linux static builds:
        musl64 = mapTestOnCross musl64 (packagePlatformsCross (filterProject noMusl64Build project));
      };
    cardano-node-linux = import ./nix/binary-release.nix {
      inherit (linuxRelease) pkgs project;
      platform = "linux";
      exes = collectJobs jobs.linux.musl64.exes;
    };
  })) // (optionalAttrs macosBuild (with macosRelease; {
    macos =
      let filteredBuilds = mapAttrsRecursiveCond (a: !(isList a)) (path: value:
        if (any (p: take (length p) path == p) onlyBuildOnDefaultSystem) then filter (s: !(elem s nonDefaultBuildSystems)) value else value)
        (packagePlatforms project);
      in (mapTestOn (__trace (__toJSON filteredBuilds) filteredBuilds));
    cardano-node-macos = import ./nix/binary-release.nix {
      inherit pkgs project;
      platform = "macos";
      exes = collectJobs jobs.macos.exes;
    };
  })) // (optionalAttrs windowsBuild (with windowsRelease; {
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross (filterProject noCrossBuild project));
    cardano-node-win64 = import ./nix/binary-release.nix {
      inherit pkgs project;
      platform = "win64";
      exes = collectJobs jobs.${mingwW64.config}.exes;
    };
  })) // extraBuilds // (linuxRelease.mkRequiredJob (concatLists [
    # Linux builds:
    (optionals linuxBuild (concatLists [
      (collectJobs jobs.linux.checks)
      (collectJobs jobs.linux.nixosTests)
      (collectJobs jobs.linux.benchmarks)
      (collectJobs jobs.linux.exes)
      [ jobs.cardano-node-linux ]
    ]))
    # macOS builds:
    (optionals macosBuild (concatLists [
      (collectJobs jobs.macos.checks)
      (collectJobs jobs.macos.nixosTests)
      (collectJobs jobs.macos.benchmarks)
      (collectJobs jobs.macos.exes)
      [ jobs.cardano-node-macos ]
    ]))
    # Windows builds:
    (optional windowsBuild jobs.cardano-node-win64)
    (optionals windowsBuild (collectJobs jobs.${mingwW64.config}.checks))
    # Default system builds (linux on hydra):
    (map (cluster: collectJobs jobs.${cluster}.scripts.node.${head supportedSystems}) environments)
    [ jobs.dockerImageArtifact ]
  ]));

in jobs
