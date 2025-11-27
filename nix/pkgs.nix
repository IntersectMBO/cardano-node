# Our packages overlay
final: prev:

let
  inherit (builtins) foldl' fromJSON listToAttrs map readFile;
  inherit (final) pkgs;
  inherit (prev.pkgs) lib;
  inherit (prev) customConfig;
  # Workbench development environment entrypoint parametrized with current pkgs.
  workbench = import ./workbench
    {inherit pkgs lib; inherit (final) cardanoNodePackages cardanoNodeProject;};

in with final;
{
  inherit (cardanoNodeProject.args) compiler-nix-name;

  # To make it a flake output so it's available as input to external flakes.
  inherit workbench;

  # A workbench runner with default parameters from customConfig.
  # Used in flake.nix for "workbench-ci-test" flake output package for CI.
  workbench-runner =
    { profileName        ? customConfig.localCluster.profileName
    , backendName        ? customConfig.localCluster.backendName
    , stateDir           ? customConfig.localCluster.stateDir
    , basePort           ? customConfig.localCluster.basePort
    , useCabalRun        ? customConfig.localCluster.useCabalRun
    , profiling          ? customConfig.profiling
    , batchName          ? customConfig.localCluster.batchName
    , workbenchStartArgs ? customConfig.localCluster.workbenchStartArgs
    , cardano-node-rev   ? null
    }:
    workbench.runner
      { # To construct profile attrset with its `materialise-profile` function.
        inherit profileName;
        # To construct backend attrset with its `materialise-profile` function.
        inherit backendName stateDir basePort useCabalRun profiling;
        # Parameters for the workbench shell `start-cluster` command.
        inherit batchName workbenchStartArgs cardano-node-rev;
      }
  ;

  cabal = haskell-nix.cabal-install.${compiler-nix-name};

  hlint = haskell-nix.tool "ghc96" "hlint" {
    version = "3.8";
    index-state = "2025-04-22T00:00:00Z";
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    index-state = "2024-12-24T12:56:48Z";
  };

  # The ghc-hls point release compatibility table is documented at:
  # https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html
  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" rec {
    src = {
      ghc8107 = haskell-nix.sources."hls-2.2";
      ghc927 = haskell-nix.sources."hls-2.0";
      ghc945 = haskell-nix.sources."hls-2.2";
      ghc946 = haskell-nix.sources."hls-2.2";
      ghc947 = haskell-nix.sources."hls-2.5";
      ghc963 = haskell-nix.sources."hls-2.5";
      ghc964 = haskell-nix.sources."hls-2.6";
      ghc981 = haskell-nix.sources."hls-2.6";
    }.${compiler-nix-name} or haskell-nix.sources."hls-2.10";
    cabalProject = readFile (src + "/cabal.project");
    sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    index-state = "2024-12-24T12:56:48Z";
  };

  profiteur = haskell-nix.tool compiler-nix-name "profiteur" {
    cabalProjectLocal = ''
      allow-newer: profiteur:base, ghc-prof:base
    '';
  };

  cabal-plan = haskell-nix.tool compiler-nix-name "cabal-plan" {
    cabalProjectLocal = ''
      flags: +exe
    '';
  };

  cardanolib-py = callPackage ./cardanolib-py { };

  scripts = foldl' lib.recursiveUpdate {} [
    (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; })
    (import ./scripts-tracer.nix { inherit pkgs; })
  ];

  clusterTests = import ./workbench/tests { inherit pkgs; };

  dockerImage =
    let
      defaultConfig = {
        stateDir = "/data";
        dbPrefix = "db";
        socketPath = "/ipc/node.socket";
      };
    in
    callPackage ./docker {
      exe = "cardano-node";
      scripts = import ./scripts.nix {
        inherit pkgs;
        customConfigs = [ defaultConfig customConfig ];
      };
      script = "node";
    };

  submitApiDockerImage =
    let
      defaultConfig = {
        socketPath = "/ipc/node.socket";
        listenAddress = "0.0.0.0";
      };
    in
    callPackage ./docker/submit-api.nix {
      exe = "cardano-submit-api";
      scripts = import ./scripts-submit-api.nix {
        inherit pkgs;
        customConfigs = [ defaultConfig customConfig ];
      };
      script = "submit-api";
    };

  tracerDockerImage =
    let
      defaultConfig = rec {
        acceptAt = "/ipc/tracer.socket";
        stateDir = "/logs";
        logging = [
          {
            logRoot = stateDir;
            logMode = "FileMode";
            logFormat = "ForHuman";
          }
        ];
      };
    in
    callPackage ./docker/tracer.nix {
      exe = "cardano-tracer";
      scripts = import ./scripts-tracer.nix {
        inherit pkgs;
        customConfigs = [ defaultConfig customConfig ];
      };
      script = "tracer";
    };

  all-profiles-json = workbench.profile-names-json;

  # The profile data and backend data of the cloud / "*-nomadperf" profiles.
  # Useful to mix workbench and cardano-node commits, mostly because of scripts.
  profile-data-nomadperf = listToAttrs (
    map
    (cloudName:
      # Only Conway era cloud profiles are flake outputs.
      let profileName = "${cloudName}-coay";
      in {
        name = profileName;
        value =
          let
              # Default values only ("run/current", 30000, profiling "none").
              profile = workbench.profile profileName;
              backend = workbench.backend
                { backendName = "nomadcloud";
                  stateDir    = customConfig.localCluster.stateDir;
                  basePort    = customConfig.localCluster.basePort;
                  useCabalRun = customConfig.localCluster.useCabalRun;
                  profiling = "none";
                }
              ;
              profileBundle = profile.profileBundle
                { inherit backend; }
              ;
              materialisedProfile = profile.materialise-profile
                { inherit profileBundle; }
              ;
              backendDataDir = backend.materialise-profile
                {inherit profileBundle;}
              ;
          in pkgs.runCommand "workbench-data-${profileName}" {}
            ''
            mkdir "$out"
            ln -s "${materialisedProfile}" "$out"/profileData
            ln -s "${backendDataDir}"      "$out"/backendData
            ''
        ;
        }
    )
    # Fetch all "*-nomadperf" profiles.
    (fromJSON (readFile
      (pkgs.runCommand "cardano-profile-names-cloud-noera" {} ''
        ${cardanoNodePackages.cardano-profile}/bin/cardano-profile names-cloud-noera > $out
      ''
      )
    ))
  );

  # Disable failing python uvloop tests
  python310 = prev.python310.override {
    packageOverrides = pythonFinal: pythonPrev: {
      uvloop = pythonPrev.uvloop.overrideAttrs (attrs: {
        disabledTestPaths = [ "tests/test_tcp.py" "tests/test_sourcecode.py" "tests/test_dns.py" ];
      });
    };
  };
}
