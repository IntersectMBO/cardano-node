# our packages overlay
final: prev:

let
  inherit (prev) customConfig;
  inherit (final) pkgs cardanoNodePackages cardanoNodeProject;
  inherit (prev.pkgs) lib;

  # A generic, fully parameteric version of the workbench development environment.
  workbench = pkgs.callPackage ./workbench {};

  # A conveniently-parametrisable workbench preset.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # The general idea is:
  # 1. backendName -> stateDir -> basePort -> useCabalRun -> backend
  # 2. batchName -> profileName -> profiling -> backend -> workbench -> runner
  # * `workbench` is in case a pinned version of the workbench is needed.
  workbench-runner =
  let
    backendRegistry =
      {
        nomadcloud      = params:
          import ./workbench/backend/nomad/cloud.nix  params;
        nomadexec       = params:
          import ./workbench/backend/nomad/exec.nix   params;
        nomadpodman     = params:
          import ./workbench/backend/nomad/podman.nix params;
        supervisor      = params:
          import ./workbench/backend/supervisor.nix   params;
      }
    ;
  in
    { stateDir           ? customConfig.localCluster.stateDir
    , batchName          ? customConfig.localCluster.batchName
    , profileName        ? customConfig.localCluster.profileName
    , backendName        ? customConfig.localCluster.backendName
    , basePort           ? customConfig.localCluster.basePort
    , useCabalRun        ? customConfig.localCluster.useCabalRun
    , workbenchDevMode   ? customConfig.localCluster.workbenchDevMode
    , workbenchStartArgs ? customConfig.localCluster.workbenchStartArgs
    , profiling          ? customConfig.profiling
    , cardano-node-rev   ? null
    , workbench          ? pkgs.workbench
    }:
    let
        # The `useCabalRun` flag is set in the backend to allow the backend to
        # override its value. The runner uses the value of `useCabalRun` from
        # the backend to prevent a runner using a different value.
        backend = (backendRegistry."${backendName}")
                   { inherit pkgs lib stateDir basePort useCabalRun; };
    in import ./workbench/backend/runner.nix
      {
        inherit pkgs lib cardanoNodePackages;
        inherit batchName profileName backend;
        inherit cardano-node-rev;
        inherit workbench workbenchDevMode workbenchStartArgs profiling;
      };

  # Workbench instantiated by parameters from customConfig:
  custom-config-workbench-runner = workbench-runner {};

in with final;
{
  inherit (cardanoNodeProject.args) compiler-nix-name;
  inherit workbench workbench-runner;

  cabal = haskell-nix.cabal-install.${compiler-nix-name};

  # TODO Use `compiler-nix-name` here instead of `"ghc928"`
  # and fix the resulting `hlint` 3.6.1 warnings.
  hlint = haskell-nix.tool "ghc928" "hlint" ({config, ...}: {
    version = {
      ghc8107 = "3.4.1";
      ghc927 = "3.5";
      ghc928 = "3.5";
    }.${config.compiler-nix-name} or "3.6.1";
    index-state = "2023-08-05T00:00:00Z";
  });

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    index-state = "2023-08-05T00:00:00Z";
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" rec {
    src = {
      ghc8107 = haskell-nix.sources."hls-2.2";
    }.${compiler-nix-name} or haskell-nix.sources."hls-2.3";
    cabalProject = builtins.readFile (src + "/cabal.project");
    sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    index-state = "2023-08-05T00:00:00Z";
  };

  profiteur = haskell-nix.tool compiler-nix-name "profiteur" {
    cabalProjectLocal = ''
      allow-newer: pofiteur:base, ghc-prof:base
    '';
  };

  cabal-plan = haskell-nix.tool compiler-nix-name "cabal-plan" {
  };

  cardanolib-py = callPackage ./cardanolib-py { };

  scripts = lib.recursiveUpdate (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; });

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
        socketPath = "/node-ipc/node.socket";
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

  all-profiles-json = workbench.profile-names-json;

  # Disable failing python uvloop tests
  python38 = prev.python38.override {
    packageOverrides = pythonFinal: pythonPrev: {
      uvloop = pythonPrev.uvloop.overrideAttrs (attrs: {
        disabledTestPaths = [ "tests/test_tcp.py" "tests/test_sourcecode.py" "tests/test_dns.py" ];
      });
    };
  };
} //
custom-config-workbench-runner.overlay final prev
