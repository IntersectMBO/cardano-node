# our packages overlay
final: prev: with final; {

  inherit (cardanoNodeProject.args) compiler-nix-name;

  cabal = haskell-nix.cabal-install.${compiler-nix-name};

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = {ghc8107 = "3.4.1";}.${compiler-nix-name} or "3.5";
    index-state = "2023-01-20T05:50:56Z";
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    index-state = "2023-01-20T05:50:56Z";
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" {
    # ghcide 1.9.0.0 does not compile on ghc 8.10.7
    version = {ghc8107 = "1.8.0.0";}.${compiler-nix-name} or "1.9.0.0";
    index-state = "2023-01-20T05:50:56Z";
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    index-state = "2023-01-20T05:50:56Z";
  };

  cardanolib-py = callPackage ./cardanolib-py { };

  scripts = lib.recursiveUpdate (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; });

  clusterTests = import ./workbench/tests { inherit pkgs; };

  plutus-scripts = callPackage ./plutus-scripts.nix { plutus-builder = plutus-example; };

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

  # A generic, parameteric version of the workbench development environment.
  workbench = pkgs.callPackage ./workbench {};

  all-profiles-json = workbench.profile-names-json;

  # A parametrisable workbench, that can be used with nix-shell or lorri.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # The general idea is:
  # backendName -> useCabalRun -> backend
  # stateDir -> batchName -> profileName -> backend -> workbench -> runner
  # * `workbench` is in case a pinned version of the workbench is needed.
  workbench-runner =
    let backendRegistry =
        {
            supervisor = ./workbench/backend/supervisor.nix;
            nomad =      ./workbench/backend/nomad.nix;
        };
    in
    { stateDir           ? customConfig.localCluster.stateDir
    , batchName          ? customConfig.localCluster.batchName
    , profileName        ? customConfig.localCluster.profileName
    , backendName        ? customConfig.localCluster.backendName
    , useCabalRun        ? false
    , profiled           ? false
    , cardano-node-rev   ? null
    , workbench          ? pkgs.workbench
    , workbenchDevMode   ? false
    }:
    let
        # The `useCabalRun` flag is set in the backend to allow the backend to
        # override its value. The runner uses the value of `useCabalRun` from
        # the backend to prevent a runner using a different value.
        backend = import (backendRegistry."${backendName}")
                   { inherit pkgs lib useCabalRun; };
    in import ./workbench/backend/runner.nix
      {
        inherit pkgs lib cardanoNodePackages;
        inherit stateDir batchName profileName backend;
        inherit cardano-node-rev;
        inherit workbench workbenchDevMode;
      };

  # Disable failing python uvloop tests
  python38 = prev.python38.override {
    packageOverrides = pythonFinal: pythonPrev: {
      uvloop = pythonPrev.uvloop.overrideAttrs (attrs: {
        disabledTestPaths = [ "tests/test_tcp.py" "tests/test_sourcecode.py" "tests/test_dns.py" ];
      });
    };
  };
}
