# our packages overlay
final: prev: with final; {

  last-index-state = lib.last (builtins.attrNames (import haskell-nix.indexStateHashesPath));

  inherit (cardanoNodeProject.args) compiler-nix-name;

  # The is used by nix/regenerate.sh to pre-compute package list to avoid double evaluation.
  genProjectPackages = lib.genAttrs
    (lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
      cardanoNodeProject.hsPkgs))
    (name: lib.attrNames cardanoNodeProject.pkg-set.options.packages.value.${name}.components.exes);

  cabal = haskell-nix.cabal-install.${compiler-nix-name};

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    inherit (cardanoNodeProject) index-state;
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    inherit (cardanoNodeProject) index-state;
  };

  haskell-language-server = haskell-nix.tool "ghc8107" "haskell-language-server" {
    # latest ghcide does not compile on ghc 8.10.7
    version = {ghc8107 = "1.8.0.0";}.${compiler-nix-name} or "latest";
    # always use last index to be safe for "latest" version.
    # (and be explicit about it to avoid warning)
    index-state = {ghc8107 = "2022-12-11T00:00:00Z";}.${compiler-nix-name} or last-index-state;
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    inherit (cardanoNodeProject) index-state;
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
