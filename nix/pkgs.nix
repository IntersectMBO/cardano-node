# our packages overlay
final: prev: with final; {

  inherit (cardanoNodeProject.args) compiler-nix-name;

  # The is used by nix/regenerate.sh to pre-compute package list to avoid double evaluation.
  genProjectPackages = lib.genAttrs
    (lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
      cardanoNodeProject.hsPkgs))
    (name: lib.attrNames cardanoNodeProject.pkg-set.options.packages.value.${name}.components.exes);

  cabal = haskell-nix.cabal-install.${compiler-nix-name};

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    index-state = "2022-09-27T00:00:00Z";
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    index-state = "2022-09-27T00:00:00Z";
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" {
    version = "1.8.0.0";
    index-state = "2023-03-13T00:00:00Z";
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    index-state = "2022-09-27T00:00:00Z";
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

  supervisord-workbench-cabal =
    { workbench ? pkgs.workbench, ... }@args: pkgs.callPackage ./workbench/supervisor.nix (args // { useCabalRun = true; });
  supervisord-workbench-nix =
    { workbench ? pkgs.workbench, ... }@args: pkgs.callPackage ./workbench/supervisor.nix args;

  all-profiles-json = (pkgs.callPackage ./workbench/supervisor.nix {}).all-profiles.JSON;

  # An instance of the workbench, specialised to the supervisord backend and a profile,
  # that can be used with nix-shell or lorri.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  supervisord-workbench-for-profile =
    { batchName             ? customConfig.localCluster.batchName
    , profileName           ? customConfig.localCluster.profileName
    , useCabalRun           ? false
    , workbenchDevMode      ? false
    , profiled              ? false
    , supervisord-workbench ? pkgs.callPackage ./workbench/supervisor.nix { inherit useCabalRun; }
    }:
    pkgs.callPackage ./workbench/supervisor-run.nix
      {
        inherit batchName profileName supervisord-workbench;
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
