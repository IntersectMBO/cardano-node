# our packages overlay
final: prev: with final; {

  inherit (cardanoNodeProject.args) compiler-nix-name;

  # The is used by nix/regenerate.sh to pre-compute package list to avoid double evaluation.
  genProjectPackages = lib.genAttrs
    (lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
      cardanoNodeProject.hsPkgs))
    (name: lib.attrNames cardanoNodeProject.pkg-set.options.packages.value.${name}.components.exes);

  cabal = haskell-nix.tool compiler-nix-name "cabal" {
    version = "latest";
    inherit (cardanoNodeProject) index-state;
  };

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    inherit (cardanoNodeProject) index-state;
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    inherit (cardanoNodeProject) index-state;
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" {
    version = "latest";
    inherit (cardanoNodeProject) index-state;
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    inherit (cardanoNodeProject) index-state;
  };

  cardanolib-py = callPackage ./cardanolib-py { };

  scripts = lib.recursiveUpdate (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; });

  clusterTests = import ./supervisord-cluster/tests { inherit pkgs; };

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

  # This provides a supervisord-backed instance of a the workbench development environment
  # that can be used with nix-shell or lorri.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  workbench-supervisord =
    { useCabalRun, profileName ? customConfig.localCluster.profileName }:
    pkgs.callPackages ./supervisord-cluster
      {
        inherit profileName useCabalRun;
        workbench = pkgs.callPackage ./workbench { inherit useCabalRun; };
      };

  clusterCabal = pkgs.workbench-supervisord {
    useCabalRun = true;
  };
  clusterNix = pkgs.workbench-supervisord { useCabalRun = false; };

  # Disable failing python uvloop tests
  python38 = prev.python38.override {
    packageOverrides = pythonFinal: pythonPrev: {
      uvloop = pythonPrev.uvloop.overrideAttrs (attrs: {
        disabledTestPaths = [ "tests/test_tcp.py" "tests/test_sourcecode.py" "tests/test_dns.py" ];
      });
    };
  };
}
