pkgs:
{ name, ... }:
with pkgs;
let
  boolOption = lib.mkOption {
    type = lib.types.bool;
    default = false;
  };

  runDir = commonLib.envOrDefault "WB_RUN_DIR" "run/current";

  nodeConfig = runDir + "/config.json";
in {
  imports = [
    iohk-ops-lib.modules.common
  ];

  options = {
    node = {
      coreIndex = lib.mkOption {
        type = lib.types.int;
      };
      nodeId = lib.mkOption {
        type = lib.types.int;
      };
      roles = {
        isByronProxy = boolOption;
        isCardanoPool = boolOption;
        isCardanoRelay = boolOption;
        isSnapshots = boolOption;
        isCustom = boolOption;
        isExplorer = boolOption;
        isExplorerBackend = boolOption;
        isFaucet = boolOption;
        isMonitor = boolOption;
        isMetadata = boolOption;
        isPublicSsh = boolOption;
      };
    };
  };

  config = {
    deployment.keys =
      let mkConfig = key:
            builtins.trace ("${name}: using " + (toString key)) {
              # keyFile = key;
              text    = __readFile key;
              user    = "cardano-node";
              group   = "cardano-node";
              destDir = "/var/lib/cardano-node";
            };
      in {
        "config.json"   = mkConfig (runDir + "/${name}/config.json");
        "topology.json" = mkConfig (runDir + "/${name}/topology.json");
      };
  };

}
