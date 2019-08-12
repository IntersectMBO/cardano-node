{ pkgs, environments }:
with pkgs.lib;
let 
  pkgsModule = {
    config._module.args.pkgs = mkDefault pkgs;
  };
  mkConnectScript = env: with environments."${env}";
    let 
      nodeConf = { config.services.cardano-node = {
        genesis-file = genesisFile;
        port = 7776;
        topology = ../../../configuration/topology-proxy-follower.json;
      };};
      script = (modules.evalModules {
        modules = [ 
          ../../nixos/cardano-node-options.nix
          nodeConf
          pkgsModule
        ];
      }).config.services.cardano-node.script;
    in
      pkgs.writeScript "cardano-node-${env}" ''
        #!${pkgs.runtimeShell}
        set -euo pipefail

        mkdir -p ${env}-db
        cd ${env}-db
        
        ${script} $@
      '';
in with builtins; listToAttrs (map
  (e: {name = e; value = mkConnectScript e;})
  (attrNames environments)
)
