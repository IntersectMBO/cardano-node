{ pkgs
, lib
, stateDir
, basePort
, baseEnvConfig
, nodeSpecs                ## :: Map NodeName NodeSpec
, useCabalRun
}:

with lib;

let
  ## Given an env config, evaluate it and produce the node service.
  ## Call the given function on this service.
  ##
  ## mapNodeService :: (EnvConfig -> Service -> a) -> EnvConfig -> a
  ##
  mapNodeService =
    f: envConfig:
    let
    systemdCompat.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      users = mkOption {};
      assertions = mkOption {};
    };
    eval = let
      extra = {
        services.cardano-node = {
          enable = true;
          inherit (envConfig) topology nodeConfig nodeConfigFile port dbPrefix socketPath;
          inherit stateDir;
        }
        // optionalAttrs (__hasAttr "vrfKey" envConfig)
        { inherit (envConfig) operationalCertificate kesKey vrfKey;
        }
        // optionalAttrs useCabalRun
        { executable = "cabal run exe:cardano-node --";
        };
      };
    in evalModules {
      prefix = [];
      modules = import ../nixos/module-list.nix ++ [ systemdCompat extra ];
      args = { inherit pkgs; };
    };
    in f envConfig eval.config.services.cardano-node;

  ##
  ## nodeSpecEnvConfig :: NodeSpec -> EnvConfig
  ##
  nodeSpecEnvConfig =
    { name, i, kind, port, isProducer }:
      recursiveUpdate
        baseEnvConfig
        ({ nodeConfig =
             {
               hasEKG         =                30100 + i;
               hasPrometheus  = [ "127.0.0.1" (30200 + i) ];
             };
           dbPrefix       = "db-${name}";
           nodeConfigFile = "${stateDir}/${name}.config.json";
           port           = basePort + i;
           socketPath     = "${stateDir}/${name}.socket";
           topology       = "${stateDir}/topologies/${name}.json";
         } // optionalAttrs isProducer {
           operationalCertificate = "${stateDir}/shelley/nodes/${name}/op.cert";
           kesKey                 = "${stateDir}/shelley/nodes/${name}/kes.skey";
           vrfKey                 = "${stateDir}/shelley/nodes/${name}/vrf.skey";
         });

  ##
  ## envConfigNodeSetup :: EnvConfig -> (Script, NodeConfig)
  ##
  envConfigNodeSetup =
    mapNodeService
      (envConfig: nodeService:
        traceValFn
          (x: "envConfigNodeSetup: mapped to attrset with keys: ${toString (attrNames x)}")
        {
          inherit envConfig;
          inherit (nodeService) nodeConfig;
          startupScript =
            pkgs.writeScript "cardano-node"
            ''
            #!${pkgs.stdenv.shell}
            ${nodeService.script}
            '';
        });

  ##
  ## nodeSetups :: Map NodeName (Script, NodeConfig)
  ##
  nodeSetups = mapAttrs
                 (_: spec: envConfigNodeSetup (nodeSpecEnvConfig spec))
                 nodeSpecs;
in
{
  inherit nodeSetups;
}
