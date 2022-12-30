{ pkgs
, runJq

## An attrset of specific methods and parameters.
, services-config

, profile
}:

with pkgs.lib;

let
  # We're reusing configuration from a cluster node.
  exemplarNode = profile.node-services."node-0";

  ##
  ## generatorServiceConfig :: Map NodeId NodeSpec -> ServiceConfig
  ##
  generatorServiceConfig =
    nodeSpecs:
    let
      generatorNodeConfigDefault =
        (__fromJSON (__readFile ../../../bench/tx-generator-config-base.json))
        // { inherit (exemplarNode.nodeConfig.value)
               Protocol
               ShelleyGenesisFile ByronGenesisFile;
           };
    in
        services-config.finaliseGeneratorService profile.value
        {
          inherit (profile.value) era;

          targetNodes = __mapAttrs
            (name: { name, port, ...}@nodeSpec:
              { inherit port;
                ip = let ip = services-config.nodePublicIP nodeSpec; # getPublicIp resources nodes name
                     in __trace "generator target:  ${name}/${ip}:${toString port}" ip;
              })
            nodeSpecs;

          ## nodeConfig of the locally running node.
          localNodeConf = removeAttrs exemplarNode.serviceConfig.value ["executable"];

          ## The nodeConfig of the Tx generator itself.
          nodeConfig = services-config.finaliseGeneratorConfig generatorNodeConfigDefault;

          dsmPassthrough = {
            # rtsOpts = ["-xc"];
          };
        }
        //
        ((x: recursiveUpdate x
          { tx_count = __ceil x.tx_count; })
          (removeAttrs profile.value.generator ["epochs"]));

  ## Given an env config, evaluate it and produce the node service.
  ## Call the given function on this service.
  ##
  ## generatorServiceConfigService :: GeneratorServiceConfig -> GeneratorService
  ##
  generatorServiceConfigService =
    serviceConfig:
    let
    systemdCompat.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      users = mkOption {};
      assertions = mkOption {};
    };
    eval = let
      extra = {
        services.tx-generator = {
          enable = true;
        } // serviceConfig;
      };
    in evalModules {
      prefix = [];
      modules = import ../../nixos/module-list.nix ++ [
        (import ../../nixos/tx-generator-service.nix pkgs)
        systemdCompat extra
      ];
      args = { inherit pkgs; };
    };
    in eval.config.services.tx-generator;

  ##
  ## generator-service :: (ServiceConfig, Service, NodeConfig, Script)
  ##
  generator-service =
    (nodeSpecs:
    let
      serviceConfig = generatorServiceConfig nodeSpecs;
      service       = generatorServiceConfigService serviceConfig;
    in {
      serviceConfig = {
        value = serviceConfig;
        JSON  = runJq "generator-service-config.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON serviceConfig}'
                  '' "$x";
      };

      service = {
        value = service;
        JSON  = runJq "generator-service.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON service}'
                  '' "$x";
      };

      nodeConfig = {
        value = service.nodeConfig;
        JSON  = runJq "generator-config.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON service.nodeConfig}'
                  '' "$x";
      };

      runScript = {
        # TODO / FIXME
        # the string '...' is not allowed to refer to a store path (such as '')
        # value = service.decideRunScript service;
        JSON  = runJq "generator-run-script.json"
                  ''--null-input
                    --argjson x '${service.decideRunScript service}'
                  '' "$x";
      };

      startupScript = rec {
        JSON = pkgs.writeScript "startup-generator.sh" value;
        value = ''
          #!${pkgs.stdenv.shell}

          ${service.script}
          '';
      };
    })
    profile.node-specs.value;
in
{
  inherit generator-service mkGeneratorScript;
}
