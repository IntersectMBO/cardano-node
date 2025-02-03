{ pkgs

, backend
, profile
, nodeSpecs
, node-services
}:

with pkgs.lib;

let
  # If there is an "explorer" node, the generator will run there!
  # TODO: Repeated code, add the generator's node name to profile.json
  runningNode = if builtins.hasAttr "explorer" nodeSpecs
    then "explorer"
    else "node-0"
  ;

  # We're reusing configuration from a cluster node.
  exemplarNode = node-services."node-0";

  nodePublicIP =
    { i, name, ... }@nodeSpec:
    "127.0.0.1";

  finaliseGeneratorService =
    profile: svc: recursiveUpdate svc
      ({
        sigKey              = "../genesis/utxo-keys/utxo1.skey";
        runScriptFile       = "run-script.json";
        ## path to the config and socket of the locally running node.
        nodeConfigFile      = "../${runningNode}/config.json";
        localNodeSocketPath = "../${runningNode}/node.socket";
        ## Relative paths to use for the Plutus redeemer and datum properties.
        ## These two properties override the default that is "plutus.redeemer"
        ## and "plutus.datum" being file paths to the Nix Store that may or may
        ## exist depending on the workbench's backend requested.
        plutusRedeemerFile  = "plutus-redeemer.json";
        plutusDatumFile     = "plutus-datum.json";
      } // optionalAttrs profile.node.tracer {
        tracerSocketPath = "../tracer/tracer.socket";
      } // optionalAttrs backend.useCabalRun {
        executable     = "tx-generator";
      });

  ##
  ## generatorServiceConfig :: Map NodeId NodeSpec -> ServiceConfig
  ##
  generatorServiceConfig =
    nodeSpecs:
    let
      generatorNodeConfigDefault =
        (__fromJSON (__readFile ../../../bench/tx-generator-config-base.json))
        // { inherit (exemplarNode.config.value)
               Protocol
               ByronGenesisFile
               ShelleyGenesisFile
               AlonzoGenesisFile
               ConwayGenesisFile
               ;
           };
    in
        finaliseGeneratorService profile
        {
          inherit (profile) era;

          targetNodes = __mapAttrs
            (name: { name, port, ...}@nodeSpec:
              { inherit name port;
                # "generator target ${name}: ${ip}:${toString port}"
                ip = nodePublicIP nodeSpec; # getPublicIp resources nodes name
              })
            (filterAttrs (_: spec: spec.isProducer) nodeSpecs);

          dsmPassthrough = {
            # rtsOpts = ["-xc"];
          };
        }
        //
        ((x: recursiveUpdate x
          { tx_count = __ceil x.tx_count; })
          (removeAttrs profile.generator ["epochs"]));

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
          environment = mkOption {};
        };
        eval =
          let
            extra = {
              services.tx-generator = {enable = true;} // serviceConfig;
            };
          in evalModules {
            prefix = [];
            modules =    import ../../nixos/module-list.nix
                      ++ [
                            (import ../../nixos/tx-generator-service.nix pkgs)
                              systemdCompat extra
                              {config._module.args = {inherit pkgs;};}
                         ]
                      ++ [ backend.service-modules.generator or {} ]
                      ;
            # args = { inherit pkgs; };
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
      start = rec {
        value = ''
          #!${pkgs.stdenv.shell}

          ###########################################
          # Extra workloads start ###################
          ###########################################
          ${builtins.concatStringsSep "" (builtins.map (workload:
              let workload_name = workload.name;
                  entrypoint = workload.entrypoints.pre_generator;
                  node_name = if profile.composition.with_explorer
                              then "explorer"
                              else "node-0"
                  ;
              in
                  ''
                  ###########################################
                  ########## workload start: ${workload_name}
                  ###########################################
                  ${if entrypoint != null
                    then
                      ''
                      ${import ../workload/${workload_name}.nix
                        {inherit pkgs profile nodeSpecs workload;}
                      }
                      (cd ../workloads/${workload_name} && ${entrypoint} ${node_name})
                      ''
                    else
                      ''
                      ''
                  }
                  ###########################################
                  ########## workload end:   ${workload_name}
                  ###########################################
                  ''
            ) (profile.workloads or []))
          }
          #############################################
          # Extra workloads end #######################
          #############################################

          ${service.script}
          '';
        JSON = pkgs.writeScript "startup-generator.sh" value;
      };

      config = rec {
        value = service.decideRunScript service;
        JSON  = pkgs.writeScript
                  "generator-run-script.json"
                  (__toJSON value)
                ;
      };

      # The Plutus redeemer file is handled as an extra service file to deploy.
      plutus-redeemer =
        # Not present on every profile.
        # Don't create a derivation to a file containing "null" !!!
        if serviceConfig.plutus == null || (serviceConfig.plutus.redeemer or null) == null
        then {}
        else rec {
               value = serviceConfig.plutus.redeemer;
               JSON = pkgs.writeScript "plutus-redeemer.json" (__toJSON value);
             }
      ;

       # The Plutus datum file is handled as an extra service file to deploy.
      plutus-datum =
        # Not present on every profile.
        # Don't create a derivation to a file containing "null" !!!
        if serviceConfig.plutus == null || (serviceConfig.plutus.datum or null) == null
        then {}
        else rec {
               value = serviceConfig.plutus.datum;
               JSON = pkgs.writeScript "plutus-datum.json" (__toJSON value);
             }
      ;
    })
    nodeSpecs;
in
{
  inherit generator-service;
}
