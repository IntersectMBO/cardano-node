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

  # The Plutus redeemer value/content.
  plutus-redeemer =
    if profile.generator.plutus == null || (profile.generator.plutus.redeemer or null) == null
    then null
    else profile.generator.plutus.redeemer
  ;

  # The Plutus datum value/content.
  plutus-datum =
    if profile.generator.plutus == null || (profile.generator.plutus.datum or null) == null
    then null
    else profile.generator.plutus.datum
  ;

  finaliseGeneratorService =
    profile: svc: recursiveUpdate svc
      ({
        sigKey              = "../genesis/utxo-keys/utxo1.skey";
        runScriptFile       = "run-script.json";
        ## path to the config and socket of the locally running node.
        nodeConfigFile      = "../${runningNode}/config.json";
        localNodeSocketPath = "../${runningNode}/node.socket";
        ## Relative paths to use for the Plutus redeemer and datum properties.
        ## The workbench backend requested handles the creation of these files.
        plutusRedeemerFile  = if plutus-redeemer != null
                              then "plutus-redeemer.json"
                              else null
        ;
        plutusDatumFile     = if plutus-datum != null
                              then "plutus-datum.json"
                              else null
        ;
      } // optionalAttrs profile.node.trace_forwarding {
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
        // { inherit (exemplarNode.config)
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
      start =
        ''
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
        ''
      ;

      config = (service.decideRunScript service);

      # Not present on every profile.
      # Don't create a derivation to a file containing "null" !!!
      # The corresponding file is created/deployed by the workbench.
      inherit plutus-redeemer plutus-datum;

    })
    nodeSpecs;
in
{
  inherit generator-service;
}
