{ pkgs
, haskellProject
, backend
## Workbench's long-form ledger era name (`conway`, `babbage`, ...).
, eraName
, profile
, nodeSpecs
, workload
, ...
}:

with pkgs.lib;

let

  # The generator's parameters are the "tx-generator" workload's parameters.
  params = workload.parameters;

  # If there is an "explorer" node, the generator will run there!
  # TODO: Repeated code, add the generator's node name to profile.json
  runningNode = if builtins.hasAttr "explorer" nodeSpecs
    then "explorer"
    else "node-0"
  ;

  nodePublicIP =
    { i, name, ... }@nodeSpec:
    "127.0.0.1";

  # The Plutus redeemer value/content.
  plutus-redeemer =
    if (params.plutus or null) == null || (params.plutus.redeemer or null) == null
    then null
    else params.plutus.redeemer
  ;

  # The Plutus datum value/content.
  plutus-datum =
    if (params.plutus or null) == null || (params.plutus.datum or null) == null
    then null
    else params.plutus.datum
  ;

  # All paths are relative to the workload's run directory
  # ("run/current/workloads/tx-generator").
  finaliseGeneratorService =
    svc: recursiveUpdate svc
      ({
        sigKey              = "../../genesis/utxo-keys/utxo1/utxo.skey";
        runScriptFile       = "run-script.json";
        ## path to the config and socket of the locally running node.
        nodeConfigFile      = "../../${runningNode}/config.json";
        localNodeSocketPath = "../../${runningNode}/node.socket";
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
      } // optionalAttrs profile.node.tracer {
        tracerSocketPath = "../../tracer/tracer.socket";
      # Decide where the executable comes from:
      #########################################
      } // optionalAttrs (!backend.useCabalRun) {
        executable     = "${haskellProject.exes.tx-generator}/bin/tx-generator";
      } // optionalAttrs   backend.useCabalRun  {
        executable     = "tx-generator";
      #########################################
      });

  ##
  ## generatorServiceConfig :: Map NodeId NodeSpec -> ServiceConfig
  ##
  generatorServiceConfig =
    nodeSpecs:
        finaliseGeneratorService
        {
          # tx-generator's NixOS service module accepts the long lowercase
          # ledger era name, which is what the workbench's `eraName` is.
          era = eraName;

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
          (removeAttrs params ["epochs"]));

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
          warnings = mkOption {};
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

  service = generatorServiceConfigService (generatorServiceConfig nodeSpecs);

in {

  start =
    ''
    #!${pkgs.stdenv.shell}

    # The entrypoint function.
    function tx_generator() {
      # The generator runs on the same machine as the "${runningNode}" node.
      if ! test -d "../../${runningNode}"
      then
        echo "tx-generator: no locally deployed \"${runningNode}\" node, nothing to do"
        exit 0
      fi

      ${service.script}
    }
    ''
  ;

  # The tx-generator's config file ("run-script.json"), materialized by the
  # backend in the workload's run directory.
  config = service.decideRunScript service;

  # Not present on every profile.
  # Don't create a derivation to a file containing "null" !!!
  # The corresponding file is created/deployed by the workbench.
  inherit plutus-redeemer plutus-datum;

}
