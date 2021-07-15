{ pkgs
, runJq

## The backend is an attrset of AWS/supervisord-specific methods and parameters.
, backend

## Environmental settings:
##   - either affect semantics on all backends equally,
##   - or have no semantic effect
, environment

, profile
}:

with pkgs.lib;

let
  # We're reusing configuration from a cluster node.
  exemplarNode = profile.node-services."node-0";

  ##
  ## mkGeneratorRunScript :: ServiceConfig -> GeneratorRunScript
  ##
  mkGeneratorRunScript =
    cfg: with cfg;
    [
      { setNumberOfInputsPerTx   = 2; } ## XXX: inputs_per_tx
      { setNumberOfOutputsPerTx  = 2; } ## XXX: outputs_per_tx
      { setNumberOfTxs           = tx_count; }
      { setTxAdditionalSize      = 0; } ## XXX: add_tx_size
      { setFee                   = tx_fee; }
      { setTTL                   = 1000000; }
      { startProtocol            = nodeConfigFile; }
      { setEra                   = "Mary"; } ## XXX: era
      { setTargets =
           __attrValues
             (__mapAttrs (name: { ip, port }:
                            { addr = ip; port = port; })
                         targetNodes);
      }
      { setLocalSocket    = localNodeSocketPath; }
      { readSigningKey    = "pass-partout"; filePath = sigKey; }
      { importGenesisFund = "pass-partout"; fundKey  = "pass-partout"; }
      { delay             = init_cooldown; }
      { createChange      = 1000; count = tx_count * 2; }
      { runBenchmark      = "walletBasedBenchmark";
                               txCount = tx_count; tps = tps; }
      { waitBenchmark     = "walletBasedBenchmark"; }
    ];

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
      configValue =
        backend.finaliseGeneratorService
        {
          era = profile.value.era;

          targetNodes = __mapAttrs
            (name: { name, port, ...}@nodeSpec:
              { inherit port;
                ip = let ip = backend.nodePublicIP nodeSpec; # getPublicIp resources nodes name
                     in __trace "generator target:  ${name}/${ip}:${toString port}" ip;
              })
            nodeSpecs;

          ## path to the socket of the locally running node.
          localNodeSocketPath = "../node-0/node.socket";

          ## nodeConfig of the locally running node.
          localNodeConf = exemplarNode.serviceConfig.value;

          ## The nodeConfig of the Tx generator itself.
          nodeConfig =
            backend.finaliseGeneratorConfig
            (recursiveUpdate generatorNodeConfigDefault
            {
              minSeverity = "Debug";
              TracingVerbosity = "MaximalVerbosity";
              defaultScribes = [
                [ "StdoutSK" "stdout" ]
                [ "FileSK"   "logs/generator.json" ]
              ];
              setupScribes = [
                { scKind = "StdoutSK"; scName = "stdout"; scFormat = "ScJson"; }
                { scKind = "FileSK"; scName = "logs/generator.json"; scFormat = "ScJson";
                  scRotation = {
                    rpLogLimitBytes = 300000000;
                    rpMaxAgeHours   = 24;
                    rpKeepFilesNum  = 20;
                  }; }
              ];
            });

          dsmPassthrough = {
            # rtsOpts = ["-xc"];
          };
        } // profile.value.generator;
    in
      configValue //
      { runScript = mkGeneratorRunScript configValue;
      };

  ## Given an env config, evaluate it and produce the node service.
  ## Call the given function on this service.
  ##
  ## generatorServiceConfigServiced :: GeneratorServiceConfig -> GeneratorService
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
      modules = import ../../nixos/module-list.nix ++ [ systemdCompat extra ];
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
        value = service.runScript;
        JSON  = runJq "generator-run-script.json"
                  ''--null-input
                    --argjson x '${__toJSON service.runScript}'
                  '' "$x";
      };

      startupScript =
        pkgs.writeScript "startup-generator.sh"
          ''
          #!${pkgs.stdenv.shell}

          ${service.script}
          '';
    })
    profile.node-specs.value;
in
{
  inherit generator-service mkGeneratorScript;
}
