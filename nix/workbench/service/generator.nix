{ pkgs
, haskellProject
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
      } // optionalAttrs profile.node.tracer {
        tracerSocketPath = "../tracer/tracer.socket";
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

  ##
  ## generator-service :: (ServiceConfig, Service, NodeConfig, Script)
  ##
  generator-service =
    (nodeSpecs:
    let
      serviceConfig = generatorServiceConfig nodeSpecs;
      service       = generatorServiceConfigService serviceConfig;
      genesisFunds =
        (let
           # create-testnet-data --testnet-magic 42 --total-supply 2010000000000000 --utxo-keys 100 --genesis-keys 0 --delegated-supply 2000000000000000 --pools 2 --stake-delegators 2 --drep-keys 0 --stuffed-utxo 000000
           # Ends with 90000000000 each utxo-key.
           # value = (profile.genesis.funds_balance - profile.genesis.shelley.protocolParams.poolDeposit * profile.composition.n_pools) / profile.genesis.utxo_keys;
           value = (profile.derived.supply_total - profile.derived.supply_delegated) * 9 / (profile.genesis.utxo_keys * 10);
         in
__toJSON
          (builtins.genList
            (i:
              { signing_key = "../genesis/utxo-keys/utxo${toString (i+1)}.skey"; # Key index is not zero based =)
                inherit value;
              }
            )
            profile.genesis.utxo_keys
          )
        )
      ;
      txCentrifugeConfig =
        { # pull-fiction parameters.
          ##########################
          initial_inputs =
            { type = "genesis_utxo_keys";
              params =
                { network_magic = profile.genesis.network_magic;
                  signing_keys_file = "./funds.json";
                }
              ;
            }
          ;
          observers =
            { local-follower =
                { type = "nodetoclient";
                  params =
                    { socket_path = "../${runningNode}/node.socket";
                      confirmation_depth = 2;
                    }
                  ;
                }
              ;
            }
          ;
          builder =
            { type = "value";
              params =
                { inputs_per_tx  = 2;
                  outputs_per_tx = 2;
                  fee = 1000000;
                }
              ;
              recycle = {type = "on_confirm"; params = "local-follower";};
            }
          ;
          rate_limit =
            { scope = "shared";
              type = "token_bucket";
              params = { tps = 12; };
            }
          ;
          max_batch_size = 500;
          on_exhaustion = "error";
          # One node per-workload.
          workloads =
            builtins.listToAttrs
              (builtins.genList
                (i:
                  { name = "node-${toString i}";
                    value =
                      { targets =
                          { "${toString i}" =
                            #  { addr = "127.0.0.1";
                            #    port = (30000 + i);
                            #  }
                              { addr = "__addr_${toString i}__";
                                port = "__port_${toString i}__";
                              }
                            ;
                          }
                        ;
                      }
                    ;
                  }
                )
                profile.composition.n_pool_hosts
              )
          ;
          # tx-centrifuge parameters.
          ###########################
          nodeConfig = "../${runningNode}/config.json";
          protocol_parameters =
            { epoch_length = profile.genesis.shelley.epochLength;
              min_fee_a = profile.genesis.shelley.protocolParams.minFeeA;
              min_fee_b = profile.genesis.shelley.protocolParams.minFeeB;
            }
          ;
          # Tracing parameters.
          #####################
          TraceOptions =
            { "" =
              { backends = [ "Stdout MachineFormat" ];
                detail = "DNormal";
                severity = "Debug";
              };
               # ouroboros-network traces.
               "KeepAlive"                           = { severity="Silence";};
               "KeepAlive.Receive.KeepAliveResponse" = { severity="Silence";};
               "KeepAlive.Send.KeepAlive"            = { severity="Silence";};
               "TxSubmission2"                       = { severity="Silence";};
               "TxSubmission2.Receive"               = { severity="Silence";};
               "TxSubmission2.Receive.MsgInit"       = { severity="Silence";};
               "TxSubmission2.Receive.RequestTxIds"  = { severity="Silence";};
               "TxSubmission2.Receive.RequestTxs"    = { severity="Silence";};
               "TxSubmission2.Receive.Done"          = { severity="Silence";};
               "TxSubmission2.Send"                  = { severity="Silence";};
               "TxSubmission2.Send.MsgInit"          = { severity="Silence";};
               "TxSubmission2.Send.ReplyTxIds"       = { severity="Silence";};
               "TxSubmission2.Send.ReplyTxs"         = { severity="Silence";};
               "TxSubmission2.Send.Done"             = { severity="Silence";};
               # tx-centrifuge traces.
               "TxCentrifuge.Builder.NewTx"   = { severity="Debug";detail="DDetailed";};
               "TxCentrifuge.Builder.Recycle" = { severity="Debug";detail="DDetailed";};
               "TxCentrifuge.TxSubmission.RequestTxIds" =
                 { severity="Debug";detail="DDetailed";};
               "TxCentrifuge.TxSubmission.ReplyTxIds"   =
                 { severity="Debug";detail="DDetailed";};
               "TxCentrifuge.TxSubmission.RequestTxs"   =
                 { severity="Debug";detail="DDetailed";};
               "TxCentrifuge.TxSubmission.ReplyTxs"     =
                 { severity="Debug";detail="DDetailed";};
            };
          TurnOnLogMetrics = false;
          TurnOnLogging = true;
          TraceOptionNodeName = "leios-generator";
        }
      ;
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
                      {inherit pkgs haskellProject profile nodeSpecs workload;}
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

        echo ${__toJSON genesisFunds} > ./funds.json
        ${haskellProject.exes.tx-centrifuge}/bin/tx-centrifuge run-script.json
        ''
      ;

      config = txCentrifugeConfig;

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
