pkgs:
profileData:
{ config, name, lib, nodes, resources, ... }:
with pkgs; with pkgs.lib;

let
  # We need a signing key with access to funds
  # to be able to run tx generator and sign generated transactions.
  signingKey = ../keys/utxo-keys/utxo1.skey;

  cardanoNodes = filterAttrs
    (_: node:    node.config.services.cardano-node.enable or false)
    nodes;
  poolNodes    = filterAttrs
    (name: node: name != "explorer" && name != "node-0")
    cardanoNodes;

  node-cfg = config.services.cardano-node;
  mayFetchNodeCfgAttr = attr:
    optionalAttrs (hasAttr attr (node-cfg.nodeConfig)) { ${attr} = node-cfg.nodeConfig.${attr}; };
in {
  _file = ./role-explorer.nix;
  imports = [
    cardano-ops.modules.base-service

    ../../../nixos
  ];

  services.tx-generator =
    recursiveUpdate
      profileData.generator-service.serviceConfig.value
      {
        enable          = true;
      };

  # rec {
  #   enable = true;
  #   targetNodes = __mapAttrs
  #     (name: node:
  #       { ip   = let ip = getPublicIp resources nodes name;
  #                in __trace "generator target:  ${name}/${ip}" ip;
  #         port = node.config.services.cardano-node.port;
  #       })
  #     poolNodes;

  #   ## nodeConfig of the locally running explorer node.
  #   localNodeConf = node-cfg;
  #   localNodeSocketPath = node-cfg.socketPath 0;
  #   sigKey = "/var/lib/keys/cardano-node-signing";

  #   ## The nodeConfig of the Tx generator itself.
  #   nodeConfig =
  #     finaliseNodeConfig
  #       globals.profile.value.node.withNewTracing
  #   {
  #     TurnOnLogging    = true;
  #     TurnOnLogMetrics = false;
  #     minSeverity = "Debug";
  #     TracingVerbosity = "MaximalVerbosity";
  #     defaultBackends = [
  #       "KatipBK"
  #     ];
  #     setupBackends = [
  #       "KatipBK"
  #     ];
  #     defaultScribes = [
  #       [ "StdoutSK" "stdout" ]
  #       [ "FileSK"   "logs/generator.json" ]
  #     ];
  #     setupScribes = [
  #       { scKind = "StdoutSK"; scName = "stdout"; scFormat = "ScJson"; }
  #       { scKind = "FileSK"; scName = "logs/generator.json"; scFormat = "ScJson";
  #         scRotation = {
  #           rpLogLimitBytes = 300000000;
  #           rpMaxAgeHours   = 24;
  #           rpKeepFilesNum  = 20;
  #         }; }
  #     ];
  #     options = {
  #     };
  #   } // __foldl' (x: y: x // y) {}
  #     (map mayFetchNodeCfgAttr
  #       [ "ByronGenesisFile"
  #         "ShelleyGenesisFile"
  #         "AlonzoGenesisFile"
  #         "Protocol"
  #         "LastKnownBlockVersion-Major"
  #         "LastKnownBlockVersion-Minor"
  #         "LastKnownBlockVersion-Alt"
  #         "ExperimentalHardForksEnabled"
  #         "ExperimentalProtocolsEnabled"
  #         "TestShelleyHardForkAtEpoch"
  #         "TestAllegraHardForkAtEpoch"
  #         "TestMaryHardForkAtEpoch"
  #         "TestAlonzoHardForkAtEpoch"
  #         "TestBabbageHardForkAtEpoch" ]);
  #   nodeConfigFile = __toFile "generator-config.json" (__toJSON nodeConfig);

  #   dsmPassthrough = {
  #     # rtsOpts = ["-xc"];
  #   };
  # }
  # // globals.environmentConfig.generatorConfig;

  services.cardano-tracer =
    recursiveUpdate
      profileData.tracer-service.serviceConfig.value
      {
        enable          = true;
        acceptingSocket = (node-cfg.stateDir 0) + "/tracer.socket";
        logRoot = node-cfg.stateDir 0;
      };
  systemd.services.cardano-tracer.serviceConfig.Environment =
    [("HOME=" + node-cfg.stateDir 0)];

  services.cardano-node =
    recursiveUpdate
      profileData.node-services.${name}.serviceConfig.value
      {
        enable         = true;
        stateDir       = "/var/lib/cardano-node";
        nodeConfigFile = "/var/lib/cardano-node/config.json";
        topology       = "/var/lib/cardano-node/topology.json";
        socketPath     = "/var/lib/cardano-node/node.socket";
        systemdSocketActivation = mkForce false;
      };
  # {
  #   nodeConfig =
  #     finaliseNodeConfig
  #       globals.profile.value.node.withNewTracing
  #       (mkForce
  #         (globals.environmentConfig.nodeConfig //
  #          {
  #            defaultScribes = [
  #              [ "StdoutSK" "stdout" ]
  #              [ "FileSK"   "logs/node.json" ]
  #            ];
  #            setupScribes = [
  #              { scKind = "StdoutSK"; scName = "stdout"; scFormat = "ScJson"; }
  #              { scKind = "FileSK"; scName = "logs/node.json"; scFormat = "ScJson";
  #                scRotation = {
  #                  rpLogLimitBytes = 300000000;
  #                  rpMaxAgeHours   = 24;
  #                  rpKeepFilesNum  = 20;
  #                }; }
  #            ];
  #            minSeverity = "Debug";
  #            TracingVerbosity = "NormalVerbosity";

  #            ExperimentalHardForksEnabled = true;
  #            ExperimentalProtocolsEnabled = true;

  #            TraceAcceptPolicy                 = false;
  #            TraceBlockFetchClient             = true;
  #            TraceBlockFetchDecisions          = false;
  #            TraceBlockFetchProtocol           = true;
  #            TraceBlockFetchProtocolSerialised = false;
  #            TraceBlockFetchServer             = false;
  #            TraceBlockchainTime               = false;
  #            TraceChainDB                      = true;
  #            TraceChainSyncBlockServer         = false;
  #            TraceChainSyncClient              = true;
  #            TraceChainSyncHeaderServer        = false;
  #            TraceChainSyncProtocol            = false;
  #            TraceDiffusionInitialization      = false;
  #            TraceDnsResolver                  = false;
  #            TraceDnsSubscription              = false;
  #            TraceErrorPolicy                  = true;
  #            TraceForge                        = false;
  #            TraceForgeStateInfo               = false;
  #            TraceHandshake                    = false;
  #            TraceIpSubscription               = false;
  #            TraceKeepAliveClient              = false;
  #            TraceLocalChainSyncProtocol       = false;
  #            TraceLocalErrorPolicy             = false;
  #            TraceLocalHandshake               = false;
  #            TraceLocalStateQueryProtocol      = false;
  #            TraceLocalTxSubmissionProtocol    = true;
  #            TraceLocalTxSubmissionServer      = true;
  #            TraceMempool                      = true;
  #            TraceTxInbound                    = true;
  #            TraceTxOutbound                   = true;
  #            TraceTxSubmissionProtocol         = true;
  #            TraceTxSubmission2Protocol        = true;

  #            TurnOnLogMetrics = true;
  #            options = {
  #              mapBackends = {
  #                "cardano.node.resources" = [ "KatipBK" ];
  #              };
  #            };
  #          } //
  #         ({
  #           shelley =
  #             { TestShelleyHardForkAtEpoch = 0;
  #             };
  #           allegra =
  #             { TestShelleyHardForkAtEpoch = 0;
  #               TestAllegraHardForkAtEpoch = 0;
  #             };
  #           mary =
  #             { TestShelleyHardForkAtEpoch = 0;
  #               TestAllegraHardForkAtEpoch = 0;
  #               TestMaryHardForkAtEpoch = 0;
  #             };
  #           alonzo =
  #             { TestShelleyHardForkAtEpoch = 0;
  #               TestAllegraHardForkAtEpoch = 0;
  #               TestMaryHardForkAtEpoch = 0;
  #               TestAlonzoHardForkAtEpoch = 0;
  #             };
  #           babbage =
  #             { TestShelleyHardForkAtEpoch = 0;
  #               TestAllegraHardForkAtEpoch = 0;
  #               TestMaryHardForkAtEpoch = 0;
  #               TestAlonzoHardForkAtEpoch = 0;
  #               TestBabbageHardForkAtEpoch = 0;
  #             };
  #         }).${globals.profile.value.era}
  #         // (globals.profile.value.node.extra_config or {})
  #         ))
  # }

  deployment.keys = {
    "cardano-node-signing" = {
        keyFile = signingKey;
        user = "cardano-node";
        group = "cardano-node";
        destDir = "/var/lib/keys";
    };
  };

  users.users.cardano-node.extraGroups = [ "keys" ];
}
