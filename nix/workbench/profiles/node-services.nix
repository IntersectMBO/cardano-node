{ pkgs
, runJq

## An attrset of specific methods and parameters.
, services-config

## The cardano-node config used as baseline:
, baseNodeConfig

, profile
}:

with pkgs.lib;
with (import ../lib.nix pkgs.lib);

let
  ##
  ## removeLegacyTracingOptions :: NodeConfig -> NodeConfig
  ##
  removeLegacyTracingOptions = cfg:
    removeAttrs cfg
    [
      "TraceAcceptPolicy"
      "TraceBlockchainTime"
      "TraceBlockFetchClient"
      "TraceBlockFetchDecisions"
      "TraceBlockFetchProtocol"
      "TraceBlockFetchProtocolSerialised"
      "TraceBlockFetchServer"
      "TraceChainDB"
      "TraceChainSyncClient"
      "TraceChainSyncBlockServer"
      "TraceChainSyncHeaderServer"
      "TraceChainSyncProtocol"
      "TraceConnectionManager"
      "TraceConnectionManagerCounters"
      "TraceConnectionManagerTransitions"
      "DebugPeerSelectionInitiator"
      "DebugPeerSelectionInitiatorResponder"
      "TraceDiffusionInitialization"
      "TraceDnsResolver"
      "TraceDnsSubscription"
      "TraceErrorPolicy"
      "TraceForge"
      "TraceForgeStateInfo"
      "TraceHandshake"
      "TraceIpSubscription"
      "TraceKeepAliveClient"
      "TraceLedgerPeers"
      "TraceLocalChainSyncProtocol"
      "TraceLocalConnectionManager"
      "TraceLocalErrorPolicy"
      "TraceLocalHandshake"
      "TraceLocalInboundGovernor"
      "TraceLocalRootPeers"
      "TraceLocalServer"
      "TraceLocalStateQueryProtocol"
      "TraceLocalTxMonitorProtocol"
      "TraceLocalTxSubmissionProtocol"
      "TraceLocalTxSubmissionServer"
      "TraceMempool"
      "TraceMux"
      "TraceLocalMux"
      "TracePeerSelection"
      "TracePeerSelectionCounters"
      "TracePeerSelectionActions"
      "TracePublicRootPeers"
      "TraceServer"
      "TraceInboundGovernor"
      "TraceInboundGovernorCounters"
      "TraceInboundGovernorTransitions"
      "TraceTxInbound"
      "TraceTxOutbound"
      "TraceTxSubmissionProtocol"
      "TraceTxSubmission2Protocol"
      "TracingVerbosity"
      "defaultBackends"
      "defaultScribes"
      "hasEKG"
      "hasPrometheus"
      "minSeverity"
      "options"
      "rotation"
      "setupBackends"
      "setupScribes"
    ];

  ## The AWS node is started with:
  ## cardano-node run
  ## --config /nix/store/nywkyj205skkqy27ip3p0678977kxq0b-config-1-0.json
  ## --database-path /var/lib/cardano-node/db-bench-dense-k51-10ep-2000kU-500kD-6600esec
  ## --topology /nix/store/sb8gn8wb4s8m7a4pmkb6hvlr4fhy5vn2-topology.yaml
  ## --shelley-vrf-key /var/lib/keys/cardano-node-vrf-signing
  ## --shelley-kes-key /var/lib/keys/cardano-node-kes-signing
  ## --shelley-operational-certificate /var/lib/keys/cardano-node-operational-cert
  ## +RTS -l-agu -t --machine-readable -RTS +RTS -N2 -A16m -qg -qb -M14336.000000M -RTS
  ##
  ## nodeSpecServiceConfig :: NodeSpec -> ServiceConfig
  ##
  nodeSpecServiceConfig =
    { name, i, kind, port, isProducer, ... }@nodeSpec:

    let
    nodeConfigBits = rec {
      base =
        ## General config bits needed for base workbench functionality.
        removeAttrs
          baseNodeConfig
          [ "AlonzoGenesisHash"
            "ByronGenesisHash"
            "ShelleyGenesisHash"
          ]
        //
        {
          TestEnableDevelopmentHardForkEras     = true;
          TestEnableDevelopmentNetworkProtocols = true;
          TurnOnLogMetrics                      = true;
        };
      tracing-transform = {
        trace-dispatcher = cfg:
          recursiveUpdate
            (import ./tracing.nix
              { inherit nodeSpec;
                inherit (profile.value.node) tracer;
              })
            (removeLegacyTracingOptions cfg);
        iohk-monitoring  = cfg:
          recursiveUpdate
            (import ./tracing-legacy.nix
              { inherit nodeSpec;
              })
            cfg;
      };
      era_setup_hardforks = {
        shelley =
          { TestShelleyHardForkAtEpoch = 0;
          };
        allegra =
          { TestShelleyHardForkAtEpoch = 0;
            TestAllegraHardForkAtEpoch = 0;
          };
        mary =
          { TestShelleyHardForkAtEpoch = 0;
            TestAllegraHardForkAtEpoch = 0;
            TestMaryHardForkAtEpoch    = 0;
          };
        alonzo =
          { TestShelleyHardForkAtEpoch = 0;
            TestAllegraHardForkAtEpoch = 0;
            TestMaryHardForkAtEpoch    = 0;
            TestAlonzoHardForkAtEpoch  = 0;
          };
        babbage =
          { TestShelleyHardForkAtEpoch = 0;
            TestAllegraHardForkAtEpoch = 0;
            TestMaryHardForkAtEpoch    = 0;
            TestAlonzoHardForkAtEpoch  = 0;
            TestBabbageHardForkAtEpoch = 0;
          };
      }.${profile.value.era};
    };
    in
    services-config.finaliseNodeService profile.value nodeSpec
    {
      inherit port;

      ## For the definition of 'nodeConfigBits', please see above.
      ## Meaning:
      ##   1. take the common base
      ##   2. apply either the hardforks config, or the preset (typically mainnet)
      ##   3. overlay the tracing config
      nodeConfig =
        recursiveUpdate
          (nodeConfigBits.tracing-transform.${profile.value.node.tracing_backend}
            (services-config.finaliseNodeConfig nodeSpec
              (recursiveUpdate
                nodeConfigBits.base
                (if __hasAttr "preset" profile.value
                 then readJSONMay (./presets + "/${profile.value.preset}/config.json")
                 else nodeConfigBits.era_setup_hardforks))))
          profile.value.node.verbatim;

      extraArgs =
        (if nodeSpec.shutdown_on_block_synced != null
          then [
            "--shutdown-on-block-synced"
            (toString nodeSpec.shutdown_on_block_synced)
          ]
          else []
        )
        ++
        (if nodeSpec.shutdown_on_slot_synced != null
            then [
              "--shutdown-on-slot-synced"
              (toString nodeSpec.shutdown_on_slot_synced)
            ]
            else []
        );
    };

  ## Given an env config, evaluate it and produce the node service.
  ## Call the given function on this service.
  ##
  ## nodeServiceConfigService :: NodeServiceConfig -> NodeService
  ##
  nodeServiceConfigService =
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
        services.cardano-node = {
          enable = true;
        } // serviceConfig;
      };
    in evalModules {
      prefix = [];
      modules = import ../../nixos/module-list.nix ++ [ systemdCompat extra ];
      args = { inherit pkgs; };
    };
    in eval.config.services.cardano-node;

  nodeSpecService =
    { name, i, mode ? null, ... }@nodeSpec:
    let
      modeIdSuffix  = if mode == null then "" else "." + mode;
      serviceConfig = nodeSpecServiceConfig    nodeSpec;
      service       = nodeServiceConfigService serviceConfig;
    in {
      nodeSpec = {
        value = nodeSpec;
        JSON  = runJq "node-spec-${name + modeIdSuffix}.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON nodeSpec}'
                  '' "$x";
      };

      serviceConfig = {
        value = serviceConfig;
        JSON  = runJq "node-service-config-${name + modeIdSuffix}.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON serviceConfig}'
                  '' "$x";
      };

      service = {
        value = service;
        JSON  = runJq "node-service-${name + modeIdSuffix}.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON service}'
                  '' "$x";
      };

      nodeConfig = {
        value = service.nodeConfig;
        JSON  = runJq "node-config-${name + modeIdSuffix}.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON service.nodeConfig}'
                  '' "$x";
      };

      topology = rec {
        JSON  = services-config.topologyForNodeSpec { inherit profile nodeSpec; };
        value = __fromJSON (__readFile JSON);
      };

      startupScript = rec {
        JSON = pkgs.writeScript "startup-${name}.sh" value;
        value = ''
          #!${pkgs.stdenv.shell}

          ${service.script}
          '';
      };
    };

  ##
  ## node-services :: Map NodeName (NodeSpec, ServiceConfig, Service, NodeConfig, Script)
  ##
  node-services = mapAttrs (_: nodeSpecService)
    profile.node-specs.value;
in
{
  inherit node-services;
}
