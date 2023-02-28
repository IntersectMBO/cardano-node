{ pkgs
, runJq
, runWorkbench

## The cardano-node config used as baseline:
, baseNodeConfig

, backend
, profile
, nodeSpecs
, topologyFiles
}:

with pkgs.lib;
with (import ../lib.nix pkgs.lib);

let
  profileName = profile.name;

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
  finaliseNodeService =
    let time_fmtstr =
          "{ " + escape [''"''] (concatStringsSep ''\n, '' time_entries) + " }";
        time_entries = [
          ''"wall_clock_s":       %e''
          ''"user_cpu_s":         %U''
          ''"sys_cpu_s":          %S''
          ''"avg_cpu_pct":       "%P"''
          ''"rss_peak_kb":        %M''
          ''"signals_received":   %k''
          ''"ctxsw_involuntary":  %c''
          ''"ctxsw_volunt_waits": %w''
          ''"pageflt_major":      %F''
          ''"pageflt_minor":      %R''
          ''"swaps":              %W''
          ''"io_fs_reads":        %I''
          ''"io_fs_writes":       %O''
          ''"cmdline":           "%C"''
          ''"exit_code":          %x''
        ];
    in
    profile: { name, i, isProducer, ... }: svc:
      recursiveUpdate svc
        ({
          inherit isProducer;
          socketPath     = "node.socket";
          topology       = "topology.json";
          nodeConfigFile = "config.json";
          nodeId         = i;
        } // optionalAttrs backend.useCabalRun {
          # Allow the shell function to take precedence.
          executable     = "cardano-node";
        } // optionalAttrs isProducer {
          operationalCertificate = "../genesis/node-keys/node${toString i}.opcert";
          kesKey                 = "../genesis/node-keys/node-kes${toString i}.skey";
          vrfKey                 = "../genesis/node-keys/node-vrf${toString i}.skey";
        } // optionalAttrs profile.node.tracer {
          tracerSocketPathConnect = mkDefault "../tracer/tracer.socket";
        });

  finaliseNodeConfig =
    { port, ... }: cfg: recursiveUpdate cfg
      (
        {
          AlonzoGenesisFile    = "../genesis/genesis.alonzo.json";
          ShelleyGenesisFile   = "../genesis/genesis-shelley.json";
          ByronGenesisFile     = "../genesis/byron/genesis.json";
        }
      );

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
                inherit (profile.node) tracer;
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
      }.${profile.era};
    };
    in
    finaliseNodeService profile nodeSpec
    {
      inherit port;

      eventlog = mkForce true;

      ## For the definition of 'nodeConfigBits', please see above.
      ## Meaning:
      ##   1. take the common base
      ##   2. apply either the hardforks config, or the preset (typically mainnet)
      ##   3. overlay the tracing config
      nodeConfig =
        recursiveUpdate
          (nodeConfigBits.tracing-transform.${profile.node.tracing_backend}
            (finaliseNodeConfig nodeSpec
              (recursiveUpdate
                nodeConfigBits.base
                (if __hasAttr "preset" profile
                 then readJSONMay (./presets + "/${profile.preset}/config.json")
                 else nodeConfigBits.era_setup_hardforks))))
          profile.node.verbatim;

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
      modules = import ../../nixos/module-list.nix
                ++ [ systemdCompat extra
                     { config._module.args = { inherit pkgs; }; }
                   ]
                ++ [ backend.service-modules.node or {} ];
      # args = { inherit pkgs; };
    };
    in
      # {
      #   services.cardano-node         = eval.config.services.cardano-node;
      #   systemd.services.cardano-node = eval.config.systemd.services.cardano-node;
      #   systemd.sockets.cardano-node  = eval.config.systemd.sockets.cardano-node;
      #   users.groups.cardano-node     = eval.config.users.groups.cardano-node;
      #   users.users.cardano-node      = eval.config.users.users.cardano-node;
      # }
      eval.config.services.cardano-node;

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

      topology =
        rec {
          JSON  = runWorkbench
                    "topology-${name}.json"
                    "topology projection-for local-${nodeSpec.kind} ${toString i} ${profileName} ${topologyFiles} ${toString backend.basePort}";
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
  node-services = mapAttrs (_: nodeSpecService) nodeSpecs;
in
{
  inherit node-services;
}
