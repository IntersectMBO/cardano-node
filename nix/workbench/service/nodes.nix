{ pkgs
, runJq
, runWorkbench

## The cardano-node config used as baseline:
, baseNodeConfig

, backend
, profile
, profiling
, nodeSpecs
, topologyFiles
}:

with pkgs.lib;
with (import ../lib.nix pkgs.lib);

let
  profileName = profile.name;

  eras = [ ## This defines the order of eras -- which is important.
    "byron"
    "shelley"
    "allegra"
    "mary"
    "alonzo"
    "babbage"
    "conway"
  ];

  configHardforksIntoEra = era:
    let go = acc: curEra: rest:
          let ret = acc // eraSetupHardforks.${curEra};
          in if curEra == era
             then ret
             else go ret (__head rest) (__tail rest);
        eraSetupHardforks = {
          byron   = {};
          shelley = { TestShelleyHardForkAtEpoch = 0; };
          allegra = { TestAllegraHardForkAtEpoch = 0; };
          mary    = { TestMaryHardForkAtEpoch    = 0; };
          alonzo  = { TestAlonzoHardForkAtEpoch  = 0; };
          babbage = { TestBabbageHardForkAtEpoch = 0; };
          conway  = { TestConwayHardForkAtEpoch  = 0; };
        };
    in if __hasAttr era eraSetupHardforks
       then go {} (__head eras) (__tail eras)
       else throw "configHardforksIntoEra:  unknown era '${era}'";

  ##
  ## nodeServiceConfig :: NodeSpec -> ServiceConfig
  ##
  nodeServiceConfig =
    { name, i, kind, port, isProducer, ... }@nodeSpec:
    {
      inherit isProducer port;

      nodeId         = i;
      databasePath   = "db";
      socketPath     = "node.socket";
      topology       = "topology.json";
      nodeConfigFile = "config.json";

      ## Combine:
      ##   0. baseNodeConfig (coming cardanoLib's testnet environ)
      ##   1. common workbench 'base'
      ##   2. either hardforks config, or preset (typically mainnet)
      ##   3. profile-specific verbatim node config section
      ##   4. tracing backend's config
      nodeConfig =
        import ./tracing.nix
          {
            inherit (pkgs) lib;
            inherit nodeSpec;
            inherit (profile.node) tracing_backend tracer;
          }
          (recursiveUpdate
            (recursiveUpdate
              (removeAttrs
                baseNodeConfig
                [ ## Let the genesis hashes be auto-computed by the node:
                  "ByronGenesisHash"
                  "ShelleyGenesisHash"
                  "AlonzoGenesisHash"
                  "ConwayGenesisHash"
                ] //
                {
                  ExperimentalHardForksEnabled = true;
                  ExperimentalProtocolsEnabled = true;
                  TurnOnLogMetrics             = true;
                  SnapshotInterval             = 4230;

                  ByronGenesisFile             = "../genesis/byron/genesis.json";
                  ShelleyGenesisFile           = "../genesis/genesis-shelley.json";
                  AlonzoGenesisFile            = "../genesis/genesis.alonzo.json";
                  ConwayGenesisFile            = "../genesis/genesis.conway.json";
                })
              (if __hasAttr "preset" profile
               ## It's either an undisturbed preset,
               ## or a hardforked setup.
               then readJSONMay (./presets + "/${profile.preset}/config.json")
               else configHardforksIntoEra profile.era))
            profile.node.verbatim);

      extraArgs =
        [ "+RTS" "-scardano-node.gcstats" "-RTS" ]
        ++
        optionals (nodeSpec.shutdown_on_block_synced != null) [
          "--shutdown-on-block-synced"
          (toString nodeSpec.shutdown_on_block_synced)
        ] ++
        optionals (nodeSpec.shutdown_on_slot_synced != null) [
          "--shutdown-on-slot-synced"
          (toString nodeSpec.shutdown_on_slot_synced)
        ];
    } // optionalAttrs (profiling != "none") {
      inherit profiling;
    } // optionalAttrs (profiling == "none") {
      eventlog               = mkForce true;
    } // optionalAttrs backend.useCabalRun {
      # Allow the shell function to take precedence.
      executable             = "cardano-node";
    } // optionalAttrs isProducer {
      operationalCertificate = "../genesis/node-keys/node${toString i}.opcert";
      kesKey                 = "../genesis/node-keys/node-kes${toString i}.skey";
      vrfKey                 = "../genesis/node-keys/node-vrf${toString i}.skey";
    } // optionalAttrs profile.node.tracer {
      tracerSocketPathConnect = mkDefault "../tracer/tracer.socket";
    };

    time_fmtstr =
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

  ## Given an env config, evaluate it and produce the node service.
  ## Call the given function on this service.
  ##
  ## evalServiceConfigToService :: NodeServiceConfig -> NodeService
  ##
  evalServiceConfigToService =
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
    };
    in
      eval.config.services.cardano-node;

  nodeService =
    { name, i, mode ? null, ... }@nodeSpec:
    let
      modeIdSuffix  = if mode == null then "" else "." + mode;
      serviceConfig = nodeServiceConfig nodeSpec;
      service       = evalServiceConfigToService serviceConfig;
    in {
      start = rec {
        value = ''
          #!${pkgs.stdenv.shell}

          ${service.script}
          '';
        JSON = pkgs.writeScript "startup-${name}.sh" value;
      };

      config = rec {
        value =
          service.nodeConfig
          //
          # New options to be added
          {
            TraceOptionPeerFrequency = 3000;
            TraceOptionResourceFrequency = 4000;
            TurnOnLogMetrics = true;
          }
          //
          # Update "new tracing system" options
          {
            TraceOptions = {
              "" = {
                severity = "Notice";
                detail = "DNormal";
                backends = [
                  "Stdout MachineFormat"
                  "EKGBackend"
                  "Forwarder"
                ];
              };
              "BlockFetch.Client".severity = "Debug";
              "BlockFetch.Decision".severity = "Notice";
              "BlockFetch.Remote".severity = "Notice";
              "BlockFetch.Remote.Serialised".severity = "Notice";
              "BlockFetch.Server".severity = "Debug";
              "BlockchainTime".severity = "Notice";
              "ChainDB".severity = "Debug";
              "ChainDB.ReplayBlock.LedgerReplay".severity = "Notice";
              "ChainSync.Client".severity = "Debug";
              "ChainSync.Local".severity = "Notice";
              "ChainSync.Remote".severity = "Notice";
              "ChainSync.Remote.Serialised".severity = "Notice";
              "ChainSync.ServerBlock".severity = "Notice";
              "ChainSync.ServerHeader".severity = "Debug";
              "Forge.Loop".severity = "Debug";
              "Forge.StateInfo".severity = "Debug";
              "Mempool".severity = "Debug";
              "Net".severity = "Notice";
              "Net.AcceptPolicy".severity = "Debug";
              "Net.ConnectionManager.Local".severity = "Debug";
              "Net.ConnectionManager.Remote".severity = "Debug";
              "Net.DNSResolver".severity = "Notice";
              "Net.ErrorPolicy.Local".severity = "Debug";
              "Net.ErrorPolicy.Remote".severity = "Debug";
              "Net.Handshake.Local".severity = "Debug";
              "Net.Handshake.Remote".severity = "Debug";
              "Net.InboundGovernor.Local".severity = "Debug";
              "Net.InboundGovernor.Remote".severity = "Debug";
              "Net.InboundGovernor.Transition".severity = "Debug";
              "Net.Mux.Local".severity = "Notice";
              "Net.Mux.Remote".severity = "Notice";
              "Net.PeerSelection.Actions".severity = "Debug";
              "Net.PeerSelection.Counters".severity = "Debug";
              "Net.PeerSelection.Initiator".severity = "Notice";
              "Net.PeerSelection.Responder".severity = "Notice";
              "Net.PeerSelection.Selection".severity = "Debug";
              "Net.Peers.Ledger".severity = "Debug";
              "Net.Peers.List".severity = "Notice";
              "Net.Peers.LocalRoot".severity = "Debug";
              "Net.Peers.PublicRoot".severity = "Debug";
              "Net.Server.Local".severity = "Debug";
              "Net.Server.Remote".severity = "Debug";
              "Net.Subscription.DNS".severity = "Debug";
              "Net.Subscription.IP".severity = "Debug";
              "NodeState".severity = "Notice";
              "Resources".severity = "Debug";
              "Shutdown".severity = "Notice";
              "Startup".severity = "Notice";
              "Startup.DiffusionInit".severity = "Debug";
              "StateQueryServer".severity = "Notice";
              "TxSubmission.Local".severity = "Notice";
              "TxSubmission.LocalServer".severity = "Notice";
              "TxSubmission.MonitorClient".severity = "Notice";
              "TxSubmission.Remote".severity = "Notice";
              "TxSubmission.TxInbound".severity = "Debug";
              "TxSubmission.TxOutbound".severity = "Notice";
            };
          }
        ;
        JSON  = runJq "node-config-${name + modeIdSuffix}.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON value}'
                  '' "$x";
      };

      topology =
        rec {
          JSON  = runWorkbench
                    "topology-${name}.json"
                    "topology projection-for local-${nodeSpec.kind} ${toString i} ${profileName} ${topologyFiles} ${toString backend.basePort}";
          value = __fromJSON (__readFile JSON);
        };
    };

  ##
  ## node-services :: Map NodeName (NodeSpec, ServiceConfig, Service, NodeConfig, Script)
  ##
  node-services = mapAttrs (_: nodeService) nodeSpecs;
in
{
  inherit node-services;
}
