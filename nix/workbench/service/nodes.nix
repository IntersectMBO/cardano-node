{ pkgs
, runJq
, runWorkbench
, jsonFilePretty

## The cardano-node config used as baseline:
, baseNodeConfig

, backend
, profile
, profiling
, nodeSpecs
, topologyFiles
}:

with pkgs.lib;

let
  readJSONMay = fp:
    let fv = __tryEval (__readFile fp);
    in if fv.success
       then __fromJSON fv.value
       else {};

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
      inherit (profile.node) rts_flags_override;

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
                  ChainSyncIdleTimeout         = 0;

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
      environment = mkOption {};
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

      config = {
        value = service.nodeConfig;
        JSON  = jsonFilePretty
                  "node-config-${name + modeIdSuffix}.json"
                  (__toJSON service.nodeConfig)
        ;
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
