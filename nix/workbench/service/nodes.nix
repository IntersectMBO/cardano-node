{ pkgs
, workbenchNix

## The cardano-node config used as baseline:
, baseNodeConfig

, backend
, profile
, profiling
, nodeSpecs

# Derivations used to generate the topology projections
, profileJsonPath, topologyJsonPath
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

  liveTablesPath = i: 
    if (profile.cluster ? "ssd_directory" && profile.cluster.ssd_directory != null)
    then "${profile.cluster.ssd_directory}/lmdb-node-${toString i}"
    else null;

  ##
  ## nodeServiceConfig :: NodeSpec -> ServiceConfig
  ##
  nodeServiceConfig =
    { name, i, kind, port, isProducer, ... }@nodeSpec: valency:
    {
      inherit isProducer port;
      inherit (profile.node) rts_flags_override;

      nodeId         = i;
      databasePath   = "db";
      socketPath     = "node.socket";
      topology       = "topology.json";
      nodeConfigFile = "config.json";

      # Allow for local clusters to have multiple LMDB directories in the same physical ssd_directory
      withUtxoHdLmdb   = profile.node.utxo_lmdb;
      lmdbDatabasePath = liveTablesPath i;

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

                  ## defaults taken from: ouroboros-network/src/Ouroboros/Network/Diffusion/Configuration.hs
                  ## NB. the following inequality must hold: known >= established >= active >= 0
                  SyncTargetNumberOfActivePeers   = max 15 valency;     # set to same value as TargetNumberOfActivePeers
                  TargetNumberOfActivePeers       = max 15 valency;
                  TargetNumberOfEstablishedPeers  = max 40 valency;

                  ByronGenesisFile             = "../genesis/byron/genesis.json";
                  ShelleyGenesisFile           = "../genesis/genesis-shelley.json";
                  AlonzoGenesisFile            = "../genesis/genesis.alonzo.json";
                  ConwayGenesisFile            = "../genesis/genesis.conway.json";
                } // optionalAttrs profile.node.utxo_lmdb
                {
                  LedgerDB = {
                    Backend = "V1LMDB";
                    LiveTablesPath = liveTablesPath i;
                  };
                })
              (if __hasAttr "preset" profile && profile.preset != null
               ## It's either an undisturbed preset,
               ## or a hardforked setup.
               then readJSONMay (../profile/presets + "/${profile.preset}/config.json")
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
      # Switch to `noGitRev` to avoid rebuilding with every commit.
      package    = pkgs.cardano-node.passthru.noGitRev;
    } // optionalAttrs backend.useCabalRun {
      # Allow the shell function to take precedence.
      executable = "cardano-node";
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

 topologiesJsonPaths =
    let projections =
          pkgs.runCommand "workbench-profile-files-${profileName}-topologies"
            { nativeBuildInputs = with pkgs;
              # A workbench with only the dependencies needed for this command.
              [ workbenchNix.workbench
                jq
                workbenchNix.cardanoNodePackages.cardano-topology
              ];
            }
            ''
            mkdir "$out"
            ${builtins.concatStringsSep
              "\n"
              (pkgs.lib.mapAttrsToList
                (name: nodeSpec: ''
                    wb topology projection-for     \
                    "local-${nodeSpec.kind}"       \
                    "${toString nodeSpec.i}"       \
                    "${profileJsonPath}"           \
                    "${topologyJsonPath}"          \
                    "${toString backend.basePort}" \
                  > "$out"/"topology-${name}.json"
                '')
                nodeSpecs
              )
            }
            ''
        ;
    in  pkgs.lib.mapAttrs
          (name: _: "${projections}/topology-${name}.json")
          nodeSpecs
  ;

  nodeService =
    { name, i, mode ? null, ... }@nodeSpec:
    let
      modeIdSuffix  = if mode == null then "" else "." + mode;
      serviceConfig = nodeServiceConfig nodeSpec valency;
      service       = evalServiceConfigToService serviceConfig;

      topology =
         rec {
           JSON  = topologiesJsonPaths.${name};
           value = __fromJSON (__readFile JSON);
         }
      ;

      valency =
        let
          topo = topology.value;
          val  = if hasAttr "localRoots" topo
                  then let lr = head topo.localRoots; in lr.valency
                  else length topo.Producers;
        in val;

    in {
      start =
        ''
        #!${pkgs.stdenv.shell}

        ${service.script}
        ''
      ;

      config = service.nodeConfig;

      inherit topology;
    };

  ##
  ## node-services :: Map NodeName (NodeSpec, ServiceConfig, Service, NodeConfig, Script)
  ##
  node-services = mapAttrs (_: nodeService) nodeSpecs;
in
{
  inherit node-services;
}
