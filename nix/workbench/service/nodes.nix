{
  pkgs,
  workbenchNix,
  ## The cardano-node config used as baseline:
  baseNodeConfig,
  backend,
  profile,
  eraName, # Workbench's long-form ledger era name (`conway`, `babbage`, ...).
  profiling,
  nodeSpecs,
  # Derivations used to generate the topology projections
  profileJsonPath,
  topologyJsonPath,
}:
with pkgs.lib; let
  readJSONMay = fp: let
    fv = __tryEval (__readFile fp);
  in
    if fv.success
    then __fromJSON fv.value
    else {};

  profileName = profile.name;

  eras = [
    ## This defines the order of eras -- which is important.
    ## Keys are the long-form ledger era names (matching ERA in the Makefile
    ## and the `eraName` runner parameter).
    "byron"
    "shelley"
    "allegra"
    "mary"
    "alonzo"
    "babbage"
    "conway"
    "dijkstra"
  ];

  configHardforksIntoEra = era: let
    ## Per-era step: `acc -> acc`. Each step either throws (when a
    ## profile-level prerequisite for the era is missing) or returns the
    ## accumulator with the era's HF flag merged in.
    ##
    ## We MUST NOT activate an era if its genesis parameters are not present
    ## because, as the node always needs valid config file for that genesis in
    ## its config, genesis.sh will create one, a stub/all zeros genesis file.
    ## If that era is not activated with its respective TestXXXHardForkAtEpoch
    ## the dummy config file is not a problem, otherwise the chain would run
    ## with, for example, zero Plutus V3 cost models or broken governance.
    eraSetup = {
      byron = acc:
        if (profile.genesis.byron or null) == null
        then throw "configHardforksIntoEra: eraName reaches 'byron' but profile.genesis.byron is null"
        else acc;
      shelley = acc:
        if (profile.genesis.shelley or null) == null
        then throw "configHardforksIntoEra: eraName reaches 'shelley' but profile.genesis.shelley is null"
        else acc // {TestShelleyHardForkAtEpoch = 0;};
      allegra = acc: acc // {TestAllegraHardForkAtEpoch = 0;};
      mary = acc: acc // {TestMaryHardForkAtEpoch = 0;};
      alonzo = acc:
        if (profile.genesis.alonzo or null) == null
        then throw "configHardforksIntoEra: eraName reaches 'alonzo' but profile.genesis.alonzo is null"
        else acc // {TestAlonzoHardForkAtEpoch = 0;};
      babbage = acc: acc // {TestBabbageHardForkAtEpoch = 0;};
      conway = acc:
      # A null or empty profile.genesis.conway will use an inert ("zero") genesis.
        acc // {TestConwayHardForkAtEpoch = 0;};
      dijkstra = acc:
      # A null or empty profile.genesis.dijkstra will use an inert ("zero") genesis.
        acc // {TestDijkstraHardForkAtEpoch = 0;};
    };

    ## Walk `eras` in order, applying each step until we've consumed the
    ## target era. `done` short-circuits all subsequent steps.
    folded =
      foldl' (
        {
          acc,
          done,
        }: curEra:
          if done
          then {inherit acc done;}
          else {
            acc = eraSetup.${curEra} acc;
            done = curEra == era;
          }
      ) {
        acc = {};
        done = false;
      }
      eras;
  in
    if folded.done
    then folded.acc
    else throw "configHardforksIntoEra: unknown era '${era}'";

  liveTablesPath = i:
    if (profile.node ? "ssd_directory" && profile.node.ssd_directory != null)
    then "${profile.node.ssd_directory}/node-${toString i}"
    else null;

  ##
  ## nodeServiceConfig :: NodeSpec -> ServiceConfig
  ##
  nodeServiceConfig = {
    name,
    i,
    kind,
    port,
    isProducer,
    ...
  } @ nodeSpec: valency:
    {
      inherit isProducer port;
      inherit (profile.node) rts_flags_override;

      nodeId = i;
      databasePath = "db";
      socketPath = "node.socket";
      topology = "topology.json";
      nodeConfigFile = "config.json";

      # Allow for local clusters to have multiple on-disk (LSM-tree) directories in the same physical ssd_directory;
      # non-block producers (like the explorer node) keep using the in-memory backend
      withUtxoHdLsmt   = profile.node.utxo_lsmt && isProducer;
      lsmDatabasePath  = liveTablesPath i;

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
            (
              recursiveUpdate
              (removeAttrs
                baseNodeConfig
                [
                  ## Let the genesis hashes be auto-computed by the node:
                  "ByronGenesisHash"
                  "ShelleyGenesisHash"
                  "AlonzoGenesisHash"
                  "ConwayGenesisHash"
                  "DijkstraGenesisHash"
                ]
                // {
                  ExperimentalHardForksEnabled = true;
                  ExperimentalProtocolsEnabled = true;
                  TurnOnLogMetrics = true;
                  ChainSyncIdleTimeout = 0;
                  PeerSharing = false;

                  ## defaults taken from: ouroboros-network/src/Ouroboros/Network/Diffusion/Configuration.hs
                  ## NB. the following inequality must hold: known >= established >= active >= 0
                  SyncTargetNumberOfActivePeers = max 15 valency; # set to same value as TargetNumberOfActivePeers
                  SyncTargetNumberOfEstablishedPeers = max 40 valency;
                  TargetNumberOfActivePeers = max 15 valency;
                  TargetNumberOfEstablishedPeers = max 40 valency;

                  ByronGenesisFile = "../genesis/genesis.byron.json";
                  ShelleyGenesisFile = "../genesis/genesis.shelley.json";
                  AlonzoGenesisFile = "../genesis/genesis.alonzo.json";
                  ## ConwayGenesisFile / DijkstraGenesisFile are always declared
                  ## here even when the profile leaves .genesis.conway or
                  ## .genesis.dijkstra null: cardano-node's config parser
                  ## requires both keys unconditionally.
                  ## The referenced files are left for stub placeholders to be
                  ## generated by the genesis backend.
                  ## If the node never activates that era the stub is inert.
                  ConwayGenesisFile = "../genesis/genesis.conway.json";
                  DijkstraGenesisFile = "../genesis/genesis.dijkstra.json";
                }
                // optionalAttrs (profile.node.utxo_lsmt && isProducer)
                {
                  LedgerDB = {
                    Backend = "V2LSM";
                    LSMDatabasePath = liveTablesPath i;
                  };
                })
              {
                ## This LedgerDB attrset is defined regardless of backend choice.
                ## It assumes the Backend default to be "V2InMemory"; it must not overwrite any of the above, more specfic choices.
                LedgerDB = {
                  Snapshots = {
                    SnapshotInterval = 4230;
                    # Disable the randomised delay for kicking off snapshots:
                    # For benchmarks, exact timing needs to be reproducible, and identical for all nodes.
                    MinDelay = 0;
                    MaxDelay = 0;
                  };
                };
              }
            )
            (
              if __hasAttr "preset" profile && profile.preset != null
              ## It's either an undisturbed preset,
              ## or a hardforked setup.
              then readJSONMay (../profile/presets + "/${profile.preset}/config.json")
              else configHardforksIntoEra eraName
            ))
          profile.node.verbatim);

      extraArgs =
        ["+RTS" "-scardano-node.gcstats" "-RTS"]
        ++ optionals (nodeSpec.shutdown_on_block_synced != null) [
          "--shutdown-on-block-synced"
          (toString nodeSpec.shutdown_on_block_synced)
        ]
        ++ optionals (nodeSpec.shutdown_on_slot_synced != null) [
          "--shutdown-on-slot-synced"
          (toString nodeSpec.shutdown_on_slot_synced)
        ];
    }
    // optionalAttrs ((profiling.profilingTypeParam or "none") != "none") {
      # Add the profiling `-h*` RTS option.
      profiling = profiling.profilingTypeParam;
    }
    // optionalAttrs (profiling.eventlog or false) {
      # Add the `-l` RTS param with profiling.
      eventlog = true;
      # Decide where the executable comes from:
      #########################################
    }
    // optionalAttrs (!backend.useCabalRun) {
      package = workbenchNix.haskellProject.exes.cardano-node;
    }
    // optionalAttrs backend.useCabalRun {
      # Allow the shell function to take precedence.
      executable = "cardano-node";
    }
    #########################################
    // optionalAttrs isProducer {
      # Key paths match create-testnet-data output layout directly.
      # create-testnet-data uses 1-indexed pool dirs (pool1, pool2, ...).
      # Dense/bulk pools and BFT nodes are not supported by create-testnet-data.
      operationalCertificate = "../genesis/pools-keys/pool${toString (i + 1)}/opcert.cert";
      kesKey = "../genesis/pools-keys/pool${toString (i + 1)}/kes.skey";
      vrfKey = "../genesis/pools-keys/pool${toString (i + 1)}/vrf.skey";
    }
    // optionalAttrs profile.node.tracer {
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
  evalServiceConfigToService = serviceConfig: let
    systemdCompat.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      users = mkOption {};
      assertions = mkOption {};
      warnings = mkOption {};
      environment = mkOption {};
    };
    eval = let
      extra = {
        services.cardano-node =
          {
            enable = true;
          }
          // serviceConfig;
      };
    in
      evalModules {
        prefix = [];
        modules =
          import ../../nixos/module-list.nix
          ++ [
            systemdCompat
            extra
            {config._module.args = {inherit pkgs;};}
          ]
          ++ [backend.service-modules.node or {}];
      };
  in
    eval.config.services.cardano-node;

  topologiesJsonPaths = let
    projections =
      pkgs.runCommand "workbench-profile-files-${profileName}-topologies"
      {
        nativeBuildInputs = with pkgs;
        # A workbench with only the dependencies needed for this command.
          [
            workbenchNix.workbench
            jq
            workbenchNix.haskellProject.exes.cardano-topology
          ];
      }
      ''
        mkdir "$out"
        ${
          builtins.concatStringsSep
          "\n"
          (
            pkgs.lib.mapAttrsToList
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
      '';
  in
    pkgs.lib.mapAttrs
    (name: _: "${projections}/topology-${name}.json")
    nodeSpecs;

  nodeService = {
    name,
    i,
    mode ? null,
    ...
  } @ nodeSpec: let
    modeIdSuffix =
      if mode == null
      then ""
      else "." + mode;
    serviceConfig = nodeServiceConfig nodeSpec valency;
    service = evalServiceConfigToService serviceConfig;

    topology = rec {
      JSON = topologiesJsonPaths.${name};
      value = __fromJSON (__readFile JSON);
    };

    valency = let
      topo = topology.value;
      val =
        if hasAttr "localRoots" topo && __length topo.localRoots > 0
        then let lr = head topo.localRoots; in lr.valency
        else length (topo.Producers or []);
    in
      val;
  in {
    start = ''
      #!${pkgs.stdenv.shell}

      export TRACE_DISPATCHER_LOGGING_HOSTNAME=${name}

      ${service.script}
    '';

    config = service.nodeConfig;

    inherit topology;
  };

  ##
  ## node-services :: Map NodeName (NodeSpec, ServiceConfig, Service, NodeConfig, Script)
  ##
  node-services = mapAttrs (_: nodeService) nodeSpecs;
in {
  inherit node-services;
}
