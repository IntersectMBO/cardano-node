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
  # Bean node override:
  # `nodeImpl` selects the node binary ("cardano" | "bean");
  # `beanNodePackage` is the bean node derivation (`bean.packages.<system>.cardano-node`);
  # `beanJavaOpts` is a list of extra JVM flags for bean nodes.
  nodeImpl        ? "cardano",
  beanJavaOpts    ? [],
  beanNodePackage ? null,
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

  ## Wall clock seconds between two ledger snapshots: 4230 by default, unless
  ## the profile sets .node.snapshot_interval_override (also in seconds).
  snapshotIntervalSeconds =
    if profile.node.snapshot_interval_override or null != null
    then profile.node.snapshot_interval_override
    else 4230;

  ## First slot of the ledger snapshot schedule: 1000 by default, unless the
  ## profile sets .node.slot_offset_override (also in slots).
  snapshotSlotOffset =
    if profile.node.slot_offset_override or null != null
    then profile.node.slot_offset_override
    else 1000;

  ##
  # Bean node override: which nodes run bean. PRODUCERS ONLY, by
  # deliberate choice: the benchmark measures bean block forging while the
  # explorer, relays, generator and tracer tooling stay on the reference
  # implementation (cardano-cli tip queries, log/trace analysis and tx
  # submission are cardano-node-specific). Swapping non-forging nodes adds
  # build/run cost without benchmark signal, and would break analysis that
  # parses cardano-node log formats.
  isBeanNode = nodeSpec: nodeImpl == "bean" && (nodeSpec.isProducer or false);

  beanNodeDrv =
    if beanNodePackage == null
    then throw "NODE=bean but no bean node package was provided: point the `bean` flake input at a bean flake exposing `packages.<system>.cardano-node` (see flake.nix)"
    else beanNodePackage;

  # JVM flags for bean nodes, from two channels that both work without
  # editing this patch: (1) BEAN_JAVA_OPTS at shell entry (make/env), split
  # on whitespace in shell.nix; (2) an optional profile key
  # `node.bean_java_opts_override` in the profile JSON (forward-compatible:
  # cardano-profile does not emit it yet, so `or []` keeps all current
  # profiles working). Delivered via BEAN_JAVA_OPTS in the start script,
  # never via argv: a JVM launcher requires -D... before the main class,
  # while node extraArgs are appended after the `run` subcommand.
  beanJavaFlags = beanJavaOpts ++ (profile.node.bean_java_opts_override or []);

  # Exported at the top of every bean producer start script. The bean
  # launcher puts $BEAN_JAVA_OPTS on the java command line, after the JVM has
  # read any inherited JAVA_TOOL_OPTIONS, so these flags win over it; an
  # inherited JAVA_TOOL_OPTIONS is left untouched. Eval-time flags come first
  # and a runtime `$BEAN_JAVA_OPTS` (set inside the shell, for a single run
  # without re-entering it) follows, so the runtime value wins. Make's
  # BEAN_JAVA_OPTS reaches only the eval-time list: lib.mk unsets it for the
  # nix-shell call, so it is not applied twice. The launcher splits the value
  # on whitespace, so a flag must not contain any.
  beanJavaFlagEscaped = flag:
    if builtins.match ".*[[:space:]].*" flag != null
    then throw "bean JVM flag must not contain whitespace: ${flag}"
    else escape [ "\\" "\"" "$" "`" ] flag;
  beanJavaPrelude = ''
    # Bean node override (NODE=bean): JVM flags for the bean node.
    export BEAN_JAVA_OPTS="${concatStringsSep " " (map beanJavaFlagEscaped beanJavaFlags)}''${BEAN_JAVA_OPTS:+ $BEAN_JAVA_OPTS}"
  '';

  ## nodeServiceConfig :: NodeSpec -> ServiceConfig
  ##
  nodeServiceConfig = {
    name,
    i,
    kind,
    port,
    isProducer,
    ...
  } @ nodeSpec: valency: let
    isBean = isBeanNode nodeSpec;
  in {
      inherit isProducer port;
      # RTS flags are GHC-only; the JVM bean node must not receive them.
      rts_flags_override = if isBean then [] else profile.node.rts_flags_override;

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

                ## Deprecated top level option, in seconds: the only one node
                ## 11.0.X and earlier read from this config (they ignore the
                ## LedgerDB.Snapshots section below). Later nodes accept it
                ## with a deprecation warning, the LedgerDB.Snapshots value
                ## taking precedence.
                SnapshotInterval = snapshotIntervalSeconds;

                ## The Snapshots section applies regardless of backend choice
                ## The Backend key, when absent, defaults to "V2InMemory".
                LedgerDB =
                  {
                    Snapshots = {
                      ## Number of slots between two ledger snapshots:
                      ## SnapshotInterval counts slots on the fixed schedule
                      ## SlotOffset + n * SnapshotInterval, the node snapshots
                      ## the last immutable ledger state before each point.
                      ##
                      ## Before node 11.1.X the workbench requested a snapshot
                      ## every 4230 seconds of wall clock time via the top level
                      ## SnapshotInterval option. The option now counts slots,
                      ## so the 4230 second cadence is converted using the
                      ## profile slot duration. Adding 0.5 before flooring
                      ## rounds to the nearest slot, avoiding float division
                      ## artifacts when slot_duration is fractional.
                      SnapshotInterval =
                        builtins.floor
                          (snapshotIntervalSeconds / profile.genesis.slot_duration + 0.5)
                      ;
                      ## First point of the schedule: later points follow every
                      ## SnapshotInterval slots after it.
                      ##
                      ## A point fires only once a block earlier than it is
                      ## immutable, and a missed point is never retried: for the
                      ## first point to fire, the offset must exceed the slot
                      ## where the cluster actually starts forging, plus margin
                      ## for the first block to appear.
                      ##
                      ## Deliberately set at 1000 based on pre 11.1.0 "value"
                      ## runs; profiles with short durations can lower it via
                      ## .node.slot_offset_override.
                      ## Never leave it unset, the default is Mithril's 388800,
                      ## unreachable on most cluster runs.
                      ## Node 11.0.X and earlier ignore it.
                      SlotOffset = snapshotSlotOffset;
                      ## Disable the minimum wall clock period between two
                      ## written snapshots, the policy replaced by this section
                      ## had no such check.
                      RateLimit = 0;
                      # Disable the randomised delay for kicking off snapshots:
                      # For benchmarks, exact timing needs to be reproducible,
                      # and identical for all nodes.
                      MinDelay = 0;
                      MaxDelay = 0;
                    };
                  }
                  // optionalAttrs (profile.node.utxo_lsmt && isProducer) {
                    Backend = "V2LSM";
                    LSMDatabasePath = liveTablesPath i;
                  };
              })
            (
              if __hasAttr "preset" profile && profile.preset != null
              ## It's either an undisturbed preset,
              ## or a hardforked setup.
              then readJSONMay (../profile/presets + "/${profile.preset}/config.json")
              else configHardforksIntoEra eraName
            ))
          profile.node.verbatim);

      # GHC RTS flags are cardano-only; bean nodes get JVM flags via
      # BEAN_JAVA_OPTS in the start script instead (see nodeService).
      extraArgs =
        optionals (!isBean) ["+RTS" "-scardano-node.gcstats" "-RTS"]
        ++ optionals (!isBean && nodeSpec.shutdown_on_block_synced != null) [
          "--shutdown-on-block-synced"
          (toString nodeSpec.shutdown_on_block_synced)
        ]
        ++ optionals (!isBean && nodeSpec.shutdown_on_slot_synced != null) [
          "--shutdown-on-slot-synced"
          (toString nodeSpec.shutdown_on_slot_synced)
        ];
    }
    // optionalAttrs (((profiling.profilingTypeParam or "none") != "none") && !isBean) {
      # Add the profiling `-h*` RTS option.
      profiling = profiling.profilingTypeParam;
    }
    // optionalAttrs ((profiling.eventlog or false) && !isBean) {
      # Add the `-l` RTS param with profiling.
      eventlog = true;
    }
    # Decide where the executable comes from:
    #########################################
    // optionalAttrs (!backend.useCabalRun && !isBean) {
      package = workbenchNix.haskellProject.exes.cardano-node;
    }
    // optionalAttrs (backend.useCabalRun && !isBean) {
      # Allow the shell function to take precedence.
      executable = "cardano-node";
    }
    #########################################
    # Bean node override: producers run the bean Nix store binary.
    # Bean has no cabal target in this repo, so `package` wins regardless
    # of `useCabalRun` (what `cabal run` would build is the Haskell node,
    # not bean). Prefer the -nix make variants with NODE=bean regardless.
    // optionalAttrs isBean {
      package = beanNodeDrv;
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

    isBean = isBeanNode nodeSpec;
  in {
    start = ''
      #!${pkgs.stdenv.shell}

      export TRACE_DISPATCHER_LOGGING_HOSTNAME=${name}
      ${optionalString isBean beanJavaPrelude}

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
