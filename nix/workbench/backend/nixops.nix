{ pkgs
, lib
, stateDir # Not used here, just returned
, basePort
## `useCabalRun` not used here unlike `supervisor.nix`.
, ...
}:
with lib; with pkgs.commonLib;
let
  nixopsAlaCardanoOps =
    let plugins = [ "nixops-aws"
                    "nixops-libvirtd"
                  ];
        sourcePaths = import ./nixops/sources.nix { inherit pkgs; };
    in
      (import (sourcePaths.nixops-core + "/release.nix") {
        nixpkgs = sourcePaths.nixpkgs-nixops;
        # nixpkgs = pkgs.outPath;
        nixpkgsConfig.permittedInsecurePackages = [ "libvirt-5.9.0" ];
        pluginsSources = sourcePaths;
        p = lib.attrVals plugins;
        withManual = false;
      }).build.${pkgs.stdenv.system};
  extraShellPkgs = [ nixopsAlaCardanoOps ];

  mkGlobals =
    profileData: pkgs: _:
    let
      inherit (profileData) topology genesis;
      inherit (pkgs) globals;

      ## WARNING: IFD
      topologyNix = __fromJSON (__readFile "${topology.files}/topology-nixops.json");

      mkNodeOverlay =
        machineOverlay: nodeConfigOverlay:
        recursiveUpdate
        {
          documentation = {
            man.enable = false;
            doc.enable = false;
          };
          networking.firewall.allowPing = mkForce true;
          services.cardano-node = {
            eventlog = mkForce true;
            extraNodeConfig = mkExtraNodeConfig nodeConfigOverlay;
            rtsArgs =
              mkForce ([ "-N2" "-A16m" "-qg" "-qb" "-scardano-node.gcstats" ]
                       ++
                       (profileData.value.node.rts_flags_override or []));
            useNewTopology = profileData.value.node.p2p or false;
            usePeersFromLedgerAfterSlot = if profileData.value.node.useLedgerPeers then 0 else -1;
          };
        } machineOverlay;

      mkExtraNodeConfig =
        { TraceBlockFetchProtocol ? false
        , ... }:
        recursiveUpdate
          (removeAttrs globals.environmentConfig.nodeConfig
            ["ByronGenesisHash"
             "ShelleyGenesisHash"
             "AlonzoGenesisHash"])
          (recursiveUpdate
            (benchmarkingLogConfig "node")
            ({
               TracingVerbosity = "NormalVerbosity";
               minSeverity = "Debug";
               TurnOnLogMetrics = true;

               ExperimentalHardForksEnabled = true;
               ExperimentalProtocolsEnabled = true;

               inherit TraceBlockFetchProtocol;

               TraceMempool               = true;
               TraceTxInbound             = true;
               TraceBlockFetchClient      = true;
               TraceBlockFetchServer      = true;
               TraceChainSyncHeaderServer = true;
               TraceChainSyncClient       = true;
            } //
            (profileData.value.node.extra_config or {})));

      benchmarkingLogConfig = name: {
        defaultScribes = [
          [ "StdoutSK" "stdout" ]
          [ "FileSK"   "logs/${name}.json" ]
        ];
        setupScribes = [
          {
            scKind     = "StdoutSK";
            scName     = "stdout";
            scFormat   = "ScJson"; }
          {
            scKind     = "FileSK";
            scName     = "logs/${name}.json";
            scFormat   = "ScJson";
            scRotation = {
              ## 1. Twice-daily, so not too large, but also most benchmarks
              ##    would be covered by that
              rpMaxAgeHours   = 12;
              ## 2. Ten per epoch, for two last epochs
              rpKeepFilesNum  = 20;
              ## 3. 10GB/file to prevent file-size cutoff from happening,
              ##    and so most benchmarks will have just 1 file
              rpLogLimitBytes = 10*1000*1000*1000;
            }; }
        ];
        options = {
          mapBackends = {
            "cardano.node.resources" = [ "KatipBK" ];
            "cardano.node.metrics"   = [ "EKGViewBK" ];
          };
        };
      };
      envConfigBase = pkgs.cardanoLib.environments.testnet;
      AlonzoGenesisFile  = "${genesis.files}/genesis.alonzo.json";
      ShelleyGenesisFile = "${genesis.files}/genesis-shelley.json";
      ByronGenesisFile   = "${genesis.files}/byron/genesis.json";
      ConwayGenesisFile  = "${genesis.files}/genesis.conway.json";
    in
    rec {
      profile = profileData;

      deploymentName = builtins.getEnv "WB_DEPLOYMENT_NAME";
      networkName = "Workbench cluster ${deploymentName}, ${toString (__length topologyNix.coreNodes)} block producers, ${toString (__length topologyNix.relayNodes)} relays, profile ${profileData.profileName}";

      withMonitoring = false;
      withExplorer = false;
      withSnapshots = false;
      withSubmitApi = false;
      withFaucet = false;
      withFaucetOptions = {};
      withSmash = false;

      dnsZone = "dev.cardano.org";
      relaysNew = "relays-new.${globals.domain}";
      domain = "${globals.deploymentName}.${globals.dnsZone}";

      deployerIp = requireEnv "DEPLOYER_IP";

      cardanoNodePort = 3001;
      cardanoNodePrometheusExporterPort = 12798;
      cardanoExplorerPrometheusExporterPort = 12698;

      systemDiskAllocationSize = 15;
      nodeDbDiskAllocationSize = 15;
      nbInstancesPerRelay = 1;

      environmentName = "bench-${profileData.profileName}";
      environmentVariables = {};
      environmentConfig = rec {
        relays = "relays.${globals.domain}";

        edgePort = globals.cardanoNodePort;
        private = true;
        networkConfig = (removeAttrs envConfigBase.networkConfig ["AlonzoGenesisHash"]) // {
          Protocol = "Cardano";
          inherit  AlonzoGenesisFile;
          inherit ShelleyGenesisFile;
          inherit   ByronGenesisFile;
          inherit  ConwayGenesisFile;
        };
        nodeConfig = (removeAttrs envConfigBase.nodeConfig ["AlonzoGenesisHash"]) // {
          Protocol = "Cardano";
          inherit  AlonzoGenesisFile;
          inherit ShelleyGenesisFile;
          inherit   ByronGenesisFile;
          inherit  ConwayGenesisFile;
        } // {
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
          conway =
            { TestShelleyHardForkAtEpoch = 0;
              TestAllegraHardForkAtEpoch = 0;
              TestMaryHardForkAtEpoch    = 0;
              TestAlonzoHardForkAtEpoch  = 0;
              TestBabbageHardForkAtEpoch = 0;
              TestConwayHardForkAtEpoch  = 0;
            };
        }.${profileData.value.era};
        txSubmitConfig = {
          inherit (networkConfig) RequiresNetworkMagic;
          inherit ConwayGenesisFile AlonzoGenesisFile ShelleyGenesisFile ByronGenesisFile;
        } // pkgs.iohkNix.cardanoLib.defaultExplorerLogConfig;

        ## This is overlaid atop the defaults in the tx-generator service,
        ## as specified in the 'cardano-node' repository.
        generatorConfig = profileData.value.generator;
      };

      topology = {
        ## XXX: ACTUAL point of definition of globals.topology for physical-aws!
        relayNodes = [];
        # relayNodes = map
        #   (recursiveUpdate
        #     (mkNodeOverlay
        #       ## 1. nixos machine overlay
        #       {
        #         ## XXX: assumes we have `explorer` as our only relay.
        #         imports = [
        #           # pkgs.cardano-ops.roles.explorer
        #           # ({ config, ...}: {
        #           # })
        #         ];
        #         systemd.services.dump-registered-relays-topology.enable = mkForce false;
        #         services.cardano-node.systemdSocketActivation = mkForce false;
        #         services.cardano-node.tracerSocketPathConnect = mkForce
        #           (if profileData.value.node.tracing_backend == "iohk-monitoring" then null
        #            else "/var/lib/cardano-node/tracer.socket");
        #         services.cardano-tracer.networkMagic =
        #           mkForce profileData.value.genesis.protocol_magic;
        #         ## Generator can only use new tracing:
        #         services.tx-generator.tracerSocketPath =
        #           mkForce "/var/lib/cardano-node/tracer.socket";
        #       }
        #       ## 2. cardano-node service config overlay
        #       {
        #         ## This allows tracking block contents on the explorer.
        #         TraceBlockFetchProtocol = true;
        #       }))
        #   (topologyNix.relayNodes or []);
        coreNodes = map
          (recursiveUpdate
            (mkNodeOverlay
              ## 1. nixos machine overlay
              {
                stakePool = true;
                services.cardano-node.systemdSocketActivation = mkForce false;
                services.cardano-node.tracerSocketPathConnect = mkForce
                  (if profileData.value.node.tracing_backend == "iohk-monitoring" then null
                   else "/var/lib/cardano-node/tracer.socket");
                services.cardano-tracer.networkMagic = mkForce profileData.value.genesis.protocol_magic;
              }
              ## 2. cardano-node service config overlay
              {
              }
            )) (topologyNix.coreNodes or []);
      };

      ec2 = with pkgs.iohk-ops-lib.physical.aws;
        {
          instances = {
            core-node = c5-2xlarge;
            relay-node = c5-2xlarge;
          };
          credentials = {
            accessKeyIds = {
              IOHK = "dev-deployer";
              dns = "dev-deployer";
            };
          };
        };
    };

  materialise-profile =
    { profileData }:
      let
      in pkgs.runCommand "workbench-backend-output-${profileData.profileName}-nixops"
        {}
        ''
        mkdir $out
        touch $out/empty
        '';

  overlay = profileData: self: super:
    {
      globals = mkGlobals profileData self super;
      cardano-ops = {
        modules = {
          base-service = import ./nixops/module-base-service.nix self profileData;
          common       = import ./nixops/module-common.nix       self;
        };
        roles = {
          pool         = import ./nixops/role-pool.nix         self;
          explorer     = import ./nixops/role-explorer.nix     self profileData;
          relay        = import ./nixops/role-relay.nix        self;
        };
      };
    };

  service-modules = {
    node =
      { name, config, ... }:
      { _file = ./nixops.nix;
        config.services.cardano-node = {
          signingKey             = mkForce "/var/lib/keys/cardano-node-signing";
          delegationCertificate  = mkForce "/var/lib/keys/cardano-node-delegation-cert";
          kesKey                 = mkForce "/var/lib/keys/cardano-node-kes-signing";
          vrfKey                 = mkForce "/var/lib/keys/cardano-node-vrf-signing";
          operationalCertificate = mkForce "/var/lib/keys/cardano-node-operational-cert";
        };
      };
  };
in
{
  name = "nixops";

  inherit extraShellPkgs materialise-profile overlay service-modules stateDir basePort;

  useCabalRun = false;
}
