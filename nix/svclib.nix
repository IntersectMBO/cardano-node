{ pkgs, cardano-node ? pkgs.cardano-node }:
with builtins; with pkgs.lib;
let

  ## mkNodeConfig
  ##   :: NodeId Int -> ServiceConfig AttrSet -> NodeConfig AttrSet
  mkNodeConfig = cfg: NodeId:
    cfg.nodeConfig //
    { inherit NodeId; } //
    (optionalAttrs (cfg.protover-major or null != null) { LastKnownBlockVersion-Major = cfg.protover-major; }) //
    (optionalAttrs (cfg.protover-minor or null != null) { LastKnownBlockVersion-Minor = cfg.protover-minor; }) //
    (optionalAttrs (cfg.protover-alt   or null != null) { LastKnownBlockVersion-Alt   = cfg.protover-alt;   }) //
    (optionalAttrs (cfg.genesisHash != null) { GenesisHash = cfg.genesisHash; });

  ## mkFullyConnectedLocalClusterTopology
  ##   :: String Address -> String Port -> Int PortNo -> Int Valency
  ##   -> Topology FilePath
  mkFullyConnectedLocalClusterTopology =
    { host-addr
    , port-base
    , node-count
    , valency ? 1
    }:
    let
      addr = host-addr;
      ports = map (x: port-base + x) (range 0 (node-count - 1));
      mkPeer = port: { inherit addr port valency; };
      mkNodeTopo = nodeId: port: {
        inherit nodeId;
        nodeAddress = { inherit addr port; };
        producers = map mkPeer (remove port ports);
      };
    in toFile "topology.json" (toJSON (imap0 mkNodeTopo ports));

  ## mkLegacyTopology
  ##   :: Map (NodeName String) NodeSpec
  ##   -> Topology FilePath
  mkLegacyTopology =
    topoSpec:
    let
      mkTopo =
        {
          nodes = mapAttrs (name: { port, static-routes, type ? "core", public ? false }: {
            inherit port type public static-routes;
            host = "${name}.cardano";
            kademlia = false;
            region = "eu-central-1";
            zone = "eu-central-1a";
            org = "IOHK";
          }) topoSpec;
        };
    in toFile "legacy-topology.json" (toJSON (mkTopo));
  
  ## mkFullyConnectedLocalClusterTopology
  ##   :: String Address -> String Port -> Int PortNo -> Int Valency
  ##   -> Topology FilePath
  mkFullyConnectedLocalClusterTopologyWithProxy =
    { addr-fn
    , port-base
    , node-id-base
    , node-count
    , valency ? 1
    , proxy-addr
    , proxy-port
    }:
    let
      ## we choose not to spread ports for this topology
      shelley-ids = range                      node-id-base (node-id-base + node-count - 1);
      mkPeer = id: { inherit valency; port = port-base + id; addr = addr-fn id; };
      mkShelleyNode = id: {
        nodeId = id;
        nodeAddress = { port = port-base + id; addr = addr-fn id; };
        producers = map (mkPeer) (remove id shelley-ids) ++
                    [ { inherit valency; port = proxy-port; addr = proxy-addr; } ];
      };
      topology = map mkShelleyNode shelley-ids;
    in toFile "topology.yaml" (toJSON topology);

  # Note how some values are literal strings, and some integral.
  # This is an important detail.
  #
  ## defaultGenesisProtocolParams
  ##   :: ProtocolParams
  defaultGenesisProtocolParams = {
    heavyDelThd = "300000000000";
    maxBlockSize = "2000000";
    maxHeaderSize = "2000000";
    maxProposalSize = "700";
    maxTxSize = "4096";
    mpcThd = "20000000000000";
    scriptVersion = 0;
    slotDuration = "20000";
    softforkRule = {
        initThd = "900000000000000";
        minThd = "600000000000000";
        thdDecrement = "50000000000000";
    };
    txFeePolicy = {
        multiplier = "43946000000";
        summand = "155381000000000";
    };
    unlockStakeEpoch = "18446744073709551615";
    updateImplicit = "10000";
    updateProposalThd = "100000000000000";
    updateVoteThd = "1000000000000";
  };

  ## defaultGenesisArgs
  ##   :: CLIArgs "cardano-cli"
  defaultGenesisArgs = {
    protocol_params_file  = toFile "genesis-protocol-params.json" (toJSON defaultGenesisProtocolParams);
    k                     = 2160;
    protocol_magic        = 314159265;
    n_poors               = 128;
    n_delegates           = 3;
    total_balance         = 8000000000000000;
    delegate_share        = 900000000000000;
    avvm_entries          = 128;
    avvm_entry_balance    = 10000000000000;
    secret_seed           = 271828182;
  };

  ## mkFixedGenesisOfDate
  ##   :: String Date -> FilePath (Genesis Legacy)
  mkFixedGenesisOfTime = start_time: args:
    pkgs.runCommand "genesis-of-${start_time}" {} ''
      args=(
      --real-pbft
      --log-config ${../configuration/log-configuration.yaml}
      genesis
      --genesis-output-dir         "''${out}"
      --start-time                    ${start_time}
      --avvm-entry-balance            ${toString args.avvm_entry_balance}
      --avvm-entry-count              ${toString args.avvm_entries}
      --delegate-share                ${toString args.delegate_share}
      --k                             ${toString args.k}
      --n-delegate-addresses          ${toString args.n_delegates}
      --n-poor-addresses              ${toString args.n_poors}
      --protocol-magic                ${toString args.protocol_magic}
      --protocol-parameters-file     "${args.protocol_params_file}"
      --secret-seed                   ${toString args.secret_seed}
      --total-balance                 ${toString args.total_balance}
      --use-hd-addresses
      )
      ${cardano-node}/bin/cardano-cli "''${args[@]}"
    '';

  # This value will change every given amount of seconds.
  ## periodicNewsTimestamp
  ##   :: Int Seconds -> String Seconds
  periodicNewsTimestamp = period:
    toString ((builtins.currentTime / period) * period);

  # We need this to have a balance of:
  #  1. moderate amount of rebuilds
  #  2. small chain length for quick validation
  ## genesisUpdatePeriod
  ##   :: Int Seconds
  genesisUpdatePeriod = 600;

  ## mkPeriodicGenesis
  ##  :: CLIArgs "cardano-cli" -> FilePath (GenesisDir Rewrite)
  mkPeriodicGenesisDir = mkFixedGenesisOfTime (periodicNewsTimestamp genesisUpdatePeriod);

  ## leakDelegateSigningKey
  ##  :: FilePath ByronLegacyKey -> FilePath ByronRewriteKey
  #
  # XXX:  DO NOT USE IN PRODUCTION
  leakDelegateSigningKey = byronLegacySK:
    pkgs.runCommand "migrated-leaked-secret-key.sk" {} ''
      args=(
      --real-pbft
      --log-config ${../configuration/log-configuration.yaml}
      migrate-delegate-key-from --byron-legacy
      --from ${byronLegacySK}
      --to $out
      )
      ${cardano-node}/bin/cardano-cli "''${args[@]}"
    '';

  ## toVerification
  ##   :: FilePath SigningKey -> FilePath VerificationKey
  toVerification = pbftSK:
    pkgs.runCommand "key.pub" {} ''
      args=(
      --real-pbft
      --log-config ${../configuration/log-configuration.yaml}
      signing-key-public
      --secret ${pbftSK}
      )
      ${cardano-node}/bin/cardano-cli "''${args[@]}" | fgrep 'public key (base64):' | cut -d: -f2 | xargs echo -n > $out
    '';

  ## extractDelegateCertificate
  ##   :: FilePath (Genesis a) -> String DelegatePK -> FilePath DelegationCert
  extractDelegateCertificate = genesisJson: delegatePk:
    pkgs.runCommand "delegate.crt" {} ''
      args=(
      '.heavyDelegation | .[] | select(.delegatePk == "${delegatePk}")'
      ${genesisJson}
      )
      ${pkgs.jq}/bin/jq "''${args[@]}" > $out
    '';

  ## genesisHash
  ##   :: FilePath (Genesis a) -> FilePath GenesisHash
  genesisHash = genesisFile:
    pkgs.runCommand "genesis-hash" {}
    ''
      args=(
      --real-pbft
      --log-config ${../configuration/log-configuration.yaml}
      print-genesis-hash
      --genesis-json "${genesisFile}"
      )
      ${cardano-node}/bin/cardano-cli "''${args[@]}" | egrep '^[0-9a-f]{64,64}$' | xargs echo -n > $out
    '';

  ## mkChairmanScript
  ##   :: ChairmanConfig -> ClusterConfig -> String ShellCmd
  mkChairmanScript =
    { chairman-config ? {
        enable = true;
      }
    , cardano-cluster-config ? {
        enable = true;
        legacy-node-count = 4;
        shelley-node-count = 3;
        genesis-dir = "${../configuration/b0109}";
      }
    ## NOTE:  we do the above defaulting, so the following works, FWIW
    ##    nix-build -A scripts.mainnet.chairman
    }:
  let
    injectServiceConfigs = {
      config.services.chairman = chairman-config;
      config.services.cardano-cluster = cardano-cluster-config;
    };
    pkgsModule = {
      config._module.args.pkgs = mkDefault pkgs;
    };
    systemdCompat.options = {
      systemd.services = mkOption {};
      assertions = [];
      users = mkOption {};
    };
    script = (modules.evalModules {
      modules = [
        ((import ./sources.nix).cardano-byron-proxy + "/nix/nixos")
        ./nixos/cardano-cluster-service.nix
        ./nixos/cardano-node-legacy-service.nix
        ./nixos/cardano-node-service.nix
        ./nixos/chairman-as-a-service.nix
        injectServiceConfigs
        pkgsModule
        systemdCompat
      ];
    }).config.services.chairman.script;
  in pkgs.writeScript "chairman" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${script} $@
  '';

  ## mkProxyFollowerTopology
  ##   :: TopologyArgs -> FilePath (Topology Rewrite)
  mkProxyFollowerTopology =
    with builtins;
    { hostAddr
    , proxyPort
    , nodePort
    }:
    let
      topology = [{
        nodeAddress = {
          addr = hostAddr;
          port = nodePort;
        };
        producers = [
          { addr = hostAddr;
            port = proxyPort;
            valency = 1;
          }
        ];
      }];
    in toFile "proxy-follower-topology.yaml" (toJSON topology);

  ## mkProxyScript
  ##   :: ProxyOptions -> String ShellCmd
  mkProxyScript =
    { cfg, cardanoConfig, configurationKey, producers }:
    ''
      args=(
        +RTS -T -RTS
        --database-path ${cfg.stateDir}/db
        --index-path ${cfg.stateDir}/index
        --configuration-file ${cardanoConfig}/lib/configuration.yaml
        --configuration-key ${configurationKey}
        --topology ${cfg.topologyFile}
        --logger-config ${cfg.logger.configFile}
        --local-addr [${cfg.proxyHost}]:${toString cfg.proxyPort}
        ${optionalString (cfg.pbftThreshold != null) "--pbft-threshold ${cfg.pbftThreshold}"}
        ${optionalString (cfg.nodeId != null) "--node-id ${cfg.nodeId}"}
        ${optionalString (cfg.listen != null) "--listen ${cfg.listen}"}
        ${concatStringsSep " " (map (x: "--producer-addr ${x}") producers)}
      )
      echo "Starting cardano-byron-proxy:  ''${args[@]}"
      exec ${cfg.package}/bin/cardano-byron-proxy "''${args[@]}"
    '';
  # ${optionalString (cfg.address != null) "--address ${cfg.address}"}
  # --disable-peer-host-check
in
{ inherit
  mkFullyConnectedLocalClusterTopology
  mkFullyConnectedLocalClusterTopologyWithProxy
  mkLegacyTopology
  mkPeriodicGenesisDir
  defaultGenesisArgs
  leakDelegateSigningKey
  extractDelegateCertificate
  genesisHash
  mkChairmanScript
  toVerification
  mkProxyFollowerTopology
  mkProxyScript
  mkNodeConfig
  ;
}
