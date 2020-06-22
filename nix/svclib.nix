{ pkgs, cardano-cli-packages ? pkgs.cardanoNodeHaskellPackages.cardano-cli.components.exes }:
with builtins; with pkgs.lib;
let
  inherit (pkgs) commonLib;
  cardano-cli-bin = "${cardano-cli-packages.cardano-cli}/bin/cardano-cli";

  ## mkNodeConfig
  ##   :: ServiceConfig AttrSet -> NodeId Int -> NodeConfig AttrSet
  mkNodeConfig = cfg: NodeId:
    cfg.nodeConfig //
    { inherit NodeId; } //
    (optionalAttrs (cfg.protover-major or null != null) { LastKnownBlockVersion-Major = cfg.protover-major; }) //
    (optionalAttrs (cfg.protover-minor or null != null) { LastKnownBlockVersion-Minor = cfg.protover-minor; }) //
    (optionalAttrs (cfg.protover-alt   or null != null) { LastKnownBlockVersion-Alt   = cfg.protover-alt;   }) //
    (optionalAttrs (cfg.genesisFile != null) { GenesisFile = cfg.genesisFile;   });

  ## mkFullyConnectedLocalClusterTopologyWithProxy
  ##   :: (Int NodeId -> String Address)
  ##   -> Int Port
  ##   -> Int NodeId
  ##   -> Int Count
  ##   -> String Address -> Int PortNo
  ##   -> Topology FilePath
  ##
  ##  Produce a store path containing a Byron Reboot topology
  ##  with following properties:
  ##   - fully connected between the nodes
  ##   - every node lists the proxy as a peer
  mkFullyConnectedLocalClusterTopologyWithProxy =
    { addr-fn
    , port-base
    , node-id-base
    , node-count
    , proxy-addr
    , proxy-port
    , valency ? 1
    }:
    let
      ## we choose not to spread ports for this topology
      shelley-ids = range                      node-id-base (node-id-base + node-count - 1);
      mkPeer = id: { inherit valency; port = port-base + id; addr = addr-fn id; };
      mkShelleyNode = {
        Producers = (map (mkPeer) (remove id shelley-ids))
                    ++ [{ valency = 1; port = proxy-port; addr = proxy-addr; }];
      };
      topology = mkShelleyNode;
    in toFile "topology.yaml" (toJSON topology);

  ## mkFullyConnectedLocalClusterLegacyTopologyWithProxy
  ##   :: Int Port
  ##   -> [String NodeName]
  ##   -> String NodeName
  ##   -> String NodeName -> Int PortNo
  ##   -> Topology FilePath
  ##
  ##  Produce a store path containing a legacy SL topology
  ##  with following properties:
  ##   - fully connected between the nodes
  ##   - the designated node is two-way-connected with the proxy
  mkFullyConnectedLocalClusterLegacyTopologyWithProxy =
  { port-base
  , node-names
  , proxy-connected-node-name
  , proxy-name
  , proxy-port
  , valency ? 1
  }:
  let
    node-specs = imap0 (i: name:
      { inherit name;
        port = port-base + i;
        static-routes = map (x: [x])
                        ((remove name node-names)
                         ++ optional (name == proxy-connected-node-name) "proxy");
      }) node-names;
    proxy-spec =
      { name = proxy-name;
        port = proxy-port;
        static-routes = [ [ proxy-connected-node-name ] ];
      };
    mkNodeTopoFromSpec = type: { name, port, static-routes }: {
        inherit name;
        value = {
          inherit type port static-routes;
          host = "${name}.cardano";
          kademlia = false;
          region = "eu-central-1";
          zone = "eu-central-1a";
          org = "IOHK";
          public = false;
        };
      };
    entries = map (mkNodeTopoFromSpec "core") node-specs
              ++ [(mkNodeTopoFromSpec "relay" proxy-spec)];
    topology = { nodes = listToAttrs entries; };
  in toFile "legacy-topology.json" (toJSON topology);

  # Note how some values are literal strings, and some integral.
  # This is an important detail.
  #
  ## defaultByronGenesisProtocolParams
  ##   :: ByronProtocolParams
  defaultByronGenesisProtocolParams = {
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

  ## defaultByronGenesisArgs
  ##   :: CLIArgs "cardano-cli"
  defaultByronGenesisArgs = {
    protocol_params_file  = toFile "byron-genesis-protocol-params.json" (toJSON defaultByronGenesisProtocolParams);
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

  ## mkFixedByronGenesisOfDate
  ##   :: String Date -> FilePath (Genesis Legacy)
  mkFixedByronGenesisOfTime = start_time: args:
    pkgs.runCommand "byron-genesis-of-${start_time}" {} ''
      args=(
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
      --real-pbft
      --secret-seed                   ${toString args.secret_seed}
      --total-balance                 ${toString args.total_balance}
      --use-hd-addresses
      )
      ${cardano-cli-bin} "''${args[@]}"
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
  mkPeriodicGenesisDir = mkFixedByronGenesisOfTime (periodicNewsTimestamp genesisUpdatePeriod);

  ## leakDelegateSigningKey
  ##  :: FilePath ByronLegacyKey -> FilePath ByronRewriteKey
  #
  # XXX:  DO NOT USE IN PRODUCTION
  leakDelegateSigningKey = byronLegacySK:
    pkgs.runCommand "migrated-leaked-secret-key.sk" {} ''
      args=(
      migrate-delegate-key-from
      --byron-legacy
      --from ${byronLegacySK}
      --real-pbft
      --to $out
      )
      ${cardano-cli-bin} "''${args[@]}"
    '';

  ## toVerification
  ##   :: FilePath SigningKey -> FilePath VerificationKey
  toVerification = pbftSK:
    pkgs.runCommand "key.pub" {} ''
      args=(
      signing-key-public
      --real-pbft
      --secret ${pbftSK}
      )
      ${cardano-cli-bin} "''${args[@]}" | fgrep 'public key (base64):' | cut -d: -f2 | xargs echo -n > $out
    '';

  ## extractDelegateCertificate
  ##   :: FilePath (Genesis a) -> FilePath DelegatePK -> FilePath DelegationCert
  extractDelegateCertificate = genesisJson: delegatePk:
    pkgs.runCommand "delegate.crt" {} ''
      PK="$(cat ${delegatePk})"
      args=(
      '.heavyDelegation | .[] | select(.delegatePk == "'$PK'")'
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
      print-genesis-hash
      --genesis-json "${genesisFile}"
      )
      ${cardano-cli-bin} "''${args[@]}" | egrep '^[0-9a-f]{64,64}$' | xargs echo -n > $out
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
      config.services.cardano-node.cardanoNodePkgs = pkgs;
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
        (commonLib.sources.cardano-byron-proxy + "/nix/nixos")
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
  mkFullyConnectedLocalClusterTopologyWithProxy
  mkFullyConnectedLocalClusterLegacyTopologyWithProxy
  mkPeriodicGenesisDir
  defaultByronGenesisArgs
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
