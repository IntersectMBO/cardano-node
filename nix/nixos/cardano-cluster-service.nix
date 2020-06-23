{ config
, lib
, pkgs
, ... }:

let
  ### Static configuration to be moved to proper options
  legacy-relay-enabled  = false;
in
with lib; with builtins;
let
  ### Service configs
  ccfg = config.services.cardano-cluster;
  ncfg = config.services.cardano-node;
  pcfg = config.services.byron-proxy;
  lcfg = config.services.cardano-node-legacy;

in let
  ### Packages and Nix libs
  cardano-node          = ncfg.package;
  cardano-sl-pkgs       = import ccfg.cardano-sl-src { gitrev = ccfg.cardano-sl-src.rev; };
  inherit (pkgs) svcLib commonLib;

in let
  ### Node enumeration
  inherit (ccfg)
    legacy-node-count
    shelley-node-count;
  total-node-count      = legacy-node-count + shelley-node-count;
  legacy-relay-id       = 7;
  legacy-node-ids       = range 0                     (legacy-node-count - 1);
  legacy-node-ids-str   = map toString legacy-node-ids;
  first-shelley-node-id = legacy-node-count;
  shelley-node-ids      = range first-shelley-node-id (total-node-count  - 1);
  shelley-node-ids-str  = map toString shelley-node-ids;
  legacy-enabled        = legacy-node-count != 0;
  shelley-enabled       = shelley-node-count != 0;

in let
  ### Addresses & ports
  addr-fn               = id: "127.1.0.${toString (id + 1)}";
  port-base             = 3001;
  first-shelley-node-port = port-base + first-shelley-node-id;
  legacy-port           = "$((${toString port-base} + $1))"; # 3000;
  shelley-port          = "$((${toString port-base} + $1))"; # 3001;
  proxy-addr            = "127.1.0.9";
  proxy-port-byron      = 5555;
  proxy-port-shelley    = 7777;

in let
  configuration-key     = "mainnet_ci_full";
  ### Names and topologies
  topology-core-names   = [ "c-a-1" "c-a-2" "c-b-1" "c-b-2" "c-c-1" "c-c-2" "c-d-1" ];
  legacy-core-names     = take legacy-node-count topology-core-names;
  shelley-core-names    = take shelley-node-count (drop legacy-node-count topology-core-names);
  legacy-relay-name     = "r-a-1";
  all-legacy-names      = topology-core-names
                          ++ optional legacy-relay-enabled legacy-relay-name;
  legacy-name-shexpr    = ''$(choice "$1" ${toString all-legacy-names})'';

  legacy-topology       =
    svcLib.mkFullyConnectedLocalClusterLegacyTopologyWithProxy
      { inherit port-base;
        node-names = legacy-core-names;
        proxy-connected-node-name = head legacy-core-names;
        proxy-name = "proxy";
        proxy-port = 5555;
      };

in let
  ### Genesis & state dirs
  genesis-dir           = ccfg.genesis-dir;
  genesisFile           = "${genesis-dir}/genesis.json";
  genesisHashPath       = svcLib.genesisHash genesisFile;
  node-root-dir         = "/var/lib/cardano-node";
  shelley-state-dir     = "${node-root-dir}/${legacy-name-shexpr}";
  shelley-database-paths = map (i: "${node-root-dir}/db-${i}") shelley-node-ids-str;
  shelley-socket-paths   = map (i: "${node-root-dir}/node-${i}.socket") shelley-node-ids-str;
  database-path-shexpr  = ''$(choice "$1" ${toString legacy-node-ids-str} ${toString shelley-database-paths})'';
  socket-path-shexpr    = ''$(choice "$1" ${toString legacy-node-ids-str} ${toString shelley-socket-paths})'';

in let
  ### Secrets
  legacy-sig-keys       = map (i: "${genesis-dir}/key${i}.sk")
                          legacy-node-ids-str;
  legacy-sig-key-shexpr = ''$(choice "$1" ${toString legacy-sig-keys} ${toString shelley-node-ids-str})'';
  migrated-pbft-keys    = map (i: svcLib.leakDelegateSigningKey "${genesis-dir}/key${i}.sk")
                          shelley-node-ids-str;
  ## WARNING:  don't reuse without understanding security implications. -- sk
  pbft-sig-key-shexpr   = ''$(choice "$1" ${toString legacy-node-ids-str} ${toString migrated-pbft-keys})'';

in let
  ### Delegation certs
  pbft-ver-keys         = map svcLib.toVerification migrated-pbft-keys;
  delegate-certs        = map (pk: svcLib.extractDelegateCertificate genesisFile pk)
                          pbft-ver-keys;
  pbft-cert-shexpr      = ''$(choice "$1" ${toString legacy-node-ids-str} ${toString delegate-certs})'';

in let
  ### Topology
  shelley-topology      = svcLib.mkFullyConnectedLocalClusterTopologyWithProxy
    { inherit proxy-addr addr-fn port-base;
      proxy-port   = proxy-port-shelley;
      node-id-base = legacy-node-count;
      node-count   = shelley-node-count;
    };
  mkProxyShelleyPeer    =
    { name, port }:
    "[${name}.cardano]:${toString port}";
  proxy-shelley-peers   =
    optional shelley-enabled { name = elemAt shelley-core-names 0; port = first-shelley-node-port; };

in let
  ### Config
  node-config-overlay   = {
    hasEKG                  = null;
    hasGUI                  = null;
    hasGraylog              = null;
    hasPrometheus           = null;
    minSeverity             = "Debug";
    TraceChainDb            = true;
    TracingVerbosity        = "MaximalVerbosity";
  };
  shelley-configs       = map (i: toFile "config-${toString i}.json" (toJSON (svcLib.mkNodeConfig ncfg i // node-config-overlay)))
                              shelley-node-ids;
  config-shexpr         = ''$(choice "$1" ${toString legacy-node-ids-str} ${toString shelley-configs})'';
in {
  ###
  ### Cluster configuration options:
  ###
  options = with types; {
    services.cardano-cluster = {
      enable = mkOption {
        type = bool;
        default = false;
        description = ''
          Enable cardano-node, a node implementing a consensus protocol from the Ouroboros family.
        '';
      };
      legacy-node-count = mkOption {
        type = int;
        default = 0;
        description = ''
          Size of the cluster's Byron Legacy segment.
        '';
      };
      shelley-node-count = mkOption {
        type = int;
        default = 3;
        description = ''
          Size of the cluster's Shelley segment.
        '';
      };
      genesis-dir = mkOption {
        type = str;
        description = ''Directory with: 1) genesis.json, 2) legacy keys in key{N}.sk.'';
      };
      system-start = mkOption {
        type = int;
        default = 1000000000; # Corresponds to mainnet-ci genesis.
      };
      slot-length = mkOption {
        type = int;
        default = 20;
        description = ''
          Slot length
        '';
      };
      cardano-sl-config = mkOption {
        type = path;
        description = "Configuration files of 'cardano-sl'.";
      };
      cardano-sl-src = mkOption {
        type = path;
        default = commonLib.sources.cardano-sl;
        description = "Source of 'cardano-sl'.";
      };
    };
  };

  ###
  ### Configure cluster's constituent services:
  ###
  config = mkIf ccfg.enable {
    services.chairman = {
      node-ids              = shelley-node-ids;
      inherit (ccfg) slot-length;
      nodeConfigFile        = builtins.elemAt shelley-configs 0;
    };
    services.cardano-node = {
      enable                = shelley-enabled;
      instanced             = true;
      topology              = shelley-topology;
      port                  = toString shelley-port;
      hostAddr              = "127.1.0.$((1 + $1))";
      inherit genesisFile;
      genesisHash           = null;
      genesisHashPath       = genesisHashPath;
      delegationCertificate = pbft-cert-shexpr;
      signingKey            = pbft-sig-key-shexpr;
      runtimeDir            = null;
      nodeConfigFile        = config-shexpr;
      databasePath          = database-path-shexpr;
      socketPath            = socket-path-shexpr;
      environment           = "mainnet";
      protover-major        = 0;
      protover-minor        = 0;
      protover-alt          = 0;
    };
    services.byron-proxy = {
      enable                = true;
      environment           = "mainnet";
      topologyFile          = legacy-topology;
      proxyHost             = proxy-addr;
      proxyPort             = proxy-port-shelley;
      nodeId                = "proxy";
      listen                = "${proxy-addr}:${toString proxy-port-byron}";
      address               = "proxy.cardano:${toString proxy-port-byron}";
      script                = svcLib.mkProxyScript {
        cfg              = pcfg;
        cardanoConfig    = ccfg.cardano-sl-config;
        configurationKey = "mainnet_ci_full";
        producers        = map mkProxyShelleyPeer proxy-shelley-peers;
      };
    };
    systemd.services.byron-proxy = {
      ## Prevent automatic start, so it's subject to the synchronous start arrangements.
      after                 = mkForce [];
      wantedBy              = mkForce [];
    };
    services.cardano-node-legacy = {
      enable                = legacy-enabled;
      instanced             = true;
      topologyYaml          = legacy-topology;
      configurationKey      = configuration-key;
      systemStart           = ccfg.system-start;
      port                  = legacy-port;
      publicIP              = null;
      privateIP             = "127.1.0.$((1 + $1))";
      name                  = legacy-name-shexpr;
      keyFile               = legacy-sig-key-shexpr;
      nodeIndex             = 0;          ## doesn't really matter
      nodeType              = "core";     ## ...
      cardanoConfig         = ccfg.cardano-sl-config;
      source                = ccfg.cardano-sl-src;
    };

    systemd.services."cardano-node@" = {
      scriptArgs = "%i ${genesisFile}";
    };

    systemd.services.cardano-cluster =
      let shelley-services = map (i:        "cardano-node@${i}.service") shelley-node-ids-str;
          legacy-services  = map (i: "cardano-node-legacy@${i}.service") (legacy-node-ids-str
                                                                          ++ optional legacy-relay-enabled
                                                                            (toString legacy-relay-id));
      in {
        description = "Cluster of cardano nodes.";
        enable  = true;
        after   = shelley-services ++ legacy-services ++ ["byron-proxy.service"];
        bindsTo = shelley-services ++ legacy-services ++ ["byron-proxy.service"];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.coreutils}/bin/echo Starting a mixed cluster of ${toString shelley-node-count} Shelley nodes & ${toString (legacy-node-count + 1)} Legacy nodes;  topologies: Shelley ${shelley-topology}, Legacy ${legacy-topology}";
          User = "cardano-node";
          Group = "cardano-node";
          RuntimeDirectory = node-root-dir;
          WorkingDirectory = node-root-dir;
          # This assumes /var/lib/ is a prefix of cfg.stateDir.
          # This is checked as an assertion below.
          StateDirectory   = node-root-dir;
        };
      };
  };
}
