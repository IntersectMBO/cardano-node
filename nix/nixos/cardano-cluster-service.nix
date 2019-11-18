{ config
, lib
, pkgs
, ... }:

with lib; with builtins;
let
  ccfg = config.services.cardano-cluster;
  ncfg = config.services.cardano-node;
  pcfg = config.services.byron-proxy;
  lcfg = config.services.cardano-node-legacy;
in let
  cardano-node          = ncfg.package;
  cardano-sl-pkgs       = import ccfg.cardano-sl-src { gitrev = ccfg.cardano-sl-src.rev; };
  svcLib                = (import ./svclib.nix { inherit pkgs cardano-node; });
in let
  inherit (ccfg)
    legacy-node-count
    shelley-node-count;
  total-node-count      = legacy-node-count + shelley-node-count;
  legacy-relay-id       = 7;
  legacy-node-ids       = map toString (range 0                     (legacy-node-count - 1));
  first-shelley-node-id = legacy-node-count;
  shelley-node-ids      = map toString (range first-shelley-node-id (total-node-count  - 1));
  legacy-enabled        = legacy-node-count != 0;
  shelley-enabled       = shelley-node-count != 0;
in let
  addr-fn               = id: "127.1.0.${toString (id + 1)}";
  port-base             = 3001;
  first-shelley-node-port = port-base + first-shelley-node-id;
  legacy-port           = "$((${toString port-base} + $1))"; # 3000;
  shelley-port          = "$((${toString port-base} + $1))"; # 3001;
  proxy-addr            = "127.1.0.9";
  proxy-port-byron      = 5555;
  proxy-port-shelley    = 7777;
in let
  legacy-topology       = ../../configuration/mainnet-ci/topology-mainnet-ci.yaml;
  configuration-key     = "mainnet_ci_full";
  ## the below is hard-coded to the names from the above
  topology-core-names   = [ "c-a-1" "c-a-2" "c-b-1" "c-b-2" "c-c-1" "c-c-2" "c-d-1" ];
  legacy-core-names     = take legacy-node-count topology-core-names;
  shelley-core-names    = take shelley-node-count (drop legacy-node-count topology-core-names);
  legacy-relay-name     = "r-a-1";
  all-legacy-names      = topology-core-names ++ [legacy-relay-name];
  legacy-name-shexpr    = ''$(choice "$1" ${concatStringsSep " " all-legacy-names})'';
in let
  genesis-dir           = ccfg.genesis-dir;
  genesisFile           = "${genesis-dir}/genesis.json";
  genesis-hash          = readFile (svcLib.genesisHash genesisFile);
  node-root-dir         = "/var/lib/cardano-node";
  shelley-state-dir     = "${node-root-dir}/${legacy-name-shexpr}";
in let
  legacy-sig-keys       = map (i: "${genesis-dir}/key${i}.sk")
                          legacy-node-ids;
  legacy-sig-key-shexpr = ''$(choice "$1" ${concatStringsSep " " legacy-sig-keys} ${concatStringsSep " " shelley-node-ids})'';
  migrated-pbft-keys    = map (i: svcLib.leakDelegateSigningKey "${genesis-dir}/key${i}.sk")
                          shelley-node-ids;
  ## WARNING:  don't reuse without understanding security implications. -- sk
  pbft-sig-key-shexpr   = ''$(choice "$1" ${concatStringsSep " " legacy-node-ids} ${concatStringsSep " " migrated-pbft-keys})'';
in let
  pbft-ver-keys         = map svcLib.toVerification migrated-pbft-keys;
  delegate-certs        = map (pk: svcLib.extractDelegateCertificate genesisFile (readFile pk))
                          pbft-ver-keys;
  pbft-cert-shexpr      = ''$(choice "$1" ${concatStringsSep " " legacy-node-ids} ${concatStringsSep " " delegate-certs})'';
in let
  shelley-topology      = svcLib.mkFullyConnectedLocalClusterTopologyWithProxy
    { inherit proxy-addr addr-fn port-base;
      proxy-port   = proxy-port-shelley;
      node-id-base = legacy-node-count;
      node-count   = shelley-node-count;
    };
  mkProxyShelleyPeer    =
    { name, port }:
    "[${name}.cardano]:${toString port}";
  mkProxyLegacyPeer     =
    { name, port }:
    "[{ host: ${name}.cardano, port: ${toString port} }]";
  proxy-shelley-peers    =
    optional shelley-enabled { name = elemAt shelley-core-names 0; port = first-shelley-node-port; };
  proxy-legacy-peers    =
    optional  legacy-enabled { name = legacy-relay-name;           port = legacy-port; };
in {
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
        default = 2;
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
        default = (import ../sources.nix).cardano-sl;
        description = "Source of 'cardano-sl'.";
      };
    };
  };

  config = mkIf ccfg.enable {
    services.chairman = {
      enable                = shelley-enabled;
      node-ids              = range legacy-node-count (legacy-node-count + shelley-node-count - 1);
      inherit (ccfg) slot-length;
    };
    services.cardano-node = {
      enable                = shelley-enabled;
      instanced             = true;
      topology              = shelley-topology;
      port                  = toString shelley-port;
      hostAddr              = "127.1.0.$((1 + $1))";
      genesisHash           = genesisHashValue;
      genesisHash           = genesis-hash;
      delegationCertificate = pbft-cert-shexpr;
      signingKey            = pbft-sig-key-shexpr;
      nodeId                = "$1";
      runtimeDir            = null;
      inherit genesisFile;
      environment           = "mainnet-ci";
      protover-major        = 0;
      protover-minor        = 0;
      protover-alt          = 0;
    };
    services.byron-proxy = {
      enable                = true;
      environment           = "mainnet-ci";
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
      let shelley-services = map (id:        "cardano-node@${id}.service") shelley-node-ids;
          legacy-services  = map (id: "cardano-node-legacy@${id}.service") (legacy-node-ids
                                                                            ++ [(toString legacy-relay-id)]);
      in {
        description = "Cluster of cardano nodes.";
        enable  = true;
        after   = shelley-services ++ legacy-services;
        bindsTo = shelley-services ++ legacy-services;
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
