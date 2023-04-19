pkgs:
profileData:
{ options, config, name, nodes, resources,  ... }:
with pkgs; with lib;
let
  inherit (profileData) node-services;

  nodeId = config.node.nodeId;
  cfg = config.services.cardano-node;
  nodePort = globals.cardanoNodePort;
  hostAddr = getListenIp nodes.${name};

  monitoringPort = globals.cardanoNodePrometheusExporterPort;

  getPublicIp = resources: nodes: nodeName:
    resources.elasticIPs."${nodeName}-ip".address or
    (let
      publicIp = nodes.${nodeName}.config.networking.publicIPv4;
    in
      if (nodes.${nodeName}.options.networking.publicIPv4.isDefined && publicIp != null) then publicIp
      else (builtins.trace "No public IP found for node: ${nodeName}" "")
    );
  getStaticRouteIp = resources: nodes: nodeName: resources.elasticIPs."${nodeName}-ip".address
    or (let
      publicIp = nodes.${nodeName}.config.networking.publicIPv4;
      privateIp = nodes.${nodeName}.config.networking.privateIPv4;
    in
      if (nodes.${nodeName}.options.networking.publicIPv4.isDefined && publicIp != null) then publicIp
      else if (nodes.${nodeName}.options.networking.privateIPv4.isDefined && privateIp != null) then privateIp
      else (builtins.trace "No suitable ip found for node: ${nodeName}" "")
    );

  getListenIp = node:
    let ip = node.config.networking.privateIPv4;
    in if (node.options.networking.privateIPv4.isDefined && ip != null) then ip else "0.0.0.0";

  hostName = name: "${name}.cardano";
  staticRouteIp = getStaticRouteIp resources nodes;

  splitProducers = partition (n: nodes ? ${n.addr or n}) cfg.allProducers;
  deployedProducers = splitProducers.right;
  thirdParyProducers = splitProducers.wrong;
  splitDeployed = partition (n: nodes.${n}.config.node.roles.isCardanoPool) deployedProducers;
  coreNodeProducers = splitDeployed.right;
  relayNodeProducers = splitDeployed.wrong;
  splitRelays = partition (r: nodes.${r}.config.deployment.ec2.region == nodes.${name}.config.deployment.ec2.region) relayNodeProducers;
  sameRegionRelays = splitRelays.right;
  otherRegionRelays = splitRelays.wrong;

  cardanoHostList = map (nodeName: {
    name = hostName nodeName;
    ip = staticRouteIp nodeName;
  }) deployedProducers;

  toNormalizedProducerGroup = producers: {
    accessPoints= map (n: {
      address= let a = n.addr or n; in if (nodes ? ${a}) then hostName a else a;
      port = n.port or nodePort;
      valency = n.valency or 1;
    }) producers;
    valency = length producers;
  };

  producerShare = i: producers: let
      indexed = imap0 (idx: node: { inherit idx node;}) producers;
      filtered = filter ({idx, ...}: mod idx cfg.instances == i) indexed;
    in catAttrs "node" filtered;

  intraInstancesTopologies = topology-lib.connectNodesWithin
    cfg.maxIntraInstancesPeers
    (genList (i: {name = i;}) cfg.instances);

  instanceProducers = i: map toNormalizedProducerGroup (filter (g: length g != 0) [
      (concatMap (i: map (p: {
        addr = cfg.ipv6HostAddr;
        port = cfg.port + p;
      }) i.producers) (filter (x: x.name == i) intraInstancesTopologies))
      (producerShare i sameRegionRelays)
      (producerShare (cfg.instances - i - 1) otherRegionRelays)
      (producerShare i coreNodeProducers)
    ]);

  instancePublicProducers = i: map toNormalizedProducerGroup (filter (g: length g != 0) [
    (producerShare (cfg.instances - i - 1) thirdParyProducers)
  ]);

in
{
  imports = [
    cardano-ops.modules.common

    ## Services:
    ../../../nixos
    (import ../../../nixos/cardano-tracer-service.nix pkgs)
  ];

  options = {
    services.cardano-node = {
      publicIp = mkOption { type = types.str; default = staticRouteIp name;};
      allProducers = mkOption {
        default = [];
        type = types.listOf (types.either types.str types.attrs);
        description = ''Static routes to peers.'';
      };
      maxIntraInstancesPeers = mkOption {
        type = types.int;
        default = 5;
      };
    };
  };

  config = {
    services.cardano-node =
      recursiveUpdate
        profileData.node-services.${name}.serviceConfig.value
        {
          enable         = true;
          stateDir       = "/var/lib/cardano-node";
          nodeConfigFile = "/var/lib/cardano-node/config.json";
          topology       = "/var/lib/cardano-node/topology.json";
        };
    # {
    #   enable = true;
    #   systemdSocketActivation = true;
    #   # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html
    #   rtsArgs = [ "-N${toString (cfg.totalCpuCores / cfg.instances)}" "-A16m" "-qg" "-qb" "-M${toString (cfg.totalMaxHeapSizeMbytes / cfg.instances)}M" ];
    #   environment = globals.environmentName;
    #   # cardanoNodePackages = lib.mkDefault cardanoNodePackages;
    #   inherit hostAddr nodeId instanceProducers instancePublicProducers;
    #   ipv6HostAddr = mkIf (cfg.instances > 1) "::1";
    #   producers = mkDefault [];
    #   publicProducers = mkDefault [];
    #   port = nodePort;
    #   environments = {
    #     "${globals.environmentName}" = globals.environmentConfig;
    #   };
    #   nodeConfig = serviceConfig.finaliseNodeConfig
    #                  globals.profile.value.node.withNewTracing
    #                  globals.environmentConfig.nodeConfig;
    #   extraNodeConfig =
    #     finaliseNodeConfig
    #       globals.profile.value.node.withNewTracing
    #   {
    #     hasPrometheus = [ cfg.hostAddr globals.cardanoNodePrometheusExporterPort ];
    #     # The maximum number of used peers when fetching newly forged blocks:
    #     MaxConcurrencyDeadline = 4;
    #     # Use Journald output:
    #     setupScribes = [{
    #       scKind = "JournalSK";
    #       scName = "cardano";
    #       scFormat = "ScText";
    #     }];
    #     defaultScribes = [
    #       [
    #         "JournalSK"
    #         "cardano"
    #       ]
    #     ];
    #     # TraceMempool makes cpu usage x3, disabling by default:
    #     TraceMempool = false;
    #   };
    #   extraServiceConfig = _: {
    #     serviceConfig = {
    #       # Allow time to uncompress when restoring db
    #       TimeoutStartSec = "1h";
    #       MemoryMax = "${toString (1.15 * cfg.totalMaxHeapSizeMbytes / cfg.instances)}M";
    #       LimitNOFILE = "65535";
    #     };
    #   };
    # }

    # systemd.services.cardano-node = {
    #   path = [ gnutar gzip ];
    #   preStart = ''
    #     cd $STATE_DIRECTORY
    #     if [ -f db-restore.tar.gz ]; then
    #       rm -rf db-${globals.environmentName}*
    #       tar xzf db-restore.tar.gz
    #       rm db-restore.tar.gz
    #     fi
    #   '';
    #   serviceConfig = {
    #     # Allow time to uncompress when restoring db
    #     TimeoutStartSec = "1h";
    #   };
    # }

    services.cardano-tracer =
      profileData.tracer-service.serviceConfig.value;
    # {
    #   enable = true;
    #   acceptingSocket = (cfg.stateDir 0) + "/tracer.socket";
    #   logRoot = cfg.stateDir 0;
    #   rotation.rpLogLimitBytes = 1000 * 1000 * 1000;
    # }

    systemd.services.cardano-tracer.serviceConfig.Environment = [("HOME=" + cfg.stateDir 0)];

    deployment.ec2.ebsInitialRootDiskSize =
      globals.systemDiskAllocationSize + globals.nodeDbDiskAllocationSize;

    environment.variables = globals.environmentVariables // {
      CARDANO_NODE_SOCKET_PATH = cfg.socketPath 0;
    };

    networking.firewall = {
      allowedTCPPorts = [ nodePort ];

      # TODO: securing this depends on CSLA-27
      # NOTE: this implicitly blocks DHCPCD, which uses port 68
      allowedUDPPortRanges = [ { from = 1024; to = 65000; } ];
    };

    users.users.cardano-node.isSystemUser = true;
    users.users.cardano-node.group = "cardano-node";
    users.groups.cardano-node = {};

    services.dnsmasq.enable = true;
    networking.dhcpcd.enable = true;

    networking.extraHosts = ''
        ${concatStringsSep "\n" (map (host: "${host.ip} ${host.name}") cardanoHostList)}
    '';
  };
}
