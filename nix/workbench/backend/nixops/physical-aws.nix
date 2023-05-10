let
  ## IFD:
  profile = __fromJSON (__readFile <profileJson>);
  nix = import ../../..
    { localCluster = {
        profileName = profile.name;
        backendName = "nixops";
        useCabalRun = false;
        workbenchDevMode = true;
        workbenchStartArgs = [];
      };
      withHoogle = false;
    };
in
with nix;

let
  inherit (pkgs.lib)
    attrValues attrNames filter filterAttrs flatten foldl' hasAttrByPath listToAttrs
    mapAttrs' mapAttrs nameValuePair recursiveUpdate unique optional any concatMap
    getAttrs optionalString hasPrefix take drop length concatStringsSep head toLower
    elem traceValSeqN;

  inherit (globals.topology) coreNodes relayNodes;
  inherit (globals.ec2.credentials) accessKeyIds;

  cluster = import ./cluster.nix {
    inherit pkgs;
    instances = globals.ec2.instances // { targetEnv = "ec2"; };
  };

  nodes = filterAttrs (name: node: ((node.deployment.targetEnv or null) == "ec2")
                                   && ((node.deployment.ec2.region or null) != null))
                      cluster;

  securityGroups = with iohk-ops-lib.physical.aws.security-groups; [
    {
      nodes = getAttrs (map (n: n.name) coreNodes) nodes;
      groups = [ (import ./sg-allow-peers.nix) ];
    }
    {
      nodes = getAttrs (map (n: n.name) relayNodes) nodes;
      groups = [ (import ./sg-allow-public.nix) ];
    }
    {
      nodes = (filterAttrs (_: n: n.node.roles.isPublicSsh or false) nodes);
      groups = [ allow-ssh ];
    }
    {
      inherit nodes;
      groups = [ allow-deployer-ssh ];
    }
  ];

  securityGroupsByNode =
    let
      importSecurityGroup =  node: securityGroup:
        securityGroup {
          inherit pkgs lib nodes;
          region = node.deployment.ec2.region;
          org = node.node.org;
          accessKeyId = accessKeyIds.${node.node.org};
        };
      importSecurityGroups = {nodes, groups}:
        mapAttrs
          (_: n: foldl' recursiveUpdate {} (map (importSecurityGroup n) groups))
          nodes;
    in
      foldl' recursiveUpdate {} (map importSecurityGroups securityGroups);

in
cluster
//
{
  defaults = { name, resources, config, ... }: {
    deployment.hasFastConnection = true;
    deployment.ec2 = {
      keyPair = resources.ec2KeyPairs."cardano-keypair-${config.node.org}-${config.deployment.ec2.region}";
      securityGroups = map (sgName: resources.ec2SecurityGroups.${sgName})
        (attrNames (securityGroupsByNode.${name} or {}));
    };
  };

  resources = {
    ec2SecurityGroups =
      foldl' recursiveUpdate {} (attrValues securityGroupsByNode);

    elasticIPs = mapAttrs' (name: node:
      nameValuePair "${name}-ip" {
        accessKeyId = accessKeyIds.${node.node.org};
        inherit (node.deployment.ec2) region;
      }) nodes;

    ec2KeyPairs =
      let
        regions = unique(map(node: node.deployment.ec2.region) (attrValues nodes));
        orgs    = unique(map(node: node.node.org)              (attrValues nodes));
      in
      listToAttrs (concatMap (region:
      map (org:
        nameValuePair "cardano-keypair-${org}-${region}" {
          inherit region;
          accessKeyId = accessKeyIds.${org};
        }
      ) orgs)
      regions);

    route53RecordSets = lib.optionalAttrs globals.withSmash {
      "smash-explorer-alias" = { resources, ... }: {
          zoneName = "${pkgs.globals.dnsZone}.";
          domainName = "smash.${globals.domain}.";
          recordValues = [ resources.machines.explorer ];
          recordType = "A";
          accessKeyId = pkgs.globals.ec2.credentials.accessKeyIds.dns;
        };
    } // (
      let mkRelayRecords = prefix: let
        relaysNewPrefix = "${prefix}${optionalString (prefix != "") "-"}relays-new";
      in relayFilter: listToAttrs (map (relay:
        nameValuePair "${relaysNewPrefix}-${relay.name}" (
        { resources, ... }: {
          zoneName = "${pkgs.globals.dnsZone}.";
          domainName = "${prefix}${optionalString (prefix != "") "."}${pkgs.globals.relaysNew}.";
          recordValues = [ resources.machines.${relay.name} ];
          recordType = "A";
          setIdentifier = relay.name;
          routingPolicy = "multivalue";
          accessKeyId = pkgs.globals.ec2.credentials.accessKeyIds.dns;
        })
        # AWS records are limited to 200 values:
      ) (let relays = filter (r: (r.public or true) && relayFilter r) relayNodes;
        numberOfRelays = length relays;
      in if (numberOfRelays > 200) then builtins.trace
        "WARNING: Getting over the 200 values limit for ${relaysNewPrefix} dns entry (${toString numberOfRelays} relays). Excluding ${concatStringsSep " " (map (r: r.name) (drop 200 relays))}."
        (take 200 relays)
      else relays));
      in mkRelayRecords "" (_: true)
        // mkRelayRecords "asia-pacific" (n: hasPrefix "ap" n.region)
        // mkRelayRecords "north-america" (n: hasPrefix "us" n.region)
        // mkRelayRecords "europe" (n: hasPrefix "eu" n.region)
        // (
          let records = map (coreNode: if coreNode ? ticker
            then mkRelayRecords (toLower coreNode.ticker) (r: elem coreNode.name r.producers)
            else {}
          ) coreNodes;
          in foldl' (a: b: a // b) {} records));

  };
}
