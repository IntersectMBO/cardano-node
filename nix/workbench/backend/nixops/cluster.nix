{ pkgs
, instances
, ...
}:
with pkgs;
with lib;
let

  mkRoleTopoEntry = argsRole: topoEntry:
    recursiveUpdate
      (recursiveUpdate
        argsRole
        {
          _file = ./cluster.nix;
          imports = (argsRole.imports or []) ++ (topoEntry.imports or []);

          nixpkgs.pkgs = pkgs;

          deployment = {
            targetEnv = instances.targetEnv;
            ec2.region = topoEntry.region;
          };

          node = { inherit (topoEntry) org nodeId; };

          services.cardano-node.allProducers = topoEntry.producers;
        })
      (builtins.removeAttrs topoEntry [
        "name"
        "org"
        "region"
        "nodeId"
        "producers"
        "staticRoutes"
        "dynamicSubscribe"
        "stakePool"
        "instance"
        "pools"
        "ticker"
        "public"
      ]);

  mkRelay = topoEntry:
  {
    inherit (topoEntry) name;

    value = mkRoleTopoEntry {
      imports = [
        (topoEntry.instance or instances.relay-node)
        cardano-ops.roles.relay
      ];
      node.roles.isCardanoRelay = true;
      node.roles.class = "relay";
    } topoEntry;
  };

  mkExplorer = topoEntry:
  {
    inherit (topoEntry) name;

    value = mkRoleTopoEntry{
      imports = [
        (topoEntry.instance or instances.relay-node)
        cardano-ops.roles.explorer
      ];
      node.roles.isCardanoRelay = true;
      node.roles.class = "relay";
    } topoEntry;
  };

  mkPool = topoEntry:
  {
    inherit (topoEntry) name;

    value = mkRoleTopoEntry {
      imports = [
        (topoEntry.instance or instances.core-node)
        (cardano-ops.roles.pool topoEntry.nodeId)
      ];
      node.roles.isCardanoPool = true;
      node.roles.class = "pool";
      security.wrappers.unix_chkpwd.source = mkForce "${pkgs.pam}/bin/unix_chkpwd";
    } topoEntry;
  };

in {
  network.description = globals.networkName;

  environment.systemPackages = with pkgs;
    [ cardano-cli
    ];
}
// listToAttrs (concatLists [
  (map mkPool globals.topology.coreNodes)

  ## Yes, a slightly odd equation.
  # (map mkExplorer globals.topology.relayNodes)
])
