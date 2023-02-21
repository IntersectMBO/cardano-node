##
## The "core" role, for block-producing nodes:
##
##  - start from usual node
##  - add just the staking credentials
##
pkgs: nodeId: {config, name, ...}:
with pkgs; with lib;
let

  runDir = commonLib.envOrDefault "WB_RUN_DIR" "run/current";

  # leftPad = number: width: lib.fixedWidthString width "0" (toString number);
  # signingKey = runDir + "/genesis/byron/delegate-keys.${leftPad nodeId 3}.key";
  # delegCert  = runDir + "/genesis/byron/delegation-cert.${leftPad nodeId 3}.json";
  signingKey = runDir + "/genesis/byron/delegate-keys.000.key";
  delegCert  = runDir + "/genesis/byron/delegation-cert.000.json";
  vrfKey     = runDir + "/genesis/node-keys/node-vrf${toString nodeId}.skey";
  kesKey     = runDir + "/genesis/node-keys/node-kes${toString nodeId}.skey";
  opCert     = runDir + "/genesis/node-keys/node${toString nodeId}.opcert";
  bulkCreds  = runDir + "/genesis/node-keys/bulk-${toString nodeId}.creds";

  keySvcs =
    [ "cardano-node-signing-key.service"
      "cardano-node-delegation-cert-key.service"
      "cardano-node-vrf-signing-key.service"
      "cardano-node-kes-signing-key.service"
      "cardano-node-operational-cert-key.service"
    ];

in {
  imports = [
    cardano-ops.modules.base-service
  ];

  users.users.cardano-node.extraGroups = [ "keys" ];

  services.cardano-node = {
    signingKey             = mkForce "/var/lib/keys/cardano-node-signing";
    delegationCertificate  = mkForce "/var/lib/keys/cardano-node-delegation-cert";
    kesKey                 = mkForce "/var/lib/keys/cardano-node-kes-signing";
    vrfKey                 = mkForce "/var/lib/keys/cardano-node-vrf-signing";
    operationalCertificate = mkForce "/var/lib/keys/cardano-node-operational-cert";
  };

  systemd.services.cardano-node = {
    after  = keySvcs;
    wants  = keySvcs;
    partOf = keySvcs;
  };

  deployment.keys =
    let mkKey = key:
          builtins.trace ("${name}: using " + (toString key)) {
            # keyFile = key;
            text    = __readFile key;
            user    = "cardano-node";
            group   = "cardano-node";
            destDir = "/var/lib/keys";
          };
    in {
      ## Byron, can be ignored, thank god.
      # cardano-node-signing          = mkKey signingKey;
      # cardano-node-delegation-cert  = mkKey delegCert;

      ## Shelley
      cardano-node-vrf-signing      = mkKey vrfKey;
      cardano-node-kes-signing      = mkKey kesKey;
      cardano-node-operational-cert = mkKey opCert;
    };
}
