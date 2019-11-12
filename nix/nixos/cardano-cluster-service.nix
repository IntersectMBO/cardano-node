{ config
, lib
, pkgs
, ... }:

with lib; with builtins;
let
  cfg = config.services.cardano-cluster;
  ncfg = config.services.cardano-node;
  node-ids = range 0 (cfg.node-count - 1);
  cardano-node = ncfg.package;

  # mkFullyConnectedLocalClusterTopology
  #   :: Address String -> Port String -> Int -> Int -> Topology FilePath
  mkFullyConnectedLocalClusterTopology =
    { hostAddr
    , portBase
    , node-count
    , valency ? 1
    }:
    let
      addr = hostAddr;
      ports = map (x: portBase + x) (range 0 (node-count - 1));
      mkPeer = port: { inherit addr port valency; };
      mkNodeTopo = nodeId: port: {
        inherit nodeId;
        nodeAddress = { inherit addr port; };
        producers = map mkPeer (remove port ports);
      };
    in toFile "topology.yaml" (toJSON (imap0 mkNodeTopo ports));

  ## Note how some values are literal strings, and some integral.
  ## This is an important detail.
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

  # We need this to have a balance of:
  #  1. moderate amount of rebuilds
  #  2. small chain length for quick validation
  # genesisUpdatePeriod
  #   :: Seconds Int
  genesisUpdatePeriod = 600;

  # mkFixedGenesisOfDate
  #   :: Date String -> Topology FilePath
  mkFixedGenesisOfTime = start_time: args:
    pkgs.runCommand "genesis-of-${start_time}" {} ''
      args=(
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
      )
      ${cardano-node}/bin/cardano-cli --real-pbft --log-config ${../../configuration/log-configuration.yaml} genesis "''${args[@]}"
    '';

  ## This value will change every given amount of seconds.
  periodicNewsTimestamp = period:
    toString ((builtins.currentTime / period) * period);
in
let
  portBase              = 3001;
  genesisDir            = mkFixedGenesisOfTime (periodicNewsTimestamp genesisUpdatePeriod) defaultGenesisArgs;
  genesisFile           = "${genesisDir}/genesis.json";
  genesisHash           = ''$(${cardano-node}/bin/cardano-cli --real-pbft --log-config ${../../configuration/log-configuration.yaml} print-genesis-hash --genesis-json "${genesisFile}" | tail -1)'';
  signingKey            = ''$(printf "%s/delegate-keys.%03d.key"    $(dirname $2) $1)'';
  delegationCertificate = ''$(printf "%s/delegation-cert.%03d.json" $(dirname $2) $1)'';
  nodeId                = "$1";
  port                  = "$((${toString portBase} + $1))";
  topology = mkFullyConnectedLocalClusterTopology
    { inherit (cfg)  node-count;
      inherit (ncfg) hostAddr;
      inherit portBase;
    };
in {
  options = with types; {

    services.cardano-cluster = {
      enable = mkOption {
        type = bool;
        default = false;
        description = ''
          Enable cardano-node, a node implementing ouroboros protocols
          (the blockchain protocols running cardano).
        '';
      };
      node-count = mkOption {
        type = int;
        default = 3;
        description = ''
          Number of nodes in cluster.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.cardano-node = {
      enable = true;
      instanced = true;
      inherit topology genesisFile signingKey delegationCertificate nodeId port;
    };
    systemd.services."cardano-node@" = {
      scriptArgs = "%i ${ncfg.genesisFile}";
    };
    systemd.services.cardano-cluster =
      let node-services = map (id: "cardano-node@${toString id}.service") node-ids;
      in {
        description = "Cluster of cardano nodes.";
        requiredBy = [ "multi-user.target" ];
        enable  = true;
        after   = node-services;
        bindsTo = node-services;
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.coreutils}/bin/echo Starting cluster of ${toString cfg.node-count} nodes, topology ${ncfg.topology}";
        };
      };
  };
}
