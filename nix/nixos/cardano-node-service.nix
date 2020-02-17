{ config
, lib
, pkgs
, ... }:

with lib; with builtins;
let
  localPkgs = import ../. {};
  cfg = config.services.cardano-node;
  inherit (localPkgs) svcLib commonLib cardanoNodeHaskellPackages;
  envConfig = cfg.environments.${cfg.environment}; systemdServiceName = "cardano-node${optionalString cfg.instanced "@"}";
  mkScript = cfg:
    let exec = "cardano-node run";
        runtimeDir = if cfg.runtimeDir == null then cfg.stateDir else "/run/${cfg.runtimeDir}";
        cmd = builtins.filter (x: x != "") [
          "${cfg.package}/bin/${exec}"
          "--genesis-file ${cfg.genesisFile}"
          "--genesis-hash ${cfg.genesisHash}"
          "--config ${cfg.nodeConfigFile}"
          "--database-path ${cfg.databasePath}"
          "--socket-path ${runtimeDir}/node-$1.socket"
          "--topology ${cfg.topology}"
          "--host-addr ${cfg.hostAddr}"
          "--port ${toString cfg.port}"
          "${lib.optionalString (cfg.signingKey != null)
            "--signing-key ${cfg.signingKey}"}"
          "${lib.optionalString (cfg.delegationCertificate != null)
            "--delegation-certificate ${cfg.delegationCertificate}"}"
        ] ++ cfg.extraArgs;
    in ''
        choice() { i=$1; shift; eval "echo \''${$((i + 1))}"; }
        echo "Starting ${exec}: ${concatStringsSep "\"\n   echo \"" cmd}"
        echo "..or, once again, in a single line:"
        echo "${toString cmd}"
        ls -l ${runtimeDir} || true
        exec ${toString cmd}'';
in {
  options = {
    services.cardano-node = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable cardano-node, a node implementing ouroboros protocols
          (the blockchain protocols running cardano).
        '';
      };
      instanced = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable systemd service instancing.
          For details see https://fedoramagazine.org/systemd-template-unit-files/
        '';
      };
      script = mkOption {
        type = types.str;
        default = mkScript cfg;
      };

      package = mkOption {
        type = types.package;
        default = cardanoNodeHaskellPackages.cardano-node.components.exes.cardano-node;
        defaultText = "cardano-node";
        description = ''
          The cardano-node package that should be used
        '';
      };

      environments = mkOption {
        type = types.attrs;
        default = commonLib.environments;
        description = ''
          environment node will connect to
        '';
      };

      environment = mkOption {
        type = types.enum (builtins.attrNames cfg.environments);
        default = "testnet";
        description = ''
          environment node will connect to
        '';
      };

      genesisFile = mkOption {
        type = types.path;
        default = envConfig.genesisFile;
        description = ''
          Genesis json file
        '';
      };

      genesisHash = mkOption {
        type = types.str;
        default = envConfig.genesisHash;
        description = ''
          Hash of the genesis file
        '';
      };

      signingKey = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Signing key
        '';
      };

      delegationCertificate = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Delegation certificate
        '';
      };

      consensusProtocol = mkOption {
        default = "real-pbft";
        type = types.enum ["bft" "praos" "mock-pbft" "real-pbft"];
        description = ''
          Consensus initially used by the node:
            - bft: BFT consensus algorithm
            - praos: Praos consensus algorithm
            - mock-pbft: Permissive BFT consensus algorithm using a mock ledger
            - real-pbft: Permissive BFT consensus algorithm using the real ledger
        '';
      };

      hostAddr = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host address to bind to
        '';
      };

      stateDir = mkOption {
        type = types.str;
        default = "/var/lib/cardano-node";
        description = ''
          Directory to store blockchain data.
        '';
      };

      runtimeDir = mkOption {
        type = types.nullOr types.str;
        default = "cardano-node";
        description = ''
          Runtime directory relative to /run
        '';
      };

      databasePath = mkOption {
        type = types.str;
        default = "${cfg.stateDir}/${cfg.dbPrefix}";
        description = ''Node database path.'';
      };

      dbPrefix = mkOption {
        type = types.str;
        default = "db-${cfg.environment}";
        description = ''
          Prefix of database directories inside `stateDir`.
          (eg. for "db", there will be db-0, etc.).
        '';
      };

      port = mkOption {
        type = types.either types.int types.str;
        default = 3001;
        description = ''
          The port number
        '';
      };

      nodeId = mkOption {
        type = types.int;
        default = 0;
        description = ''
          The ID for this node
        '';
      };

      topology = mkOption {
        type = types.path;
        default = commonLib.mkEdgeTopology {
          inherit (cfg) port;
          inherit (envConfig) edgeNodes;
        };
        description = ''
          Cluster topology
        '';
      };

      nodeConfig = mkOption {
        type = types.attrs;
        default = envConfig.nodeConfig;
        description = ''Internal representation of the config.'';
      };

      nodeConfigFile = mkOption {
        type = types.str;
        default = "${toFile "config-${toString cfg.nodeId}.json" (toJSON (svcLib.mkNodeConfig cfg cfg.nodeId))}";
        description = ''Actual configuration file (shell expression).'';
      };

      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''Extra CLI args for 'cardano-node'.'';
      };

      tracingVerbosity = mkOption {
        type = types.str;
        default = envConfig.tracingVerbosity;
        description = ''verbosity level of structured representation of traced values.'';
      };

      protover-major = mkOption {
        type = types.nullOr types.int;
        default = null;
      };
      protover-minor = mkOption {
        type = types.nullOr types.int;
        default = null;
      };
      protover-alt = mkOption {
        type = types.nullOr types.int;
        default = null;
      };
    };
  };

  config = mkIf cfg.enable ( let stateDirBase = "/var/lib/"; in {
    users.groups.cardano-node.gid = 10016;
    users.users.cardano-node = {
      description = "cardano-node node daemon user";
      uid = 10016;
      group = "cardano-node";
    };

    ## TODO:  use http://hackage.haskell.org/package/systemd for:
    ##   1. only declaring success after we perform meaningful init (local state recovery)
    ##   2. heartbeat & watchdog functionality
    systemd.services."${systemdServiceName}" = {
      description   = "cardano-node node service";
      after         = [ "network.target" ];
      script = cfg.script;
      serviceConfig = {
        User = "cardano-node";
        Group = "cardano-node";
        Restart = "always";
        RuntimeDirectory = cfg.runtimeDir;
        WorkingDirectory = cfg.stateDir;
        # This assumes /var/lib/ is a prefix of cfg.stateDir.
        # This is checked as an assertion below.
        StateDirectory =  lib.removePrefix stateDirBase cfg.stateDir;
      };
    } // optionalAttrs (! cfg.instanced) {
      wantedBy = [ "multi-user.target" ];
    };
    assertions = [{
      assertion = lib.hasPrefix stateDirBase cfg.stateDir;
      message =
        "The option services.cardano-node.stateDir should have ${stateDirBase} as a prefix!";
    }];
  });
}
