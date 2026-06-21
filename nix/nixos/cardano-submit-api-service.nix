# This service exposes an http port, and connects to a cardano-node over a UNIX socket
{ config, lib, pkgs, ... }:
let
  inherit (builtins) fromJSON readFile;
  inherit (cfg.cardanoNodePackages) cardanoLib;

  cfg = config.services.cardano-submit-api;
in {
  options = {
    services.cardano-submit-api = {
      enable = lib.mkEnableOption "Enable the cardano-submit-api api";

      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
        description = "Generated cardano-submit-api launch script (internal).";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = cfg.cardanoNodePackages.cardano-submit-api;
        description = "The cardano-submit-api package to run.";
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 8090;
        description = "HTTP port submit-api listens on.";
      };

      listenAddress = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = "Host address submit-api binds to.";
      };

      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          The cardano node socket path. If set, the entrypoint takes this value
          over CARDANO_NODE_SOCKET_PATH env variable.
        '';
      };

      group = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Optional supplementary group added to the service's dynamic user,
          typically the cardano-node socket group, so cardano-submit-api can
          access the node socket at CARDANO_NODE_SOCKET_PATH.
        '';
      };

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = cardanoLib.defaultSubmitApiConfig;
        description = "Tracing configuration passed to submit-api.";
      };

      network = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "Network name.";
        default = null;
      };

      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = cfg.cardanoNodePackages.cardanoLib.environments.${cfg.network};
        description = "Cardano environment attrset for the selected network.";
      };

      cardanoNodePackages = lib.mkOption {
        type = lib.types.attrs;
        default = pkgs.cardanoNodePackages or (import ../. {}).cardanoNodePackages;
        defaultText = "cardano-node packages";
        description = ''
          The cardano-node packages and library that should be used.
          Main usage is sharing optimization to reduce eval time when services
          are instantiated multiple times.
        '';
      };
    };
  };
  config = let
    envNodeCfg = cfg.environment.nodeConfig;
    shelleyGenesisParams = fromJSON (readFile envNodeCfg.ShelleyGenesisFile);
    envFlag = if cfg.network == "mainnet" then "--mainnet" else "--testnet-magic ${toString shelleyGenesisParams.networkMagic}";
  in lib.mkIf cfg.enable {
    services.cardano-submit-api.script = pkgs.writeShellScript "cardano-submit-api" ''
      ${if (cfg.socketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
      then
        echo "You must set \$CARDANO_NODE_SOCKET_PATH"
        exit 1
      fi'' else "export \"CARDANO_NODE_SOCKET_PATH=${cfg.socketPath}\""}
      exec ${cfg.package}/bin/cardano-submit-api --socket-path "$CARDANO_NODE_SOCKET_PATH" ${envFlag} \
            --port ${toString cfg.port} \
            --listen-address ${cfg.listenAddress} \
            --config ${builtins.toFile "submit-api.json" (builtins.toJSON cfg.config)}
    '';
    systemd.services.cardano-submit-api = {
      serviceConfig = {
        ExecStart = config.services.cardano-submit-api.script;
        DynamicUser = true;

        # The api connects to the node over a UNIX socket that only becomes
        # available once the node has started; `after` orders startup but does
        # not wait for the socket. Default to restarting until it is reachable
        # rather than failing permanently on a fresh boot. These are mkDefault
        # so a consumer can impose a bounded restart + start-limit policy that
        # lets persistent failures surface as a failed unit for alerting.
        Restart = lib.mkDefault "always";
        RestartSec = lib.mkDefault 1;
      } // lib.optionalAttrs (cfg.group != null) {
        # A DynamicUser is not otherwise a member of the node socket group, so
        # without this it cannot open CARDANO_NODE_SOCKET_PATH.
        SupplementaryGroups = [ cfg.group ];
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "cardano-node.service" ];
    };
  };
}
