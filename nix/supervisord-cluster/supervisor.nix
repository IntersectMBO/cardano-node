{ pkgs
, lib
, stateDir
, baseEnvConfig
, basePort
, profile
, useCabalRun
  ## Last-moment overrides:
, extraSupervisorConfig
}:

with profile;

let
  mkStartScript = envConfig: let
    systemdCompat.options = {
      systemd.services = lib.mkOption {};
      systemd.sockets = lib.mkOption {};
      users = lib.mkOption {};
      assertions = lib.mkOption {};
    };
    eval = let
      extra = {
        services.cardano-node = {
          enable = true;
          inherit (envConfig) topology nodeConfig nodeConfigFile port dbPrefix socketPath;
          inherit stateDir;
        }
        // lib.optionalAttrs (__hasAttr "vrfKey" envConfig)
        { inherit (envConfig) operationalCertificate kesKey vrfKey;
        }
        // lib.optionalAttrs useCabalRun
        { executable = "cabal run exe:cardano-node --";
        };
      };
    in lib.evalModules {
      prefix = [];
      modules = import ../nixos/module-list.nix ++ [ systemdCompat extra ];
      args = { inherit pkgs; };
    };
  in pkgs.writeScript "cardano-node" ''
    #!${pkgs.stdenv.shell}
    ${eval.config.services.cardano-node.script}
  '';

  config =
  (pkgs.commonLib.supervisord.writeSupervisorConfig ({
    supervisord = {
      logfile = "${stateDir}/supervisord.log";
      pidfile = "${stateDir}/supervisord.pid";
      strip_ansi = true;
    };
    supervisorctl = {};
    inet_http_server = {
      port = "127.0.0.1:9001";
    };
    "rpcinterface:supervisor" = {
      "supervisor.rpcinterface_factory" = "supervisor.rpcinterface:make_main_rpcinterface";
    };
  } // lib.listToAttrs (map (i:
    lib.nameValuePair "program:bft${toString i}" {
      command = let
        index = i - 1;
        envConfig = baseEnvConfig // rec {
          operationalCertificate = "${stateDir}/shelley/nodes/node-bft${toString i}/op.cert";
          kesKey = "${stateDir}/shelley/nodes/node-bft${toString i}/kes.skey";
          vrfKey = "${stateDir}/shelley/nodes/node-bft${toString i}/vrf.skey";
          topology = "${stateDir}/topologies/node-${toString index}.json";
          socketPath = "${stateDir}/bft${toString i}.socket";
          dbPrefix = "db-bft${toString i}";
          port = basePort + i;
          nodeConfigFile = "${stateDir}/config.json";
        };
        script = mkStartScript envConfig;
      in "${script}";
      stdout_logfile = "${stateDir}/bft${toString i}.stdout";
      stderr_logfile = "${stateDir}/bft${toString i}.stderr";
    }
  ) (lib.genList (i: i + 1) composition.n_bft_hosts))
  // lib.listToAttrs (map (i:
    lib.nameValuePair "program:pool${toString i}" {
      command = let
        index = composition.n_bft_hosts + i - 1;
        envConfig = baseEnvConfig // rec {
          operationalCertificate = "${stateDir}/shelley/nodes/node-pool${toString i}/op.cert";
          kesKey = "${stateDir}/shelley/nodes/node-pool${toString i}/kes.skey";
          vrfKey = "${stateDir}/shelley/nodes/node-pool${toString i}/vrf.skey";
          topology = "${stateDir}/topologies/node-${toString index}.json";
          socketPath = "${stateDir}/pool${toString i}.socket";
          dbPrefix = "db-pool${toString i}";
          port = basePort + composition.n_bft_hosts + i;
          nodeConfigFile = "${stateDir}/config.json";
        };
        script = mkStartScript envConfig;
      in "${script}";
      stdout_logfile = "${stateDir}/pool${toString i}.stdout";
      stderr_logfile = "${stateDir}/pool${toString i}.stderr";
    }
  ) (lib.genList (i: i + 1) composition.n_pools))
  // lib.listToAttrs ([(
    lib.nameValuePair "program:observer" {
      command = let
        envConfig = baseEnvConfig // rec {
          topology = "${stateDir}/topologies/observer.json";
          socketPath = "${stateDir}/observer.socket";
          dbPrefix = "db-observer";
          port = basePort + composition.n_hosts + 1;
          nodeConfigFile = "${stateDir}/config.json";
        };
        script = mkStartScript envConfig;
      in "${script}";
      stdout_logfile = "${stateDir}/observer.stdout";
      stderr_logfile = "${stateDir}/observer.stderr";
    }
  )])
  // {
    "program:webserver" = {
      command = "${pkgs.python3}/bin/python -m http.server ${toString basePort}";
      directory = "${stateDir}/shelley/webserver";
    };

  } // extraSupervisorConfig));

in
  pkgs.writeText "supervisor.conf" config
