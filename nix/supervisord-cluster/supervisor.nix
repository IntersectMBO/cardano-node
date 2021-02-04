{ pkgs
, lib
, stateDir
, baseEnvConfig
, basePort
, profile
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
          ## topology wtf?
          inherit (envConfig) operationalCertificate kesKey vrfKey topology nodeConfig nodeConfigFile port dbPrefix socketPath;
          inherit stateDir;
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

  topologyFile = selfPort: {
    Producers = map (p:
      {
        addr = "127.0.0.1";
        port = p;
        valency = 1;
      }) (lib.filter (p: p != selfPort) (lib.genList (i: basePort + i + 1) (composition.n_bft_hosts + composition.n_pools)));
  };

  config =
  (pkgs.commonLib.supervisord.writeSupervisorConfig ({
    supervisord = {
      logfile = "${stateDir}/supervisord.log";
      pidfile = "${stateDir}/supervisord.pid";
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
        envConfig = baseEnvConfig // rec {
          operationalCertificate = "${stateDir}/nodes/node-bft${toString i}/op.cert";
          kesKey = "${stateDir}/nodes/node-bft${toString i}/kes.skey";
          vrfKey = "${stateDir}/nodes/node-bft${toString i}/vrf.skey";
          topology = __toFile "topology.yaml" (__toJSON (topologyFile port));
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
        envConfig = baseEnvConfig // rec {
          operationalCertificate = "${stateDir}/nodes/node-pool${toString i}/op.cert";
          kesKey = "${stateDir}/nodes/node-pool${toString i}/kes.skey";
          vrfKey = "${stateDir}/nodes/node-pool${toString i}/vrf.skey";
          topology = __toFile "topology.yaml" (__toJSON (topologyFile port));
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
  // {
    "program:webserver" = {
      command = "${pkgs.python3}/bin/python -m http.server ${toString basePort}";
      directory = "${stateDir}/webserver";
    };

  } // extraSupervisorConfig));

in
  pkgs.writeText "supervisor.conf" config
