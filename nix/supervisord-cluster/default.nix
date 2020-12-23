{ pkgs
, lib
, cardano-cli
, bech32
, basePort ? 30000
, stateDir ? "./state-cluster"
, extraSupervisorConfig ? {}
##
, profileName ? "default"
, profileOverride ? {}
, ...
}:
let
  profiles = pkgs.callPackage ./profiles.nix
    { inherit
      lib;
    };
  profile = lib.recursiveUpdate profiles."${profileName}" profileOverride;
  inherit (profile) composition monetary;

  ## This yields two attributes: 'params' and 'files'
  genesis = pkgs.callPackage ./genesis.nix
    { inherit
      lib
      path
      stateDir
      baseEnvConfig
      basePort
      profile
      ;
    };

  baseEnvConfig = pkgs.callPackage ./base-env.nix { inherit (pkgs.commonLib.cardanoLib) defaultLogConfig; inherit stateDir; };
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
      }) (lib.filter (p: p != selfPort) (lib.genList (i: basePort + i + 1) (composition.numBft + composition.numPools)));
  };
  supervisorConfig = pkgs.writeText "supervisor.conf" (pkgs.commonLib.supervisord.writeSupervisorConfig ({
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
  ) (lib.genList (i: i + 1) composition.numBft))
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
          port = basePort + composition.numBft + i;
          nodeConfigFile = "${stateDir}/config.json";
        };
        script = mkStartScript envConfig;
      in "${script}";
      stdout_logfile = "${stateDir}/pool${toString i}.stdout";
      stderr_logfile = "${stateDir}/pool${toString i}.stderr";
    }
  ) (lib.genList (i: i + 1) composition.numPools))
  // {
    "program:webserver" = {
      command = "${pkgs.python3}/bin/python -m http.server ${toString basePort}";
      directory = "${stateDir}/webserver";
    };

  } // extraSupervisorConfig));
  path = lib.makeBinPath [ cardano-cli bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils ];

  start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail
    if [ -f ${stateDir}/supervisord.pid ]
    then
      echo "Cluster already running. Please run `stop-cluster` first!"
    fi
    ${genesis.files}
    ${pkgs.python3Packages.supervisor}/bin/supervisord --config ${supervisorConfig} $@
    while [ ! -S $CARDANO_NODE_SOCKET_PATH ]; do echo "Waiting 5 seconds for bft node to start"; sleep 5; done
    echo "Transfering genesis funds to pool owners, register pools and delegations"
    cardano-cli transaction submit --shelley-mode \
      --tx-file ${stateDir}/shelley/transfer-register-delegate-tx.tx \
      --testnet-magic ${toString genesis.params.networkMagic}
    sleep 5
    echo 'Cluster started. Run `stop-cluster` to stop'
  '';
  stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail
    ${pkgs.python3Packages.supervisor}/bin/supervisorctl stop all
    if [ -f ${stateDir}/supervisord.pid ]
    then
      kill $(<${stateDir}/supervisord.pid)
      echo "Cluster terminated!"
    else
      echo "Cluster is not running!"
    fi
  '';

in { inherit baseEnvConfig start stop profile genesis; }
