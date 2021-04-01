{ pkgs
, lib
, stateDir
, basePort
, nodeSpecs                ## :: Map NodeName NodeSpec
, nodeSetups               ## :: Map NodeName NodeSetup
  ## Last-moment overrides:
, extraSupervisorConfig
}:

with lib;

let
  ##
  ## nodeSpecSupervisorProgram :: NodeSpec -> SupervisorConfSection
  ##
  nodeSpecSupervisorProgram = { name, i, kind, port, isProducer }:
    nameValuePair "program:${kind}-${toString i}" {
      command        = "${nodeSetups."${name}".startupScript}";
      stdout_logfile = "${stateDir}/${name}/stdout";
      stderr_logfile = "${stateDir}/${name}/stderr";
    };

  ##
  ## supervisorConf :: SupervisorConf
  ##
  supervisorConf =
    {
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
    }
    //
    listToAttrs
      (mapAttrsToList (_: nodeSpecSupervisorProgram) nodeSpecs)
    //
    {
      "program:webserver" = {
        command = "${pkgs.python3}/bin/python -m http.server ${toString (basePort - 1)}";
        directory = "${stateDir}/shelley/webserver";
      };
    }
    //
    extraSupervisorConfig;

in
  pkgs.writeText "supervisor.conf"
    (pkgs.commonLib.supervisord.writeSupervisorConfig
       supervisorConf)
