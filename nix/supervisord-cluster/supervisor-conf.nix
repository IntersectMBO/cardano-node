{ pkgs
, lib
, stateDir
, basePort
, nodeSetups               ## :: Map NodeName NodeSetup
  ## Last-moment overrides:
, extraSupervisorConfig
}:

with lib;

let
  ##
  ## nodeSetupSupervisorProgram :: NodeSpec -> SupervisorConfSection
  ##
  ## Refer to: http://supervisord.org/configuration.html#program-x-section-settings
  ##
  nodeSetupSupervisorProgram = { nodeSpec, nodeService, startupScript, ... }:
    nameValuePair "program:${nodeSpec.name}" {
      directory      = "${nodeService.stateDir}";
      command        = "${startupScript}";
      stdout_logfile = "${nodeService.stateDir}/stdout";
      stderr_logfile = "${nodeService.stateDir}/stderr";
    };

  ##
  ## supervisorConf :: SupervisorConf
  ##
  ## Refer to: http://supervisord.org/configuration.html
  ##
  supervisorConf =
    {
      supervisord = {
        logfile = "${stateDir}/supervisor/supervisord.log";
        pidfile = "${stateDir}/supervisor/supervisord.pid";
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
      (mapAttrsToList (_: nodeSetupSupervisorProgram) nodeSetups)
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
