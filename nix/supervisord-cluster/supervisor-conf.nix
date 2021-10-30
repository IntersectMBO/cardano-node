{ pkgs
, lib
, stateDir
, basePort
, node-services
, generator-service
  ## Last-moment overrides:
, extraSupervisorConfig
}:

with lib;

let
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
      (mapAttrsToList (_: nodeSvcSupervisorProgram) node-services)
    //
    {
      "program:generator" = {
        directory      = "${stateDir}/generator";
        command        = "${stateDir}/generator/start.sh";
        stdout_logfile = "${stateDir}/generator/stdout";
        stderr_logfile = "${stateDir}/generator/stderr";
        autostart      = false;
      };
    }
    //
    extraSupervisorConfig;

  ##
  ## nodeSvcSupervisorProgram :: NodeService -> SupervisorConfSection
  ##
  ## Refer to: http://supervisord.org/configuration.html#program-x-section-settings
  ##
  nodeSvcSupervisorProgram = { nodeSpec, service, ... }:
    nameValuePair "program:${nodeSpec.value.name}" {
      directory      = "${service.value.stateDir}";
      command        = "sh start.sh";
      stdout_logfile = "${service.value.stateDir}/stdout";
      stderr_logfile = "${service.value.stateDir}/stderr";
    };

in
  pkgs.writeText "supervisor.conf"
    (generators.toINI {} supervisorConf)
