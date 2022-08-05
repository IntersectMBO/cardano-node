{ pkgs
, lib
, stateDir
, basePort
, node-services
  ## Last-moment overrides:
, extraBackendConfig
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
        command        = "sh start.sh";
        stdout_logfile = "${stateDir}/generator/stdout";
        stderr_logfile = "${stateDir}/generator/stderr";
        autostart      = false;
        startretries   = 0;
      };
    }
    //
    {
      "program:tracer" = {
        directory      = "${stateDir}/tracer";
        command        = "sh start.sh";
        stdout_logfile = "${stateDir}/tracer/stdout";
        stderr_logfile = "${stateDir}/tracer/stderr";
        autostart      = false;
        startretries   = 0;
      };
    }
    //
    extraBackendConfig;

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
      startretries   = 0;
      autostart      = false;
      autorestart    = false;
    };

in
  pkgs.writeText "supervisor.conf"
    (generators.toINI {} supervisorConf)
