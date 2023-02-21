{ pkgs
, lib
, stateDir
, profileNix
, unixHttpServerPort ? null
, inetHttpServerPort ? null
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
      "rpcinterface:supervisor" = {
        "supervisor.rpcinterface_factory" = "supervisor.rpcinterface:make_main_rpcinterface";
      };
    }
    //
    {
      "program:generator" = {
        directory      = "${stateDir}/generator";
        command        = "sh start.sh";
        stdout_logfile = "${stateDir}/generator/stdout";
        stderr_logfile = "${stateDir}/generator/stderr";
        stopasgroup    = false;
        killasgroup    = false;
        autostart      = false;
        autorestart    = false;
        startretries   = 1;
        startsecs      = 5;
      };
    }
    //
    lib.attrsets.optionalAttrs (profileNix.value.node.tracer)
    {
      "program:tracer" = {
        directory      = "${stateDir}/tracer";
        command        = "sh start.sh";
        stdout_logfile = "${stateDir}/tracer/stdout";
        stderr_logfile = "${stateDir}/tracer/stderr";
        stopasgroup    = true;
        killasgroup    = true;
        autostart      = false;
        autorestart    = false;
        startretries   = 1;
        startsecs      = 1;
      };
    }
    //
    listToAttrs
      (flip mapAttrsToList profileNix.node-services
        (_: { nodeSpec, service, ... }:
          nameValuePair "program:${nodeSpec.value.name}" {
            ##
            ## Refer to: http://supervisord.org/configuration.html#program-x-section-settings
            ##
            directory      = "${service.value.stateDir 0}";
            command        = "sh start.sh";
            stdout_logfile = "${service.value.stateDir 0}/stdout";
            stderr_logfile = "${service.value.stateDir 0}/stderr";
            stopasgroup    = false;
            killasgroup    = false;
            autostart      = false;
            autorestart    = false;
            startretries   = 1;
            startsecs      = 1;
          })
        )
    //
    lib.attrsets.optionalAttrs (unixHttpServerPort != null) {
      unix_http_server = {
        file = unixHttpServerPort;
        chmod = "0777";
      };
    }
    //
    lib.attrsets.optionalAttrs (inetHttpServerPort != null) {
      inet_http_server = {
        port = inetHttpServerPort;
      };
    };

in {
  value = supervisorConf;
  INI = pkgs.writeText "supervisor.conf" (generators.toINI {} supervisorConf);
}
