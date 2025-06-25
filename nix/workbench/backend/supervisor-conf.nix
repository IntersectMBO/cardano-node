{ pkgs
, lib
, stateDir
, profile
, nodeSpecs
, withGenerator
, withTracer
, withSsh
, unixHttpServerPort ? null
, inetHttpServerPort ? null
}:

with lib;

let
  # The prefix for every "[program:X] command=sh start.sh" because `supervisord`
  # may run in the most unexpected places where we can't asume what is or isn't
  # included in $PATH. Just make sure pkgs.bashInteractive is in the nix store.
  sh = "${pkgs.bashInteractive}/bin/sh";
  # We can't obtain the exit codes using `supervisorctl status`, it returns zero
  # when RUNNING and non-zero for STOPPED, EXITED, FATAL, and UNKNOWN, so as we
  # are not using any "autorestart" like functionality supervisord programs are
  # echoing their exit code after the script to a file named `exit_code` that is
  # created and/or emptied before every script run.
  # Warning: This command assumes the "directory" is set correctly.
  command = "${sh} -c ':> ./exit_code; ./start.sh; exit_code=\"''$?\"; echo \"''$exit_code\" > ./exit_code; exit \"''$exit_code\"'";
  ##
  ## supervisorConf :: SupervisorConf
  ##
  ## Refer to:
  ## - http://supervisord.org/configuration.html
  supervisorConf =
    ##
    ## [unix_http_server] Section Settings
    ##
    ## Refer to:
    ## - http://supervisord.org/configuration.html#unix-http-server-section-settings
    lib.attrsets.optionalAttrs (unixHttpServerPort != null) {
      unix_http_server = {
        file = unixHttpServerPort;
        chmod = "0777";
      };
    }
    ##
    ## [inet_http_server] Section Settings
    ##
    ## Refer to:
    ## - http://supervisord.org/configuration.html#inet-http-server-section-settings
    //
    lib.attrsets.optionalAttrs (inetHttpServerPort != null) {
      inet_http_server = {
        port = inetHttpServerPort;
      };
    }
    //
    {
      ##
      ## [supervisord] Section Settings
      ##
      ## Refer to:
      ## http://supervisord.org/configuration.html#supervisord-section-settings
      supervisord = {
        logfile = "${stateDir}/supervisor/supervisord.log";
        pidfile = "${stateDir}/supervisor/supervisord.pid";
        strip_ansi = true;
      };
      ##
      ## [supervisorctl] Section Settings
      ##
      ## Refer to:
      ## http://supervisord.org/configuration.html#supervisorctl-section-settings
      supervisorctl = {};
      "rpcinterface:supervisor" = {
        "supervisor.rpcinterface_factory" = "supervisor.rpcinterface:make_main_rpcinterface";
      };
    }
    ##
    ## [program:x] Section Settings
    ##
    ## Refer to:
    ## - http://supervisord.org/configuration.html#program-x-section-settings
    //
    lib.attrsets.optionalAttrs withTracer
    {
      "program:tracer" = {
        # "command" below assumes "directory" is set accordingly.
        directory      = "${stateDir}/tracer";
        command        = "${command}";
        stdout_logfile = "${stateDir}/tracer/stdout";
        stderr_logfile = "${stateDir}/tracer/stderr";
        # Set these values to 0 to indicate an unlimited log size / no rotation.
        stdout_logfile_maxbytes = 0;
        stderr_logfile_maxbytes = 0;
        stopasgroup    = true;
        killasgroup    = true;
        autostart      = false;
        autorestart    = false;
        # Don't attempt any restart!
        startretries   = 0;
        # Seconds it needs to stay running to consider the start successful
        startsecs      = 5;
      };
    }
    //
    (builtins.listToAttrs (lib.mapAttrsToList (nodeName: nodeSpec:
      lib.attrsets.nameValuePair "program:${nodeName}" {
        # "command" below assumes "directory" is set accordingly.
        directory      = "${stateDir}/${nodeName}";
        command        = "${command}";
        stdout_logfile = "${stateDir}/${nodeName}/stdout";
        stderr_logfile = "${stateDir}/${nodeName}/stderr";
        # Set these values to 0 to indicate an unlimited log size / no rotation.
        stdout_logfile_maxbytes = 0;
        stderr_logfile_maxbytes = 0;
        # Send stop and kill signals to the whole process group.
        stopasgroup    = true;
        killasgroup    = true;
        autostart      = false;
        autorestart    = false;
        # Don't attempt any restart!
        startretries   = 0;
        # Seconds it needs to stay running to consider the start successful
        # In cases with a big genesis file, like the "value" profile with ~600
        # mega, if this file has a format error the node can fail after the 5
        # seconds we use as default for the other "program"s and the error will
        # be caught later by the healthcheck service with a misleading message.
        # We found with our AWS reference machines (c5.2xlarge, 16 MB and 8
        # cores), when running the "value" profile, that with 50 seconds at
        # least one node was assumed successful (its socket was created). So to
        # the default 5 we add 50 seconds when the UTxO set size is the one of
        # the "value" profile and seconds proportionally to this for the others.
        # Not directly related to "genesis.extra_future_offset" or
        # "derived.genesis_future_offset".
        ### derived.dataset_measure
        ### - fast:                          0 (Default of 5s is OK)
        ### - ci-test:                       0 (Default of 5s is OK)
        ### - default:                       0 (Default of 5s is OK)
        ### - plutus:                        0 (Default of 5s is OK)
        ### - forge-stress-pre:        5000000
        ### - forge-stress-large:     11300000
        ### - value:                   5000000 (50s more needed)
        startsecs      = 5 + (profile.derived.dataset_measure / (5000000 / 50));
      })
    nodeSpecs))
    //
    lib.attrsets.optionalAttrs withGenerator
    {
      "program:generator" = {
        # "command" below assumes "directory" is set accordingly.
        directory      = "${stateDir}/generator";
        command        = "${command}";
        stdout_logfile = "${stateDir}/generator/stdout";
        stderr_logfile = "${stateDir}/generator/stderr";
        # Set these values to 0 to indicate an unlimited log size / no rotation.
        stdout_logfile_maxbytes = 0;
        stderr_logfile_maxbytes = 0;
        # Send stop and kill signals to the whole process group.
        stopasgroup    = true;
        killasgroup    = true;
        autostart      = false;
        autorestart    = false;
        # Don't attempt any restart!
        startretries   = 0;
        # Seconds it needs to stay running to consider the start successful
        startsecs      = 5;
      };
    }
    //
    {
      "program:healthcheck" = {
        # "command" below assumes "directory" is set accordingly.
        directory      = "${stateDir}/healthcheck";
        command        = "${command}";
        stdout_logfile = "${stateDir}/healthcheck/stdout";
        stderr_logfile = "${stateDir}/healthcheck/stderr";
        # Set these values to 0 to indicate an unlimited log size / no rotation.
        stdout_logfile_maxbytes = 0;
        stderr_logfile_maxbytes = 0;
        # Send stop and kill signals to the whole process group.
        stopasgroup    = true;
        killasgroup    = true;
        autostart      = false;
        autorestart    = false;
        # Don't attempt any restart!
        startretries   = 0;
        # Seconds it needs to stay running to consider the start successful
        startsecs      = 5;
      };
    }



    //
    (builtins.listToAttrs (builtins.map (workload: {
      name  = "program:${workload.name}";
      value = {
        # "command" below assumes "directory" is set accordingly.
        directory      = "${stateDir}/workloads/${workload.name}";
        command        = "${command}";
        stdout_logfile = "${stateDir}/workloads/${workload.name}/stdout";
        stderr_logfile = "${stateDir}/workloads/${workload.name}/stderr";
        # Set these values to 0 to indicate an unlimited log size / no rotation.
        stdout_logfile_maxbytes = 0;
        stderr_logfile_maxbytes = 0;
        # Send stop and kill signals to the whole process group.
        stopasgroup    = true;
        killasgroup    = true;
        autostart      = false;
        autorestart    = false;
        # Don't attempt any restart!
        startretries   = 0;
        # Seconds it needs to stay running to consider the start successful
        startsecs      = 5;
      };
    }) profile.workloads))



    //
    lib.attrsets.optionalAttrs withSsh
    {
      "program:ssh" = {
        # "command" below assumes "directory" is set accordingly.
        directory      = "${stateDir}/ssh";
        command        = "${command}";
        stdout_logfile = "${stateDir}/ssh/stdout";
        stderr_logfile = "${stateDir}/ssh/stderr";
        # Set these values to 0 to indicate an unlimited log size / no rotation.
        stdout_logfile_maxbytes = 0;
        stderr_logfile_maxbytes = 0;
        # Send stop and kill signals to the whole process group.
        stopasgroup    = true;
        killasgroup    = true;
        autostart      = false;
        autorestart    = false;
        # Don't attempt any restart!
        startretries   = 0;
        # Seconds it needs to stay running to consider the start successful
        startsecs      = 5;
      };
    }
    ;

in
  generators.toINI {} supervisorConf
