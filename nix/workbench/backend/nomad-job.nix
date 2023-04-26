################################################################################
# A Nomad job description intended to be reused between local/development
# clusters and SRE infrastructure used for long-running cloud benchmarks. Why?
# To make it easier to improve and debug the almighty workbench!
################################################################################
{ lib
, stateDir
, profileData
, containerSpecs
# Needs unix_http_server.file
, supervisorConf
, execTaskDriver
, oneTracerPerNode ? false
}:

let

  # Task (Container or chroot) defaults:
  ## This values are the defaults that are stored on the job's "meta" stanza to
  ## be able to overrided them with `jq` inside the workbench shell.
  ## Values go: Nix (defaults) -> meta -> template -> envars
  #
  ## See ./oci-images.nix for further details if using the `podman` driver.
  ## For the `exec` driver almost everything is here.
  #
  # Templates are rendered into the task working directory. Drivers without
  # filesystem isolation (such as raw_exec) or drivers that build a chroot in
  # the task working directory (such as exec) can have templates rendered to
  # arbitrary paths in the task. But task drivers such as docker can only access
  # templates rendered into the NOMAD_ALLOC_DIR, NOMAD_TASK_DIR, or
  # NOMAD_SECRETS_DIR. To work around this restriction, you can create a mount
  # from the template destination to another location in the task.
  ## - https://developer.hashicorp.com/nomad/docs/job-specification/template#template-destinations
  ## - https://developer.hashicorp.com/nomad/docs/runtime/environment#task-directories
  ## - https://developer.hashicorp.com/nomad/docs/concepts/filesystem
  task_workdir = if execTaskDriver
    # A `work_dir` stanza is comming (?):
    # https://github.com/hashicorp/nomad/pull/10984
    # TODO: Try with ''${NOMAD_TASK_DIR}'' in both!
    then "/local"
    # This value must also be used inside the `podman` `config` stanza.
    else "/local"
    ;
  # Usually "*/local/run/current"
  task_statedir = "${task_workdir}${if stateDir == "" then "" else ("/" + stateDir)}";
  # A symlink to the supervisord nix-installed inside the OCI image/chroot.
  # We need to be able to `nomad exec supervisorctl ...` , for these the path
  # of the installed supervisor binaries is needed.
  task_supervisor_nix = "${task_statedir}/supervisor/nix-store";
  # The URL to the listening inet or socket of the supervisord server:
  # The problem is that if we use "127.0.0.1:9001" as parameter (without the
  # "http" part) the container returns:
  # error: <class 'ValueError'>, Unknown protocol for serverurl 127.0.0.1:9001: file: /nix/store/izqhlj5i1x9ldyn43d02kcy4mafmj3ci-python3.9-supervisor-4.2.4/lib/python3.9/site-packages/supervisor/xmlrpc.py line: 508
  # Without using the `--serverurl` parameter at all (using INI config file's
  # [inet_http_server] port stanza) also without "http://":
  # error: <class 'socket.gaierror'>, [Errno -2] Name or service not known: file: /nix/store/hb1lzaisgx2m9n29hqhh6yp6hasplq1v-python3-3.9.10/lib/python3.9/socket.py line: 954
  # If I add the "http" part to the INI file, when starting `supervisord` inside
  # the container I get (from journald):
  # Nov 02 11:44:36 hostname cluster-18f3852f-e067-6394-8159-66a7b8da2ecc[1088457]: Error: Cannot open an HTTP server: socket.error reported -2
  # Nov 02 11:44:36 hostname cluster-18f3852f-e067-6394-8159-66a7b8da2ecc[1088457]: For help, use /nix/store/izqhlj5i1x9ldyn43d02kcy4mafmj3ci-python3.9-supervisor-4.2.4/bin/supervisord -h
  task_supervisord_url = "unix://${supervisorConf.value.unix_http_server.file}";
  # Location of the supervisord config file inside the container.
  # This file can be mounted as a volume or created as a template.
  task_supervisord_conf = "${task_statedir}/supervisor/supervisord.conf";
  task_supervisord_loglevel = "info";

  entrypoint =
    let
      coreutils  = containerSpecs.containerPkgs.coreutils.nix-store-path;
      gnutar     = containerSpecs.containerPkgs.gnutar.nix-store-path;
      zstd       = containerSpecs.containerPkgs.zstd.nix-store-path;
      supervisor = containerSpecs.containerPkgs.supervisor.nix-store-path;
    in escapeTemplate
      ''
      # Store the entrypoint env vars for debugging purposes
      ${coreutils}/bin/env > /local/entrypoint.env

      # Only needed for "exec" ?
      if test "''${TASK_DRIVER}" = "exec"
      then
        cd "''${TASK_WORKDIR}"
      fi

      # The SUPERVISOR_NIX variable must be set
      [ -z "''${SUPERVISOR_NIX:-}" ] && echo "SUPERVISOR_NIX env var must be set -- aborting" && exit 1

      # The SUPERVISORD_CONFIG variable must be set
      [ -z "''${SUPERVISORD_CONFIG:-}" ] && echo "SUPERVISORD_CONFIG env var must be set -- aborting" && exit 1

      # Create a link to the `supervisor` Nix folder.
      # First check if already exists to be able to restart containers.
      if ! test -e "$SUPERVISOR_NIX"
      then
        ${coreutils}/bin/ln -s "${supervisor}" "$SUPERVISOR_NIX"
      fi

      # The SUPERVISORD_LOGLEVEL variable defaults to "info" if not present
      # The logging level at which supervisor should write to the
      # activity log. Valid levels are trace, debug, info, warn, error
      # and critical.
      LOGLEVEL="''${SUPERVISORD_LOGLEVEL:-info}"

      # Start `supervisord` on the foreground.
      ${supervisor}/bin/supervisord --nodaemon --configuration "$SUPERVISORD_CONFIG" --loglevel="$LOGLEVEL"
      ''
  ;

  # About the JSON Job Specification and its odd assumptions:
  #
  # At least in Nomad version v1.4.3, the CLI command to submit new jobs
  # (https://developer.hashicorp.com/nomad/docs/commands/job/run) says:
  # "Job files must conform to the job specification format." With this link:
  # https://developer.hashicorp.com/nomad/docs/job-specification. This is the
  # HCL format that is heavily specified in the docs. Nice!
  #
  # But note that it starts saying "Nomad HCL is parsed in the command line and
  # sent to Nomad in JSON format via the HTTP API." and here there are the API
  # docs that have "JSON Job Specification" in its title:
  # https://developer.hashicorp.com/nomad/api-docs/json-jobs
  # well, this is the format that `nomad job run` expects if you use the `-json`
  # argument.
  #
  # I finally found this in the HCL overview page:
  # https://developer.hashicorp.com/nomad/docs/job-specification/hcl2
  # "Since HCL is a superset of JSON, `nomad job run example.json` will attempt
  # to parse a JSON job using the HCL parser. However, the JSON format accepted
  # by the HCL parser is not the same as the API's JSON format. The HCL parser's
  # JSON format is unspecified, so the API format is preferred. You can use the
  # API format with the -json command line flag."
  #
  # So, if you don't provide the `-json` argument it expects HCL or its JSON
  # representation: https://github.com/hashicorp/hcl/blob/main/json/spec.md
  #
  # We are using what HashiCorp calls an unespecified format but it the same
  # format the SRE team is using.

  # The job stanza is the top-most configuration option in the job
  # specification. A job is a declarative specification of tasks that Nomad
  # should run. Jobs have one or more task groups, which are themselves
  # collections of one or more tasks. Job names are unique per region or
  # namespace.
  # https://developer.hashicorp.com/nomad/docs/job-specification/job
  clusterJob = { job."workbench-cluster-job" = jobDefaults // {

    # Specifies the Nomad scheduler to use. Nomad provides the service, system,
    # batch, and sysbatch (new in Nomad 1.2) schedulers.
    # https://developer.hashicorp.com/nomad/docs/schedulers
    type = "service";

    # The namespace in which to execute the job. Prior to Nomad 1.0 namespaces
    # were Enterprise-only.
    namespace = "default";

    # The region in which to execute the job.
    region = "global"; # SRE: They are actually using global.

    #  A list of datacenters in the region which are eligible for task
    # placement. This must be provided, and does not have a default.
    # SRE: 3 Nomad datacenters exist actually
    datacenters = [ "eu-central-1" "eu-west-1" "us-east-2" ];

    # The reschedule stanza specifies the group's rescheduling strategy. If
    # specified at the job level, the configuration will apply to all groups
    # within the job. If the reschedule stanza is present on both the job and
    # the group, they are merged with the group stanza taking the highest
    # precedence and then the job.
    # To disable rescheduling, set the attempts parameter to zero and unlimited
    # to false.
    reschedule = {
      # Specifies the number of reschedule attempts allowed in the configured
      # interval. Defaults vary by job type.
      attempts = 0;
      # Enables unlimited reschedule attempts. If this is set to true the
      # attempts and interval fields are not used.
      unlimited = false;
    };

    # Specifies a key-value map that annotates with user-defined metadata.
    meta = {
      # Only top level "KEY=STRING" are allowed!
      TASK_DRIVER = if execTaskDriver then "exec" else "podman";
      TASK_WORKDIR = task_workdir;
      TASK_STATEDIR = task_statedir;
      SUPERVISOR_NIX = task_supervisor_nix;
      SUPERVISORD_URL = task_supervisord_url;
      SUPERVISORD_CONFIG = task_supervisord_conf;
      SUPERVISORD_LOGLEVEL = task_supervisord_loglevel;
      ONE_TRACER_PER_NODE = oneTracerPerNode;
    };

    # A group defines a series of tasks that should be co-located
    # on the same client (host). All tasks within a group will be
    # placed on the same host.
    # https://developer.hashicorp.com/nomad/docs/job-specification/group
    group = let
      valueF = (taskName: serviceName: portName: portNum: nodeSpec: (groupDefaults // {

        # Specifies the number of instances that should be running under for
        # this group. This value must be non-negative. This defaults to the min
        # value specified in the scaling block, if present; otherwise, this
        # defaults to 1
        count = 1;

        # The reschedule stanza specifies the group's rescheduling strategy. If
        # specified at the job level, the configuration will apply to all groups
        # within the job. If the reschedule stanza is present on both the job
        # and the group, they are merged with the group stanza taking the highest
        # precedence and then the job.
        # To disable rescheduling, set the attempts parameter to zero and
        # unlimited to false.
        reschedule = {
          # Specifies the number of reschedule attempts allowed in the
          # configured interval. Defaults vary by job type.
          attempts = 0;
          # Enables unlimited reschedule attempts. If this is set to true the
          # attempts and interval fields are not used.
          unlimited = false;
        };

        # Specifies the restart policy for all tasks in this group. If omitted,
        # a default policy exists for each job type, which can be found in the
        # restart stanza documentation.
        restart = {
          attempts = 0;
          mode = "fail";
        };

        # Specifies a key-value map that annotates with user-defined metadata.
        # Used as a "template" to generate the envars passed to the container.
        # This makes it easier to change them using `jq` inside the workbench!
        meta = null;

        # The network stanza specifies the networking requirements for the task
        # group, including the network mode and port allocations.
        # https://developer.hashicorp.com/nomad/docs/job-specification/network
        # TODO: Use "bridge" mode and port allocations ?
        network = {
          # FIXME: "bridge" right now is not working. Client error is:
          # {"@level":"error","@message":"prerun failed","@module":"client.alloc_runner","@timestamp":"2023-02-01T13:52:24.948596Z","alloc_id":"03faca46-0fdc-4ba0-01e9-50f67c088f99","error":"pre-run hook \"network\" failed: failed to create network for alloc: mkdir /var/run/netns: permission denied"}
          # {"@level":"info","@message":"waiting for task to exit","@module":"client.alloc_runner","@timestamp":"2023-02-01T13:52:24.983021Z","alloc_id":"03faca46-0fdc-4ba0-01e9-50f67c088f99","task":"tracer"}
          # {"@level":"info","@message":"marking allocation for GC","@module":"client.gc","@timestamp":"2023-02-01T13:52:24.983055Z","alloc_id":"03faca46-0fdc-4ba0-01e9-50f67c088f99"}
          # {"@level":"info","@message":"node registration complete","@module":"client","@timestamp":"2023-02-01T13:52:27.489795Z"}
          mode = "host";
          port = lib.listToAttrs (
            # If not oneTracerPerNode, an individual tracer task is needed (instead
            # of running a tracer alongside a node with supervisor)
            lib.optionals (profileData.value.node.tracer && !oneTracerPerNode) [
              # TODO: Leave empty or invent one?
              {name = "tracer"; value = {};}
            ]
            ++
            [
              {
                # All names of the form node#, without the "-", instead of node-#
                name = portName;
                value =
                  # The "podman" driver accepts "Mapped Ports", but not the "exec" driver
                  # https://developer.hashicorp.com/nomad/docs/job-specification/network#mapped-ports
                  # If you use a network in bridge mode you can use "Mapped Ports"
                  # https://developer.hashicorp.com/nomad/docs/job-specification/network#bridge-mode
                  if execTaskDriver
                  then {
                    to     = ''${toString portNum}'';
                    static = ''${toString portNum}'';
                  }
                  else {
                    to     = ''${toString portNum}'';
                  };
              }
            ]
          );
        };

        # The Consul namespace in which group and task-level services within the
        # group will be registered. Use of template to access Consul KV will read
        # from the specified Consul namespace. Specifying namespace takes
        # precedence over the -consul-namespace command line argument in job run.
        # namespace = "";
        # Not available as the documentations says: Extraneous JSON object property; No argument or block type is named "namespace".

        # The task stanza creates an individual unit of work, such as a Docker
        # container, web application, or batch processing.
        # https://developer.hashicorp.com/nomad/docs/job-specification/task
        task.${taskName} = taskDefaults // {

          # The meta stanza allows for user-defined arbitrary key-value pairs.
          # It is possible to use the meta stanza at the job, group, or task
          # level.
          # Here you can override the meta used at the group level.
          meta = null;

          # Specifies environment variables that will be passed to the running
          # process.
          # `null` because we are using a "template" (see below).
          env = {};

          # https://developer.hashicorp.com/nomad/docs/job-specification/service
          service = {
            # Specifies the name this service will be advertised as in Consul.
            # If not supplied, this will default to the name of the job, task
            # group, and task concatenated together with a dash, like
            # "docs-example-server". Each service must have a unique name within
            # the cluster. Names must adhere to RFC-1123 §2.1 and are limited to
            # alphanumeric and hyphen characters (i.e. [a-z0-9\-]), and be less
            # than 64 characters in length.
            name = serviceName;
            # Specifies the service registration provider to use for service
            # registrations. Valid options are either consul or nomad. All
            # services within a single task group must utilise the same provider
            # value.
            provider = "nomad";
            # Specifies the port to advertise for this service. The value of
            # port depends on which address_mode is being used:
            # - alloc: Advertise the mapped to value of the labeled port and the
            # allocation address. If a to value is not set, the port falls back
            # to using the allocated host port. The port field may be a numeric
            # port or a port label specified in the same group's network block.
            # - driver: Advertise the port determined by the driver (e.g.
            # Docker). The port may be a numeric port or a port label specified
            # in the driver's ports field.
            # - host: Advertise the host port for this service. port must match
            # a port label specified in the network block.
            port = portName;
            # TODO: Use it to heartbeat with cardano-ping!!!
            # https://developer.hashicorp.com/nomad/docs/job-specification/check
            # check = {};
          };

          # Specifies the set of templates to render for the task. Templates can
          # be used to inject both static and dynamic configuration with data
          # populated from environment variables, Consul and Vault.
          template = [
            # Envars
            {
              # podman container input environment variables.
              env = true;
              # File name to create inside the allocation directory.
              # Created in NOMAD_DATA_DIR/alloc/ALLOC_ID/TASK_NAME/envars
              destination = "envars";
              # See runtime for available variables:
              # https://developer.hashicorp.com/nomad/docs/runtime/environment
              data = ''
                TASK_DRIVER="{{ env "NOMAD_META_TASK_DRIVER" }}"
                TASK_WORKDIR="{{ env "NOMAD_META_TASK_WORKDIR" }}"
                TASK_STATEDIR="{{ env "NOMAD_META_TASK_STATEDIR" }}"
                SUPERVISOR_NIX="{{ env "NOMAD_META_SUPERVISOR_NIX" }}"
                SUPERVISORD_URL="{{ env "NOMAD_META_SUPERVISORD_URL" }}"
                SUPERVISORD_CONFIG="{{ env "NOMAD_META_SUPERVISORD_CONFIG" }}"
                SUPERVISORD_LOGLEVEL="{{ env "NOMAD_META_SUPERVISORD_LOGLEVEL" }}"
              '';
              # Specifies the behavior Nomad should take if the rendered
              # template changes. Nomad will always write the new contents of
              # the template to the specified destination. The following
              # possible values describe Nomad's action after writing the
              # template to disk.
              change_mode = "noop";
              error_on_missing_key = true;
            }
            # entrypoint
            {
              env = false;
              destination = "${task_workdir}/entrypoint.sh";
              data = entrypoint;
              change_mode = "noop";
              error_on_missing_key = true;
            }
            # Dynamically generated addresses for debugging purposes
            {
              env = false;
              destination = "${task_workdir}/networking.json";
              data = ''
              {
              {{- $first := true -}}
              {{- range nomadServices -}}
                {{- if not $first -}}
                  ,
                {{- else -}}
                  {{- $first = false -}}
                {{- end }}
                "{{ .Name }}": {
                  {{- range nomadService .Name }}
                    "service": {
                        "address": "{{ .Address }}"
                      , "port": {{ .Port }}
                    }
                  , "env": {
                        "nomad": {
                            "ip":   "{{     env (printf "%v%v" "NOMAD_IP_"         .Name)    }}"
                          , "port":  {{ or (env (printf "%v%v" "NOMAD_PORT_"       .Name)) 0 }}
                          , "addr": "{{     env (printf "%v%v" "NOMAD_ADDR_"       .Name)    }}"
                        }
                      , "host": {
                            "ip":   "{{     env (printf "%v%v" "NOMAD_HOST_IP_"    .Name)    }}"
                          , "port":  {{ or (env (printf "%v%v" "NOMAD_HOST_PORT_"  .Name)) 0 }}
                          , "addr": "{{     env (printf "%v%v" "NOMAD_HOST_ADDR_"  .Name)    }}"
                        }
                      , "alloc": {
                            "port":  {{ or (env (printf "%v%v" "NOMAD_ALLOC_PORT_" .Name)) 0 }}
                        }
                    }
                  {{- end }}
                }
              {{- end }}
              }
              '';
              change_mode = "noop";
              error_on_missing_key = true;
            }
            # supervisord
            ## supervisord configuration file.
            {
              env = false;
              destination = "${task_supervisord_conf}";
              data = escapeTemplate (__readFile
                supervisorConf.INI);
              change_mode = "noop";
              error_on_missing_key = true;
            }
            # Generator
            ## Generator start.sh script.
            {
              env = false;
              destination = "${task_statedir}/generator/start.sh";
              data = escapeTemplate
                profileData.generator-service.startupScript.value;
              change_mode = "noop";
              error_on_missing_key = true;
            }
            ## Generator configuration file.
            {
              env = false;
              destination = "${task_statedir}/generator/run-script.json";
              data = escapeTemplate (__readFile
                profileData.generator-service.runScript.JSON.outPath);
              change_mode = "noop";
              error_on_missing_key = true;
            }
          ]
          ++
          # Tracer(s)
          ## If using oneTracerPerNode no "tracer volumes" need to be mounted
          ## (because of no socket sharing between tasks), and tracer files are
          ## created using templates.
          (lib.optionals (profileData.value.node.tracer && oneTracerPerNode) [
            ## Tracer start.sh script.
            {
              env = false;
              destination = "${task_statedir}/tracer/start.sh";
              data = escapeTemplate
                profileData.tracer-service.startupScript.value;
              change_mode = "noop";
              error_on_missing_key = true;
            }
            ## Tracer configuration file.
            {
              env = false;
              destination = "${task_statedir}/tracer/config.json";
              data = escapeTemplate (lib.generators.toJSON {}
                # TODO / FIXME: Ugly config patching!
                (lib.attrsets.recursiveUpdate
                  # When running locally every tracer has a 127.0.0.1 address
                  # and EKG and prometheus ports clash!
                  (builtins.removeAttrs
                    profileData.tracer-service.config.value
                    [ "hasEKG" "hasPrometheus" "hasRTView" ]
                  )
                  # Make it easier to download every log
                  {
                      logging = builtins.map
                        (value: value // { logRoot="./logRoot"; })
                        profileData.tracer-service.config.value.logging
                      ;
                  }
                )
              );
              change_mode = "noop";
              error_on_missing_key = true;
            }
          ])
          ++
          # Node(s)
          (lib.lists.flatten (lib.mapAttrsToList
            (_: nodeSpec: [
              ## Node start.sh script.
              {
                env = false;
                destination = "${task_statedir}/${nodeSpec.name}/start.sh";
                data = escapeTemplate (
                  let scriptValue = profileData.node-services."${nodeSpec.name}".startupScript.value;
                  in if execTaskDriver
                    then (startScriptToGoTemplate
                      nodeSpec.name
                      ("perf-" + nodeSpec.name)
                      ("node" + (toString nodeSpec.i))
                      nodeSpec
                      scriptValue
                    )
                    else scriptValue
                );
                change_mode = "noop";
                error_on_missing_key = true;
              }
              ## Node configuration file.
              {
                env = false;
                destination = "${task_statedir}/${nodeSpec.name}/config.json";
                data = escapeTemplate (lib.generators.toJSON {}
                  profileData.node-services."${nodeSpec.name}".nodeConfig.value);
                change_mode = "noop";
                error_on_missing_key = true;
              }
              ## Node topology file.
              {
                env = false;
                destination = "${task_statedir}/${nodeSpec.name}/topology.json";
                data = escapeTemplate (
                  let topology = profileData.node-services."${nodeSpec.name}".topology;
                  in if execTaskDriver
                    then (topologyToGoTemplate topology.value)
                    else (__readFile           topology.JSON )
                );
                change_mode = "noop";
                error_on_missing_key = true;
              }
            ])
            profileData.node-specs.value
          ))
          ;

          # Specifies logging configuration for the stdout and stderr of the
          # task.
          # TODO: Why not use the stdout and stderr instead of volumes?
          logs = null;

          # Specifies a configurable kill signal for a task, where the default
          # is SIGINT (or SIGTERM for docker, or CTRL_BREAK_EVENT for raw_exec
          # on Windows). Note that this is only supported for drivers sending
          # signals (currently docker, exec, raw_exec, and java drivers).
          kill_signal = "SIGINT";

          # Specifies the duration to wait for an application to gracefully quit
          # before force-killing. Nomad first sends a kill_signal. If the task
          # does not exit before the configured timeout, SIGKILL is sent to the
          # task. Note that the value set here is capped at the value set for
          # "max_kill_timeout" on the agent running the task, which has a
          # default value of 30 seconds.
          # Default: (string: "5s").
          kill_timeout = "15s";

          }
          //
          (if execTaskDriver
            then {
              driver = "exec";

              config = {

                command = "${containerSpecs.containerPkgs.bashInteractive.nix-store-path}/bin/bash";

                args = ["${task_workdir}/entrypoint.sh"];

                nix_installables =
                  (lib.attrsets.mapAttrsToList
                    (name: attr: attr.nix-store-path)
                    containerSpecs.containerPkgs
                  )
                ;

              };
            } else {
              driver = "podman";

              # Specifies the driver configuration, which is passed directly to the
              # driver to start the task. The details of configurations are specific
              # to each driver, so please see specific driver documentation for more
              # information.
              # https://github.com/hashicorp/nomad-driver-podman#task-configuration
              config = {

                command = "${containerSpecs.containerPkgs.bashInteractive.nix-store-path}/bin/bash";

                args = ["${task_workdir}/entrypoint.sh"];

                # The image to run. Accepted transports are docker (default if
                # missing), oci-archive and docker-archive. Images reference as
                # short-names will be treated according to user-configured
                # preferences.
                image = "${containerSpecs.ociImage.imageName}:${containerSpecs.ociImage.imageTag}";

                # Always pull the latest image on container start.
                force_pull = false;

                # Podman redirects its combined stdout/stderr logstream directly
                # to a Nomad fifo. Benefits of this mode are: zero overhead,
                # don't have to worry about log rotation at system or Podman
                # level. Downside: you cannot easily ship the logstream to a log
                # aggregator plus stdout/stderr is multiplexed into a single
                # stream.
                logging = {
                  # The other option is: "journald"
                  driver = "nomad";
                };

                # The hostname to assign to the container. When launching more
                # than one of a task (using count) with this option set, every
                # container the task starts will have the same hostname.
                hostname = taskName;

                network_mode = "host";

                # This can be used here but not with "exec"!
                # All names of the form node#, without the "-", instead of node-#
                ports = [ portName ];

                # A list of /container_path strings for tmpfs mount points. See
                # podman run --tmpfs options for details.
                tmpfs = [
                  "/tmp"
                ];

                # A list of host_path:container_path:options strings to bind
                # host paths to container paths. Named volumes are not supported.
                volumes = [];

                # The working directory for the container. Defaults to the
                # default set in the image.
                #working_dir = ''{{ env "NOMAD_META_TASK_WORKDIR" }}'';
                working_dir = task_workdir;

              };
            }
          );
      }));
      in lib.listToAttrs (
        # If not oneTracerPerNode, an individual tracer task is needed (instead
        # of running a tracer alongside a node with supervisor)
        lib.optionals (profileData.value.node.tracer && !oneTracerPerNode) [
          {
            name = "tracer";
            value = valueF
              "tracer"                               # taskName
              "perf-tracer"                          # serviceName
              "tracer"                               # portName (can't have "-")
              0                                      # portNum
              {};                                    # node-specs
          }
        ]
        ++
        (lib.mapAttrsToList
          (_: nodeSpec: {
            /* Nomad randomly changes '-' to '_', so switching all service/ports
            # names to '_'
            NOMAD_ADDR_node_0=192.168.2.125:30000
            NOMAD_ADDR_node_1=192.168.2.125:30001
            NOMAD_HOST_ADDR_node-0=192.168.2.125:30000
            NOMAD_HOST_ADDR_node-1=192.168.2.125:30001
            */
            name = nodeSpec.name;
            value = valueF
              nodeSpec.name                          # taskName
              ("perf-node-" + (toString nodeSpec.i)) # serviceName
              ("node" + (toString nodeSpec.i))       # portName (can't have "-")
              nodeSpec.port                          # portNum
              nodeSpec;                              # node-specs
          })
          (profileData.node-specs.value)
        )
      );

    };

  };

  jobDefaults = {
    ########################################
    # Vault / Consul: Not used locally, yet!
    ########################################

    # Specifies the set of Vault policies required by all tasks in this job.
    vault = null;

    # Specifies the Vault token that proves the submitter of the job has access
    # to the specified policies in the vault stanza. This field is only used to
    # transfer the token and is not stored after job submission.
    vault_token = "";

    # Specifies the Consul token that proves the submitter of the job has access
    # to the Service Identity policies associated with the job's Consul Connect
    # enabled services. This field is only used to transfer the token and is not
    # stored after job submission.
    consul_token = "";

    #############################
    # Job miscellaneous defaults:
    #############################

    # Controls whether the scheduler can make partial placements if optimistic
    # scheduling resulted in an oversubscribed node. This does not control
    # whether all allocations for the job, where all would be the desired count
    # for each task group, must be placed atomically. This should only be used
    # for special circumstances.
    all_at_once = false;

    # This can be provided multiple times to define additional constraints. See
    # the Nomad constraint reference for more details.
    # https://developer.hashicorp.com/nomad/docs/job-specification/constraint
    constraint = null;

    # This can be provided multiple times to define preferred placement
    # criteria. See the Nomad affinity reference for more details.
    affinity = null;

    # Specifies the groups strategy for migrating off of draining nodes. If
    # omitted, a default migration strategy is applied. Only service jobs with a
    # count greater than 1 support migrate stanzas.
    migrate = null;

    # The "multiregion" stanza specifies that a job will be deployed to multiple
    # federated regions. If omitted, the job will be deployed to a single region
    # - the one specified by the region field or the `-region` command line flag
    # to `nomad job run`.
    multiregion = null;

    # Specifies the job as a parameterized job such that it can be dispatched
    # against.
    parameterized = null;

    # Allows the job to be scheduled at fixed times, dates or intervals.
    periodic = null;

    # Specifies the job priority which is used to prioritize scheduling and
    # access to resources. Must be between 1 and 100 inclusively, with a larger
    # value corresponding to a higher priority. Priority only has an effect when
    # job preemption is enabled. It does not have an effect on which of multiple
    # pending jobs is run first.
    priority = 50;

    # This can be provided multiple times to define criteria for spreading
    # allocations across a node attribute or metadata. See the Nomad spread
    # reference for more details.
    # https://developer.hashicorp.com/nomad/docs/job-specification/spread
    spread = null;

    # Specifies the task's update strategy. When omitted, a default update
    # strategy is applied.
    update = null;
  };

  groupDefaults = {
    ########################################
    # Vault / Consul: Not used locally, yet!
    ########################################

    # Specifies Consul configuration options specific to the group.
    consul = null;

    # Specifies the set of Vault policies required by all tasks in this group.
    # Overrides a vault block set at the job level.
    vault = null;

    ###############################
    # Group miscellaneous defaults:
    ###############################

    # This can be provided multiple times to define preferred placement
    # criteria
    affinity = null;

    # Specifies user-defined constraints on the task. This can be provided
    # multiple times to define additional constraints.
    constraint = null;

    # Specifies the ephemeral disk requirements of the group. Ephemeral disks
    # can be marked as sticky and support live data migrations.
    ephemeral_disk = null;

    # Specifies a duration during which a Nomad client will attempt to
    # reconnect allocations after it fails to heartbeat in the heartbeat_grace
    # window. See the example code below for more details. This setting cannot
    # be used with stop_after_client_disconnect.
    # max_client_disconnect = "";
    # Error using the documentation default: Unsuitable value type; Unsuitable duration value: time: invalid duration ""

    # Specifies the groups strategy for migrating off of draining nodes. If
    # omitted, a default migration strategy is applied. Only service jobs with
    # a count greater than 1 support migrate stanzas.
    migrate = null;

    # Specifies integrations with Consul for service discovery. Nomad
    # automatically registers each service when an allocation is started and
    # de-registers them when the allocation is destroyed.
    service = null;

    # Specifies the duration to wait when stopping a group's tasks. The delay
    # occurs between Consul deregistration and sending each task a shutdown
    # signal. Ideally, services would fail healthchecks once they receive a
    # shutdown signal. Alternatively shutdown_delay may be set to give
    # in-flight requests time to complete before shutting down. A group level
    # shutdown_delay will run regardless if there are any defined group
    # services. In addition, tasks may have their own shutdown_delay which
    # waits between deregistering task services and stopping the task.
    shutdown_delay = "0s";

    # Specifies a duration after which a Nomad client will stop allocations,
    # if it cannot communicate with the servers. By default, a client will not
    # stop an allocation until explicitly told to by a server. A client that
    # fails to heartbeat to a server within the heartbeat_grace window and any
    # allocations running on it will be marked "lost" and Nomad will schedule
    # replacement allocations. The replaced allocations will normally continue
    # to run on the non-responsive client. But you may want them to stop
    # instead — for example, allocations requiring exclusive access to an
    # external resource. When specified, the Nomad client will stop them after
    # this duration. The Nomad client process must be running for this to
    # occur. This setting cannot be used with max_client_disconnect.
    # stop_after_client_disconnect = "";
    # Error using the documentation default: Unsuitable value type; Unsuitable duration value: time: invalid duration ""

    # This can be provided multiple times to define criteria for spreading
    # allocations across a node attribute or metadata. See the Nomad spread
    # reference for more details.
    # https://developer.hashicorp.com/nomad/docs/job-specification/spread
    spread = null;

    # Specifies the task's update strategy. When omitted, a default update
    # strategy is applied.
    update = null;
  };

  taskDefaults = {

    # Specifies where a group volume should be mounted.
    volume_mount = null;

    ########################################
    # Vault / Consul: Not used locally, yet!
    ########################################

    # Specifies the set of Vault policies required by the task. This
    # overrides any vault block set at the group or job level.
    vault = null;

    ##############################
    # Task miscellaneous defaults:
    ##############################

    # This can be provided multiple times to define preferred placement
    # criteria
    affinity = null;

    # Defines an artifact to download before running the task. This may be
    # specified multiple times to download multiple artifacts.
    artifact = null;

    # Specifies user-defined constraints on the task. This can be provided
    # multiple times to define additional constraints.
    constraint = null;

    # Configures the task to have access to dispatch payloads.
    dispatch_payload = null;

    # Used internally to manage tasks according to the value of this
    # field. Initial use case is for Consul Connect.
    # kind = null;

    # Specifies whether the task is the leader task of the task group. If
    # set to true, when the leader task completes, all other tasks within
    # the task group will be gracefully shutdown. The shutdown process
    # starts by applying the shutdown_delay if configured. It then stops
    # the the leader task first, followed by non-sidecar and non-poststop
    # tasks, and finally sidecar tasks. Once this process completes,
    # post-stop tasks are triggered. See the lifecycle documentation for a
    # complete description of task lifecycle management.
    leader = false; # Only one task may be marked as leader!

    # Specifies the minimum resource requirements such as RAM, CPU and
    # devices.
    resources = null;

    # Specifies the duration to wait when killing a task between removing
    # it from Consul and sending it a shutdown signal. Ideally services
    # would fail healthchecks once they receive a shutdown signal.
    # Alternatively shutdown_delay may be set to give in flight requests
    # time to complete before shutting down. In addition, task groups may
    # have their own shutdown_delay which waits between deregistering
    # group services and stopping tasks.
    shutdown_delay = "0s";

    # Specifies the user that will run the task. Defaults to nobody for
    # the "exec" and "java" drivers. "Docker" and "rkt" images specify
    # their own default users. This can only be set on Linux platforms,
    # and clients can restrict which drivers are allowed to run tasks as
    # certain users.
    # user = null;
  };

  # About the "template" stanza:
  # The templates documentations show "left_delimiter" and "right_delimiter"
  # options which default to "{{" and "}}" repectively. See:
  # https://developer.hashicorp.com/nomad/docs/job-specification/template#left_delimiter
  # But those don't work to escape text to avoid HCL expressions interpolation.
  # We are using what says here:
  # "In both quoted and heredoc string expressions, Nomad supports template
  # sequences that begin with ${ and %{. These are described in more detail in
  # the following section. To include these sequences literally without
  # beginning a template sequence, double the leading character: $${ or %%{."
  # https://developer.hashicorp.com/nomad/docs/job-specification/hcl2/expressions#string-literals
  escapeTemplate = str: builtins.replaceStrings ["\${" "%{"] ["\$\${" "%%{"] str;

  startScriptToGoTemplate = taskName: serviceName: portName: nodeSpec: startScript:
    builtins.replaceStrings
      [
        # Address string from
        ''--host-addr 127.0.0.1''
        # Port string from
        ''--port ${toString profileData.node-specs.value."${nodeSpec.name}".port}''
      ]
      [
        # Address string to
        #''--host-addr {{ env "NOMAD_IP_${portName}" }}''
        ''--host-addr {{ env "NOMAD_HOST_IP_${portName}" }}''
        #''--host-addr {{range nomadService "${serviceName}"}}{{.Address}}{{end}}''
        #''--host-addr 0.0.0.0''
        # Port string to
        #''--port {{ env "NOMAD_PORT_${portName}" }}''
        ''--port {{ env "NOMAD_HOST_PORT_${portName}" }}''
        #''--port {{ env "NOMAD_ALLOC_PORT_${name}" }}''
        #''--port {{range nomadService "${serviceName}"}}{{.Port}}{{end}}''
      ]
      startScript
  ;

  # Move from a topology.json with all addresses being "127.0.0.01" to one with
  # all addresses being a placeholder like "{{NOMAD_IP_node-X}}"
  #
  # topology.json example:
  # {
  #   "Producers": [
  #     {
  #       "addr": "127.0.0.1",
  #       "port": 30001,
  #       "valency": 1
  #     }
  #   ]
  # }
  # Example node-specs.json:
  # {
  #   "node-1": {
  #     "i": 1,
  #     "kind": "pool",
  #     "pools": 1,
  #     "autostart": true,
  #     "shutdown_on_slot_synced": null,
  #     "name": "node-1",
  #     "isProducer": true,
  #     "port": 30001,
  #     "shutdown_on_block_synced": 3
  #   }
  # }
  # Input is a profileData.node-services."${nodeSpec.name}".topology.value
  topologyToGoTemplate =
    let
#            "addr": "127.0.0.1"
#          , "port":  {{range nomadService "${"perf-node-" + (toString mergedNodeSpecs.i)}"}}{{.Port}}{{end}}
# OR
#            "addr": "{{range nomadService "${"perf-node-" + (toString mergedNodeSpecs.i)}"}}{{.Address}}{{end}}"
#          , "port":  {{range nomadService "${"perf-node-" + (toString mergedNodeSpecs.i)}"}}{{.Port}}{{end}}
# OR
#            "addr": "{{ env "NOMAD_IP_${"node" + (toString mergedNodeSpecs.i)}"   }}"
#          , "port":  {{ env "NOMAD_PORT_${"node" + (toString mergedNodeSpecs.i)}" }}
# OR
#            "addr": "''${NOMAD_HOST_IP_${"node" + (toString mergedNodeSpecs.i)}}"
#          , "port":  ''${NOMAD_HOST_PORT_${"node" + (toString mergedNodeSpecs.i)}}
      mergedNodeSpecToStr = mergedNodeSpecs: ''
        {
            "addr": "{{range nomadService "${"perf-node-" + (toString mergedNodeSpecs.i)}"}}{{.Address}}{{end}}"
          , "port":  {{range nomadService "${"perf-node-" + (toString mergedNodeSpecs.i)}"}}{{.Port}}{{end}}
          , "valency": ${toString mergedNodeSpecs.valency}
        }
      '';
    in
      topology: ''
        {
          "Producers": [
            ${builtins.concatStringsSep "," (
                builtins.map
                  mergedNodeSpecToStr
                  (insertNodeSpecsInProducers topology).Producers
            )}
          ]
        }
      ''
  ;
  # builtins.concatStringsSep
  insertNodeSpecsInProducers =
    let fromPortToNodeSpec = port: (
      builtins.head # Must exist!
        # Returns
        (builtins.filter
          (nodeSpec: nodeSpec.port == port)
          (lib.attrsets.mapAttrsToList
            (nodeName: nodeSpec: nodeSpec)
            profileData.node-specs.value
          )
        )
    );
# lib.debug.traceVal
    in topology: builtins.mapAttrs
      (key: value:
        if key == "Producers"
        then builtins.map
          (remoteAddress:
            remoteAddress // (fromPortToNodeSpec remoteAddress.port)
          )
          value
        else value # Error!
      )
      topology
  ;

in lib.generators.toJSON {} clusterJob
