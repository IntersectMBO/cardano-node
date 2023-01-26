################################################################################
# A Nomad job description intended to be reused between local/development
# clusters and SRE infrastructure used for long-running cloud benchmarks. Why?
# To make it easier to improve and debug the almighty workbench!
################################################################################
{ pkgs
, lib
, stateDir
, profileNix
, ociImages
# Needs unix_http_server.file
, supervisorConf
, oneTracerPerNode ? false
}:

let

  # Container defaults:
  ## Stored on the job's "meta" stanza and intended to be overrided with `jq`.
  ## See ./oci-images.nix for further details.
  #
  # The template stanza can only generate files inside /local (NOMAD_TASK_DIR)
  ## - https://developer.hashicorp.com/nomad/docs/job-specification/template#template-destinations
  ## - https://developer.hashicorp.com/nomad/docs/runtime/environment#task-directories
  ## - https://developer.hashicorp.com/nomad/docs/concepts/filesystem
  container_workdir = "/local";
  # Usually "/local/run/current"
  container_statedir = "${container_workdir}/${stateDir}";
  # A link to the supervisord nix-installed inside the OCI image.
  container_supervisor_nix = "${container_statedir}/supervisor/nix-store";
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
  container_supervisord_url = "unix://${supervisorConf.value.unix_http_server.file}";
  # Location of the supervisord config file inside the container.
  # This file can be mounted as a volume or created as a template.
  container_supervisord_conf = "${container_statedir}/supervisor/supervisord.conf";
  container_supervisord_loglevel = "info";

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
    region = "workbench-region-1";

    #  A list of datacenters in the region which are eligible for task
    # placement. This must be provided, and does not have a default.
    datacenters = [ "workbench-datacenter-1" ];

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
    meta = null;

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

    # A group defines a series of tasks that should be co-located
    # on the same client (host). All tasks within a group will be
    # placed on the same host.
    # https://developer.hashicorp.com/nomad/docs/job-specification/group
    group."workbench-cluster-job-group" = groupDefaults // {

      # Specifies the number of instances that should be running under for this
      # group. This value must be non-negative. This defaults to the min value
      # specified in the scaling block, if present; otherwise, this defaults to
      # 1
      count = 1;

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

      # Specifies the restart policy for all tasks in this group. If omitted, a
      # default policy exists for each job type, which can be found in the restart
      # stanza documentation.
      restart = {
        attempts = 0;
        mode = "fail";
      };

      # Specifies a key-value map that annotates with user-defined metadata.
      # Used as a "template" to generate the envars passed to the container.
      # This makes it easier to change them using `jq` inside the workbench!
      meta = {
        # Only top level "KEY=STRING" are allowed!
        WORKING_DIRECTORY = container_workdir;
        STATE_DIRECTORY = container_statedir;
        SUPERVISOR_NIX = container_supervisor_nix;
        SUPERVISORD_URL = container_supervisord_url;
        SUPERVISORD_CONFIG = container_supervisord_conf;
        SUPERVISORD_LOGLEVEL = container_supervisord_loglevel;
        ONE_TRACER_PER_NODE = oneTracerPerNode;
      };

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
        port =
          let
            valueF = (port: {
              to = ''${toString port}'';
            });
          in lib.listToAttrs (
            # If not oneTracerPerNode, an individual tracer task is needed (instead
            # of running a tracer alongside a node with supervisor)
            lib.optionals (profileNix.value.node.tracer && !oneTracerPerNode) [
              {name = "tracer";    value = valueF 31000;}
            ]
            ++
            (lib.mapAttrsToList
              (_: nodeSpec: {
                name = nodeSpec.name;
                value = valueF nodeSpec.port;
              })
              (profileNix.node-specs.value)
            )
          );
      };

      # TODO:
      # Specifies the volumes that are required by tasks within the group.
      # volume

      ########################################
      # Vault / Consul: Not used locally, yet!
      ########################################

      # Specifies Consul configuration options specific to the group.
      consul = null;

      # Specifies the set of Vault policies required by all tasks in this group.
      # Overrides a vault block set at the job level.
      vault = null;

      # The Consul namespace in which group and task-level services within the
      # group will be registered. Use of template to access Consul KV will read
      # from the specified Consul namespace. Specifying namespace takes
      # precedence over the -consul-namespace command line argument in job run.
      # namespace = "";
      # Not available as the documentations says: Extraneous JSON object property; No argument or block type is named "namespace".

      # The task stanza creates an individual unit of work, such as a Docker
      # container, web application, or batch processing.
      # https://developer.hashicorp.com/nomad/docs/job-specification/task
      task = let
        valueF = (name: volumes: (taskDefaults // {

          driver = "podman";

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
            name = "${name}";
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
            port = "${name}";
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
# TODO
            # Dynamically generated addresses
/*
            {
              env = false;
              destination = "${container_statedir}/addresses.json";
              data = ''
                <<EOH
                  {
                    {{ range nomadService "redis" }}
                      DEMO_REDIS_ADDR={{ .Address }}:{{ .Port }}
                    {{ end }}
                  }
                EOH
              ''
              change_mode = "noop";
              error_on_missing_key = true;
            }
*/
            # supervisord
            ## supervisord configuration file.
            {
              env = false;
              destination = "${container_supervisord_conf}";
              data = escapeTemplate (__readFile
                supervisorConf.INI);
              change_mode = "noop";
              error_on_missing_key = true;
            }
            # Generator
            ## Generator start.sh script.
            {
              env = false;
              destination = "${container_statedir}/generator/start.sh";
              data = escapeTemplate
                profileNix.generator-service.startupScript.value;
              change_mode = "noop";
              error_on_missing_key = true;
            }
            ## Generator configuration file.
            {
              env = false;
              destination = "${container_statedir}/generator/run-script.json";
              data = escapeTemplate (__readFile
                profileNix.generator-service.runScript.JSON.outPath);
              change_mode = "noop";
              error_on_missing_key = true;
            }
          ]
          ++
          # Tracer
          ## If using oneTracerPerNode no "tracer volumes" need to be mounted
          ## (because of no socket sharing between tasks), and tracer files are
          ## created using templates.
          (lib.optionals (profileNix.value.node.tracer && oneTracerPerNode) [
            ## Tracer start.sh script.
            {
              env = false;
              destination = "${container_statedir}/tracer/start.sh";
              data = escapeTemplate
                profileNix.tracer-service.startupScript.value;
              change_mode = "noop";
              error_on_missing_key = true;
            }
            ## Tracer configuration file.
            {
              env = false;
              destination = "${container_statedir}/tracer/config.json";
              data = escapeTemplate (lib.generators.toJSON {}
                # TODO / FIXME: Ugly!
                # When running locally every tracer has a 127.0.0.1 address
                # and EKG and prometheus ports clash!
                (builtins.removeAttrs
                  profileNix.tracer-service.config.value
                  [ "hasEKG" "hasPrometheus" "hasRTView" ]
                )
              );
              change_mode = "noop";
              error_on_missing_key = true;
            }
          ])
          ++
          # Nodes
          (lib.lists.flatten (lib.mapAttrsToList
            (_: nodeSpec: [
              ## Node start.sh script.
              {
                env = false;
                destination = "${container_statedir}/${nodeSpec.name}/start.sh";
                data = escapeTemplate
                    profileNix.node-services."${nodeSpec.name}".startupScript.value;
                change_mode = "noop";
                error_on_missing_key = true;
              }
              ## Node configuration file.
              {
                env = false;
                destination = "${container_statedir}/${nodeSpec.name}/config.json";
                data = escapeTemplate (lib.generators.toJSON {}
                  profileNix.node-services."${nodeSpec.name}".nodeConfig.value);
                change_mode = "noop";
                error_on_missing_key = true;
              }
              ## Node topology file.
              {
                env = false;
                destination = "${container_statedir}/${nodeSpec.name}/topology.json";
                data = escapeTemplate (lib.generators.toJSON {}
                  profileNix.node-services."${nodeSpec.name}".topology.value);
                change_mode = "noop";
                error_on_missing_key = true;
              }
            ])
            profileNix.node-specs.value
          ))
          ;

          # Specifies where a group volume should be mounted.
          volume_mount = null; #TODO

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
          kill_timeout = "5s";

          # Specifies the driver configuration, which is passed directly to the
          # driver to start the task. The details of configurations are specific
          # to each driver, so please see specific driver documentation for more
          # information.
          # https://github.com/hashicorp/nomad-driver-podman#task-configuration
          config = {

            # The image to run. Accepted transports are docker (default if
            # missing), oci-archive and docker-archive. Images reference as
            # short-names will be treated according to user-configured
            # preferences.
            image = "${ociImages.value.clusterNode.imageName}:${ociImages.value.clusterNode.imageTag}";

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
            hostname = name;

            network_mode = "host";

            # A list of /container_path strings for tmpfs mount points. See
            # podman run --tmpfs options for details.
            tmpfs = [
              "/tmp"
            ];

            # A list of host_path:container_path:options strings to bind
            # host paths to container paths. Named volumes are not supported.
            volumes = volumes;

            # The working directory for the container. Defaults to the
            # default set in the image.
            working_dir = container_workdir;

          };

          ########################################
          # Vault / Consul: Not used locally, yet!
          ########################################

          # Specifies the set of Vault policies required by the task. This
          # overrides any vault block set at the group or job level.
          vault = null;

        }));
      in lib.listToAttrs (
        # If not oneTracerPerNode, an individual tracer task is needed (instead
        # of running a tracer alongside a node with supervisor)
        lib.optionals (profileNix.value.node.tracer && !oneTracerPerNode) [
          {name = "tracer";    value = valueF "tracer" [];}
        ]
        ++
        (lib.mapAttrsToList
          (_: nodeSpec: {
            name = nodeSpec.name;
            value = valueF nodeSpec.name [];
          })
          (profileNix.node-specs.value)
        )
      );

    };

  };};

  jobDefaults = {
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

    # Specifies integrations with Consul for service discovery. Nomad
    # automatically registers when a task is started and de-registers it
    # when the task dies.
    service = null;

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

in pkgs.writeText "workbench-cluster-nomad-job.json"
  (lib.generators.toJSON {} clusterJob)
