################################################################################
# A Nomad job description intended to be reused between local/development
# clusters and SRE infrastructure used for long-running cloud benchmarks. Why?
# To make it easier to improve and debug the almighty workbench!
################################################################################
{ pkgs
, lib
, stateDir
, profileData
, containerSpecs
, execTaskDriver
, generatorTaskName
, oneTracerPerNode ? false
, withSsh ? false
}:

let

  # Filesystem
  #
  # Nomad creates a working directory for each allocation on a client. This
  # directory can be found in the Nomad data_dir at ./alloc/«alloc_id». The
  # allocation working directory is where Nomad creates task directories and
  # directories shared between tasks, write logs for tasks, and downloads
  # artifacts or templates.
  # https://developer.hashicorp.com/nomad/docs/concepts/filesystem
  #
  # For example:
  ## - Driver "exec" ("chroot" isolation):
  ## - - NOMAD_ALLOC_DIR=/alloc
  ## - - NOMAD_TASK_DIR=/local
  ## - Driver "raw_exec" ("none" isolation):
  ## - - NOMAD_ALLOC_DIR=DATA-DIR/alloc/XXXXXXXX/alloc
  ## - - NOMAD_TASK_DIR=DATA-DIR/alloc/XXXXXXXX/TASK-NAME/local
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

  # Task's filesystem / working directory (maybe container or chroot) defaults:
  #
  # When using the isolated fork task driver ("exec")
  ## Default values below are stored in the job's "meta" stanza to be able to
  ## overrided them with 'jq' from a workbench shell. These values in "meta"
  ## are used to programatically create a "template" with "env = true;" so they
  ## are automagically reachable as envars inside the Task's entrypoint and
  ## 'supervisord' programs.
  ## Values go: Nix (defaults) -> meta -> template -> envars
  #
  ## See ./oci-images.nix for further details if using the `podman` driver.
  ## For the `exec` driver almost everything is here.
  #

  # A symlink to the supervisord nix-installed inside the OCI image/chroot.
  # We need to be able to `nomad exec supervisorctl ...` , for this the path
  # of the installed supervisor binaries is needed.
  task_supervisor_nix = "${stateDir}/supervisor/nix-store";
  # Location of the supervisord config file inside the container.
  # This file can be mounted as a volume or created as a template.
  task_supervisord_conf = "${stateDir}/supervisor/supervisord.conf";
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
  unixHttpServerPort = "/tmp/supervisor-{{ env \"NOMAD_TASK_NAME\" }}.sock";
  task_supervisord_url = "unix://${unixHttpServerPort}";
  task_supervisord_loglevel = "info";

  entrypoint =
    let
      coreutils  = containerSpecs.containerPkgs.coreutils.nix-store-path;
      supervisor = containerSpecs.containerPkgs.supervisor.nix-store-path;
    in escapeTemplate
      ''
      # Store entrypoint's envars and "uname" in a file for debugging purposes.
      ${coreutils}/bin/env               > "''${NOMAD_TASK_DIR}"/entrypoint.env
      ${coreutils}/bin/uname -a          > "''${NOMAD_TASK_DIR}"/entrypoint.uname
      ${coreutils}/bin/cat /proc/cpuinfo > "''${NOMAD_TASK_DIR}"/entrypoint.cpuinfo
      # Directories map to use when `nomad fs` and `nomad alloc exec`
      SUPERVISOR_NIX="''${NOMAD_TASK_DIR}/${task_supervisor_nix}"
      SUPERVISOR_CONF="''${NOMAD_TASK_DIR}/${task_supervisord_conf}"
      echo \
        "{                                                                     \
            \"nomad\": {                                                       \
              \"alloc\": \"''${NOMAD_ALLOC_DIR}\"                              \
            , \"task\":  \"''${NOMAD_TASK_DIR}\"                               \
          }                                                                    \
          , \"workbench\":  {                                                  \
              \"state\": \"''${NOMAD_TASK_DIR}/${stateDir}\"                   \
          }                                                                    \
          , \"supervisor\": {                                                  \
              \"nix\":    \"''${SUPERVISOR_NIX}\"                              \
            , \"config\": \"''${SUPERVISOR_CONF}\"                             \
            , \"socket\": \"${unixHttpServerPort}\"                            \
            , \"url\": \"${task_supervisord_url}\"                             \
          }                                                                    \
        }" \
      > "''${NOMAD_TASK_DIR}"/entrypoint.dirs

      # Only needed for "exec" ?
      if test "''${TASK_DRIVER}" = "exec"
      then
        cd "''${NOMAD_TASK_DIR}"
      fi

      # Create a symlink to 'supervisor' Nix Store folder so we can call it from
      # 'ssh' or 'nomad exec' without having it in PATH or knowing the currently
      # running version. But first check if it already exists to be able to
      # restart containers without errors.
      if ! test -e "''${SUPERVISOR_NIX}"
      then
        ${coreutils}/bin/ln -s "${supervisor}" "''${SUPERVISOR_NIX}"
      fi

      # The SUPERVISORD_LOGLEVEL variable defaults to "info" if not present
      # The logging level at which supervisor should write to the
      # activity log. Valid levels are trace, debug, info, warn, error
      # and critical.
      LOGLEVEL="''${SUPERVISORD_LOGLEVEL:-info}"

      # Start `supervisord` on the foreground.
      # Make sure it never runs in unbuffered mode:
      # https://docs.python.org/3/using/cmdline.html#envvar-PYTHONUNBUFFERED
      PYTHONUNBUFFERED="" ${supervisor}/bin/supervisord --nodaemon --configuration "''${SUPERVISOR_CONF}" --loglevel="''${LOGLEVEL}"
      ''
  ;

  # About the JSON Job Specification and my odd assumptions:
  #
  # TL;DR; We are using what HashiCorp calls an unespecified format but it's the
  # same format the SRE team is using.
  #
  # At least in Nomad version v1.4.3, the CLI command to submit new jobs
  # (https://developer.hashicorp.com/nomad/docs/commands/job/run) says:
  # "Job files must conform to the job specification format." With this link:
  # https://developer.hashicorp.com/nomad/docs/job-specification. This is the
  # HCL format that is heavily specified in the docs. Nice but not compatible
  # with Nix, can't easily be used here.
  #
  # Hopefully note that it starts saying "Nomad HCL is parsed in the command
  # line and sent to Nomad in JSON format via the HTTP API." and here you can
  # see the API docs I found with "JSON Job Specification" in its title:
  # https://developer.hashicorp.com/nomad/api-docs/json-jobs
  # This is the format `nomad job run` expects when using the `-json` argument
  # but probably incomplete/outdated because when I tried to follow it I got
  # errors.
  #
  # I finally found this explanation in the HCL overview page:
  # https://developer.hashicorp.com/nomad/docs/job-specification/hcl2
  # "Since HCL is a superset of JSON, `nomad job run example.json` will attempt
  # to parse a JSON job using the HCL parser. However, the JSON format accepted
  # by the HCL parser is not the same as the API's JSON format. The HCL parser's
  # JSON format is unspecified, so the API format is preferred. You can use the
  # API format with the -json command line flag."
  #
  # So, if you don't provide the `-json` argument it expects HCL or its JSON
  # representation: https://github.com/hashicorp/hcl/blob/main/json/spec.md

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
    # This is also set with `NOMAD_NAMESPACE="perf"` when using Nomad's cli.
    # "When using commands that operate on objects that are namespaced, the
    # namespace can be specified either with the flag -namespace or read from
    # the NOMAD_NAMESPACE environment variable."
    # https://developer.hashicorp.com/nomad/tutorials/manage-clusters/namespaces
    namespace = "perf"; # Default to "perf" to avoid errors were possible.

    # The region in which to execute the job.
    region = "global"; # SRE: They are actually using global.

    #  A list of datacenters in the region which are eligible for task
    # placement. This must be provided, and does not have a default.
    # What we currently have available:
    # - Cardano World cluster: "eu-central-1", "us-east-2"
    # - Dedicated P&T cluster: "eu-central-1", "us-east-1", and "ap-southeast-2"
    datacenters = [ "ap-southeast-2" "eu-central-1" "us-east-1" "us-east-2" ];

    # Specifies user-defined constraints on the task. This can be provided
    # multiple times to define additional constraints.
    # Cloud runs set the distinct hosts constraint here but local runs can't
    # because we are only starting one Nomad client.
    constraint = null; # Values are appended inside the workbench (bash).

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
      # Only top level "KEY=STRING" are allowed, no child objects/attributes!
      WORKBENCH_STATEDIR = stateDir;
      TASK_DRIVER = if execTaskDriver then "exec" else "podman";
      SUPERVISORD_LOGLEVEL = task_supervisord_loglevel;
      ONE_TRACER_PER_NODE = oneTracerPerNode;
    };

    # A group defines a series of tasks that should be co-located on the same
    # client (host). All tasks within a group will be placed on the same host.
    # https://developer.hashicorp.com/nomad/docs/job-specification/group
    group = let
      # For each node-specs.json object
      valueF = (taskName: nodeSpec: servicePortName: portNum: (groupDefaults // {

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

        # Prevent allocations from being restarted:
        ###########################################
        # Nomad Clients periodically heartbeat to Nomad Servers to confirm they
        # are operating as expected. By default, Nomad Clients which do not
        # heartbeat in the specified amount of time are considered down and
        # their allocations are marked as lost (or disconnected if
        # "max_client_disconnect" is set) and rescheduled.
        # This means that if not properly configured allocations running on a
        # client that fails to heartbeat will be marked "lost" and when the
        # client reconnects, its allocations, which may still be healthy,
        # restarted because they have been marked "lost"!!!
        # See:
        # - https://developer.hashicorp.com/nomad/docs/configuration/server#client-heartbeats
        # - https://developer.hashicorp.com/nomad/docs/job-specification/group#stop-after-client-disconnect
        # - https://developer.hashicorp.com/nomad/docs/job-specification/group#max-client-disconnect
        # We want these allocations to reconnect without a restart.
        ### Nomad 1.6.X solution:
        ### Specifies a duration during which a Nomad client will attempt to
        ### reconnect allocations after it fails to heartbeat in the
        ### "heartbeat_grace" window. See the example code below for more
        ### details. This setting cannot be used with
        ### "stop_after_client_disconnect".
        ### When "max_client_disconnect" is specified, the Nomad server will
        ### mark clients that fail to heartbeat as "disconnected" rather than
        ### "down", and will mark allocations on a disconnected client as
        ### "unknown" rather than "lost". These allocations may continue to run
        ### on the disconnected client. Replacement allocations will be
        ### scheduled according to the allocations' reschedule policy until the
        ### disconnected client reconnects. Once a disconnected client
        ### reconnects, Nomad will compare the "unknown" allocations with their
        ### replacements and keep the one with the best node score. If the
        ### "max_client_disconnect" duration expires before the client
        ### reconnects, the allocations will be marked "lost". Clients that
        ### contain "unknown" allocations will transition to "disconnected"
        ### rather than "down" until the last "max_client_disconnect" duration
        ### has expired.
        ### https://developer.hashicorp.com/nomad/docs/v1.6.x/job-specification/group#max-client-disconnect
        max_client_disconnect = "999h";
        ### Nomad 1.7.X solution:
        ### (TODO blocker issue https://github.com/hashicorp/nomad/issues/19506)
        ### Defines the reschedule behaviour of an allocation when the node it
        ### is running on misses heartbeats. When enabled, if the node it is
        ### running on becomes disconnected or goes down, this allocations won't
        ### be rescheduled and will show up as unknown until the node comes back
        ### up or it is manually restarted.
        ### This behaviour will only modify the reschedule process on the
        ### server. To modify the allocation behaviour on the client, see
        ### "stop_after_client_disconnect" below.
        ### The unknown allocation has to be manually stopped to run it again.
        ### Setting `max_client_disconnect` and
        ### `prevent_reschedule_on_lost = true` at the same time requires that
        ### rescheduling is disabled entirely (what is done above in the
        ### reschedule stanza).
        # prevent_reschedule_on_lost = true;
        ### Specifies a duration after which a Nomad client will stop
        ### allocations, if it cannot communicate with the servers. By default,
        ### a client will not stop an allocation until explicitly told to by a
        ### server. A client that fails to heartbeat to a server within the
        ### "heartbeat_grace" window and any allocations running on it will be
        ### marked "lost" and Nomad will schedule replacement allocations. The
        ### replaced allocations will normally continue to run on the
        ### non-responsive client. But you may want them to stop instead — for
        ###  example, allocations requiring exclusive access to an external
        ### resource. When specified, the Nomad client will stop them after this
        ### duration. The Nomad client process must be running for this to
        ### occur. This setting cannot be used with "max_client_disconnect".
        # stop_after_client_disconnect = "999h";

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

        # The affinity block allows operators to express placement preference
        # for a set of nodes. Affinities may be expressed on attributes or
        # client metadata. Additionally affinities may be specified at the
        # job, group, or task levels for ultimate flexibility.
        affinity =
          let region = nodeSpec.region;
          in if region == null || region == "loopback"
            then null
            else
              { attribute = "\${node.datacenter}";
                value     = region;
              }
        ;

        # This can be provided multiple times to define additional constraints.
        # See the Nomad constraint reference for more details.
        # https://developer.hashicorp.com/nomad/docs/job-specification/constraint
        constraint = {
          attribute = "\${node.class}";
          operator = "=";
          # Cloud jobs can run in the dedicated P&T Nomad cluster on AWS or in
          # Cardano World Nomad cluster's "qa" class nodes.
          # This default is just a precaution, like the top level namespace,
          # because "qa" Class nodes usage must be limited to short test and
          # "infra" Class nodes, that are used for HA jobs, must be avoided
          # entirely.
          value = "perf";
        };

        # The network stanza specifies the networking requirements for the task
        # group, including the network mode and port allocations.
        # When scheduling jobs in Nomad they are provisioned across your fleet
        # of machines along with other jobs and services. Because you don't know
        # in advance what host your job will be provisioned on, Nomad will
        # provide your tasks with network configuration when they start up.
        # https://developer.hashicorp.com/nomad/docs/job-specification/network
        network = {
          # Mode of the network. This option is only supported on Linux clients.
          # All other operating systems use the host networking mode.
          # The following modes are available:
          # - none:       Task group will have an isolated network without any
          #               network interfaces.
          # - bridge:     Task group will have an isolated network namespace
          #               with an interface that is bridged with the host. Note
          #               that bridge networking is only currently supported for
          #               the docker, exec, raw_exec, and java task drivers.
          # - host:       Each task will join the host network namespace and a
          #               shared network namespace is not created. This matches
          #               the current behavior in Nomad 0.9.
          # - cni/<name>: Task group will have an isolated network namespace
          #               with the network configured by CNI.
          # Actually using the interface specified on Nomad Client startup that
          # for local runs it's forced to "lo" and whatever is automatically
          # fingerprinted or provided for cloud runs.
          # TODO: Use "bridge" mode for podman, this will allow to run isolated
          # local cluster with no addresses or ports clashing.
          mode = "host";
          # Specifies a TCP/UDP port allocation and can be used to specify both
          # dynamic ports and reserved ports.
          # https://developer.hashicorp.com/nomad/docs/job-specification/network#port-parameters
          port = lib.listToAttrs [
            {
              # The label assigned to the port is used to identify the port
              # in service discovery, and used in the name of the
              # environment variable that indicates which port your
              # application should bind to (envar only available for the
              # ports of current Tasks, not to resolve all port names).
              name = servicePortName; # Cannot be used for envars ("-" to "_").
              value =
                if portNum != null && portNum != 0
                then
                  (
                    # Dynamic ports vs Static ports as seen by Nomad:
                    # Most services run in your cluster should use dynamic
                    # ports. This means that the port will be allocated
                    # dynamically by the scheduler, and your service will have
                    # to read an environment variable to know which port to bind
                    # to at startup.
                    # https://developer.hashicorp.com/nomad/docs/job-specification/network#dynamic-ports
                    # Static ports bind your job to a specific port on the host
                    # they are placed on. Since multiple services cannot share
                    # a port, the port must be open in order to place your task.
                    # https://developer.hashicorp.com/nomad/docs/job-specification/network#static-ports
                    # Some drivers (such as Docker and QEMU) allow you to map
                    # ports. A mapped port means that your application can
                    # listen on a fixed port (it does not need to read the
                    # environment variable) and the dynamic port will be mapped
                    # to the port in your container or virtual machine.
                    # https://developer.hashicorp.com/nomad/docs/job-specification/network#mapped-ports
                    {
                      # Specifies the static TCP/UDP port to allocate. If
                      # omitted, a dynamic port is chosen. We do not recommend
                      # using static ports, except for system or specialized
                      # jobs like load balancers.
                      static = ''${toString portNum}'';

                      # TODO: When switching the network mode to "bridge" for
                      # podman use "Mapped Ports" to be able to run isolated
                      # local cluster with no addresses or ports clashing.
                      # Applicable when using "bridge" mode to configure port
                      # to map to inside the task's network namespace. Omitting
                      # this field or setting it to -1 sets the mapped port
                      # equal to the dynamic port allocated by the scheduler.
                      # The NOMAD_PORT_<label> environment variable will contain
                      # the to value.
                      # to = ''${toString portNum}'';
                      # The "podman" driver accepts "Mapped Ports", but not the
                      # "exec" driver
                      # https://developer.hashicorp.com/nomad/docs/job-specification/network#mapped-ports
                      # https://developer.hashicorp.com/nomad/docs/job-specification/network#bridge-mode
                    }
                  )
                else
                  # Only reserve the name!
                  {}
              ;
            }
          ];
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
          env = {
            # The "old Nomad" setup somehow included the CA certs inside the
            # task namespace but apparently not "new Nomad". We are adding the
            # necessary Nix package ("cacert") and making sure `wget` finds it.
            #
            # "All nix-docker images set environment variables which point to
            # cacert.", see:
            # - https://github.com/NixOS/nixpkgs/issues/48211#issuecomment-434102565
            # - https://github.com/LnL7/nix-docker/blob/8dcfb3aff1f87cdafeecb0d27964b27c3fb8b1d2/default.nix#L70-L71
            #
            # Error when using `wget` to deploy the genesis tar file was:
            # ERROR: cannot verify iog-cardano-perf.s3.eu-central-1.amazonaws.com's certificate, issued by 'CN=Amazon RSA 2048 M01,O=Amazon,C=US':
            # Unable to locally verify the issuer's authority.
            # To connect to iog-cardano-perf.s3.eu-central-1.amazonaws.com insecurely, use `--no-check-certificate'.
            SSL_CERT_FILE = "${containerSpecs.containerPkgs.cacert.nix-store-path}/etc/ssl/certs/ca-bundle.crt";
          };

          # Sensible defaults to run cloud version of "default", "ci-test" and
          # "ci-bench" in Cardano World Nomad cluster's "qa" class nodes.
          # For benchmarking the dedicated P&T Nomad cluster on AWS is used and
          # this value should be updated accordingly.
          resources = {
            # Task can only ask for 'cpu' or 'cores' resource but not both.
            cores = 2;       # cpu = 512;
            memory = 1024*4; # memory_max = 32768;
          };

          # The service block instructs Nomad to register a service with the
          # specified provider; Nomad or Consul (we are using Nomad).
          # https://developer.hashicorp.com/nomad/docs/job-specification/service
          #
          # This services are used to dynamically configure the IP and ports of
          # nodes using the "template" stanza below.
          service = {
            # Specifies the service registration provider to use for service
            # registrations. Valid options are either consul or nomad. All
            # services within a single task group must utilise the same provider
            # value.
            # We don't use Consul to avoid having one extra dependency / thing
            # to configure and monitor during local runs while sharing as much
            # code as possible with cloud runs.
            provider = "nomad";
            # Specifies the name this service will be advertised as in Consul.
            # If not supplied, this will default to the name of the job, task
            # group, and task concatenated together with a dash, like
            # "docs-example-server". Each service must have a unique name within
            # the cluster. Names must adhere to RFC-1123 §2.1 and are limited to
            # alphanumeric and hyphen characters (i.e. [a-z0-9\-]), and be less
            # than 64 characters in length.
            name = servicePortName; # Cannot be used for envars ("-" to "_").
            # Specifies a custom address to advertise in Consul or Nomad service
            # registration. If set, address_mode must be in auto mode. Useful
            # with interpolation - for example to advertise the public IP
            # address of an AWS EC2 instance set this to
            # ${attr.unique.platform.aws.public-ipv4}.
            address =
              # When using the dedicated P&T Nomad cluster on AWS we use public
              # IPs/routing, all the other cloud runs are behind a VPC/firewall.
              # Local runs just use 12.0.0.1.
              if lib.strings.hasInfix "-nomadperf" profileData.profileName
              then "\${attr.unique.platform.aws.public-ipv4}"
              else ""
            ;
            # Specifies the port to advertise for this service. The value of
            # port depends on which address_mode is being used:
            # - alloc:  Advertise the mapped to value of the labeled port and the
            #           allocation address. If a to value is not set, the port
            #           falls back to using the allocated host port. The port
            #           field may be a numeric port or a port label specified in
            #           the same group's network block.
            # - driver: Advertise the port determined by the driver (e.g.
            #           Docker). The port may be a numeric port or a port label
            #           specified in the driver's ports field.
            # - host:   Advertise the host port for this service. port must
            #           match a port label specified in the network block.
            # Here we use "network"->"port"->"name" specified in the Group.
            port = servicePortName; # Cannot be used for envars ("-" to "_").
            # Checks of type "script" need "consul" instead of "nomad" as
            # service provider, so as healthcheck we are using a supervisord
            # "program".
            # The initial idea was to use Nomad's builtin `service -> check`
            # functionality but it won't be 100% compatible/interchangeable with
            # local runs using the `supervisord` backend and critical business
            # logic, like when to start it/how to control it, will be delegated
            # to Nomad. Plus, Nomad needs `consul` to configure "check"s and
            # that means an extra dependency for local runs.
            # https://developer.hashicorp.com/nomad/docs/job-specification/check
            check = null;
          };

          # Specifies the set of templates to render for the task. Templates can
          # be used to inject both static and dynamic configuration with data
          # populated from environment variables, Consul and Vault.
          #
          # We are using the template machinery to populate IP and ports.
          # See "Dynamic Configuration":
          # Nomad's job specification includes a template block that utilizes a
          # Consul ecosystem tool called Consul Template. This mechanism creates
          # a convenient way to ship configuration files that are populated from
          # environment variables, Consul data, Vault secrets, or just general
          # configurations within a Nomad task.
          # For more information on Nomad's template block and how it leverages
          # Consul Template, please see the template job specification
          # documentation.
          # - Template block: https://developer.hashicorp.com/nomad/docs/job-specification/template
          # - Consul template: https://github.com/hashicorp/consul-template
          # https://developer.hashicorp.com/nomad/docs/integrations/consul-integration#dynamic-configuration
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
                WORKBENCH_STATEDIR="{{ env "NOMAD_META_WORKBENCH_STATEDIR" }}"
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
            ## Make the profile.json file available (mainly for healthchecks)
            {
              env = false;
              destination = "local/${stateDir}/profile.json";
              data = escapeTemplate (__readFile
                profileData.JSON.outPath);
              change_mode = "noop";
              error_on_missing_key = true;
            }
            ## Make the node-specs.json file available (mainly for healthchecks)
            {
              env = false;
              destination = "local/${stateDir}/node-specs.json";
              data = escapeTemplate (__readFile
                profileData.node-specs.JSON.outPath);
              change_mode = "noop";
              error_on_missing_key = true;
            }
            # entrypoint
            {
              env = false;
              destination = "local/entrypoint.sh";
              data = entrypoint;
              change_mode = "noop";
              error_on_missing_key = true;
            }
            # Dynamically generated addresses for debugging purposes
            {
              env = false;
              destination = "local/networking.json";
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
              destination = "local/${task_supervisord_conf}";
              data = escapeTemplate (__readFile (
                let supervisorConf = import ./supervisor-conf.nix
                  { inherit pkgs lib stateDir;
                    # Include only this taks' node
                    nodeSpecs = if taskName == "tracer"
                      then {}
                      else {"${nodeSpec.name}"=nodeSpec;}
                    ;
                    # Only for the node that will run the generator
                    withGenerator = taskName == generatorTaskName;
                    # Only for the tracer task or also nodes if oneTracerPerNode
                    withTracer = oneTracerPerNode || taskName == "tracer";
                    inherit withSsh;
                    # ''{{ env "NOMAD_TASK_DIR" }}/supervisor.sock''
                    inherit unixHttpServerPort;
                  };
                in supervisorConf.INI
              ));
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
              destination = "local/${stateDir}/tracer/start.sh";
              data = escapeTemplate
                profileData.tracer-service.start.value;
              change_mode = "noop";
              error_on_missing_key = true;
              perms = "744"; # Only for every "start.sh" script. Default: "644"
            }
            ## Tracer configuration file.
            {
              env = false;
              destination = "local/${stateDir}/tracer/config.json";
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
          # Node
          (lib.optionals (taskName != "tracer") [
            ## Node start.sh script.
            {
              env = false;
              destination = "local/${stateDir}/${nodeSpec.name}/start.sh";
              data = escapeTemplate (
                let scriptValue = profileData.node-services."${nodeSpec.name}".start.value;
                in if execTaskDriver
                  then (startScriptToGoTemplate
                    nodeSpec                         # nodeSpec
                    servicePortName                  # servicePortName
                    scriptValue                      # startScript
                  )
                  else scriptValue
              );
              change_mode = "noop";
              error_on_missing_key = true;
              perms = "744"; # Only for every "start.sh" script. Default: "644"
            }
            ## Node configuration file.
            {
              env = false;
              destination = "local/${stateDir}/${nodeSpec.name}/config.json";
              data = escapeTemplate (lib.generators.toJSON {}
                profileData.node-services."${nodeSpec.name}".config.value);
              change_mode = "noop";
              error_on_missing_key = true;
            }
            ## Node topology file.
            {
              env = false;
              destination = "local/${stateDir}/${nodeSpec.name}/topology.json";
              data = escapeTemplate (
                if execTaskDriver
                then
                  # Recreate the "topology.json" with IPs and ports that are
                  # nomad template variables.
                  (topologyToGoTemplate
                    # Fetch this node's entry in the profile's "topology.json".
                    (if nodeSpec.name != "explorer"
                     then
                      # Non explorer nodes are under "coreNodes"
                      builtins.head # Must exist!
                        (builtins.filter
                          (coreNode: coreNode.name == nodeSpec.name)
                          profileData.topology.value.coreNodes
                        )
                     else
                      # The explorer node is under "relayNodes"
                      builtins.head # Must exist!
                        (builtins.filter
                          (coreNode: coreNode.name == nodeSpec.name)
                          profileData.topology.value.relayNodes
                        )
                    )
                    # The P2P flag.
                    (if profileData.value.node ? verbatim && profileData.value.node.verbatim ? EnableP2P
                     then profileData.value.node.verbatim.EnableP2P
                     else false
                    )
                  )
                # Do nothing with the topology files.
                else (__readFile profileData.node-services."${nodeSpec.name}".topology.JSON )
              );
              change_mode = "noop";
              error_on_missing_key = true;
            }
          ])
          ++
          # Generator
          (lib.optionals (taskName == generatorTaskName) [
            ## Generator start.sh script.
            {
              env = false;
              destination = "local/${stateDir}/generator/start.sh";
              data = escapeTemplate
                profileData.generator-service.start.value;
              change_mode = "noop";
              error_on_missing_key = true;
              perms = "744"; # Only for every "start.sh" script. Default: "644"
            }
            ## Generator configuration file.
            {
              env = false;
              destination = "local/${stateDir}/generator/run-script.json";
              data = escapeTemplate (
                let runScript = profileData.generator-service.config;
                in if execTaskDriver
                  # Recreate the "run-script.json" with IPs and ports that are
                  # nomad template variables.
                  then (runScriptToGoTemplate
                    runScript.value
                    # Just the node names.
                    (lib.attrsets.mapAttrsToList
                      (nodeSpecNodeName: nodeSpecNode: nodeSpecNodeName)
                      # All the producer nodes. How the workbench creates it.
                      (lib.attrsets.filterAttrs
                        (nodeSpecNodeName: nodeSpecNode: nodeSpecNode.isProducer)
                        profileData.node-specs.value
                      )
                    )
                  )
                  # Do nothing with the topology files.
                  else (__readFile runScript.JSON)
              );
              change_mode = "noop";
              error_on_missing_key = true;
            }
            ## Generator Plutus redeemer.
            {
              env = false;
              destination = "local/${stateDir}/generator/plutus-redeemer.json";
              data = escapeTemplate
                (__readFile profileData.generator-service.plutus-redeemer.JSON)
              ;
              change_mode = "noop";
              error_on_missing_key = true;
            }
            ## Generator Plutus datum.
            {
              env = false;
              destination = "local/${stateDir}/generator/plutus-datum.json";
              data = escapeTemplate
                (__readFile profileData.generator-service.plutus-datum.JSON)
              ;
              change_mode = "noop";
              error_on_missing_key = true;
            }
          ])
          ++
          # healthcheck
          [
            ## healthcheck start.sh script.
            {
              env = false;
              destination = "local/${stateDir}/healthcheck/start.sh";
              data = escapeTemplate
                profileData.healthcheck-service.start.value;
              change_mode = "noop";
              error_on_missing_key = true;
              perms = "744"; # Only for every "start.sh" script. Default: "644"
            }
          ]
          ++
          # ssh
          (lib.optionals withSsh (
            let
              ssh-service = import
                  ../service/ssh.nix
                  {
                    inherit pkgs;
                    bashInteractive = containerSpecs.containerPkgs.bashInteractive.nix-store-path;
                    coreutils = containerSpecs.containerPkgs.coreutils.nix-store-path;
                    openssh_hacks = containerSpecs.containerPkgs.openssh_hacks.nix-store-path;
                  }
              ;
            in [
              ## ssh start.sh script.
              {
                env = false;
                destination = "local/${stateDir}/ssh/start.sh";
                data = escapeTemplate ssh-service.start.value;
                change_mode = "noop";
                error_on_missing_key = true;
                perms = "744"; # Only for every "start.sh" script. Default: "644"
              }
              ## ssh config file.
              {
                env = false;
                destination = "local/${stateDir}/ssh/sshd_config";
                data = escapeTemplate ssh-service.config.value;
                change_mode = "noop";
                error_on_missing_key = true;
                perms = "744"; # Only for every "start.sh" script. Default: "644"
              }
              # The deployer script must add the templates for the private keys:
              # - local/${stateDir}/ssh/sshd.id_ed25519
              # - local/${stateDir}/ssh/nobody.id_ed25519.pub
            ]
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

                args = ["local/entrypoint.sh"];

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

                args = ["local/entrypoint.sh"];

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
                # These name cannot be used for envars, "-" replaced with "_".
                ports = [ servicePortName ];

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
                working_dir = "local/";

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
              # TODO: Which region?
              {region=null;}                         # node-spec
              # These name cannot be used for envars, "-" is replaced with "_".
              "perf-tracer"                          # servicePortName
              0                                      # portNum
            ;
          }
        ]
        ++
        (lib.mapAttrsToList
          (_: nodeSpec: {
            name = nodeSpec.name;
            value = valueF
              nodeSpec.name                          # taskName
              nodeSpec                               # node-spec
              # These name cannot be used for envars, "-" is replaced with "_".
              (nodeSpecToServicePortName nodeSpec)   # servicePortName
              nodeSpec.port                          # portNum
            ;
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

  # A single node's service and network port use the same name, they are
  # "perf-node-0" .. "perf-node-53".
  # These names cannot be used for envars, envars translate "-" to "_", for
  # example "NOMAD_HOST_PORT_perf_node_0".
  nodeSpecToServicePortName = nodeSpec: "${"perf-node-" + (toString nodeSpec.i)}";
  # WARNING: Node names should be of the form "node-#".
  nodeNameToServicePortName = nodeName: "${"perf-" + (toString nodeName)}";

  # Replaces the addresses and ports occurrences with Nomad templates variables.
  startScriptToGoTemplate = nodeSpec: servicePortName: startScript:
    builtins.replaceStrings
      [
        # Address string from
        ''--host-addr 127.0.0.1''
        # Port string from
        ''--port ${toString nodeSpec.port}''
      ]
      # On cloud deployments to SRE-managed / dedicated P&T Nomad cluster, that
      # uses AWS, the hosts at Linux level may not be aware of the EIP public
      # address they have so we can't bind to the public IP (that we can resolve
      # to using templates). The only options available are to bind to the
      # "all-weather" 0.0.0.0 or use the private IP provided by AWS. We use the
      # latter in case the Nomad Client was not started with the correct
      # `-network-interface XX` parameter.
      [
        # Address string to
        (
          if lib.strings.hasInfix "-nomadperf" profileData.profileName
          then ''--host-addr {{ env "attr.unique.platform.aws.local-ipv4" }}''
          else ''--host-addr 0.0.0.0''
        )
        # Alternatives (may not work):
        #''--host-addr {{ env "NOMAD_HOST_IP_${servicePortName}" }}''
        #''--host-addr {{ env "NOMAD_IP_${servicePortName}" }}''
        #''--host-addr {{range nomadService "${servicePortName}"}}{{.Address}}{{end}}''

        # Port string to
        ''--port {{range nomadService "${servicePortName}"}}{{.Port}}{{end}}''
        # Alternatives (may not work):
        #''--port {{ env "NOMAD_PORT_${servicePortName}" }}''
        #''--port {{ env "NOMAD_HOST_PORT_${servicePortName}" }}''
        #''--port {{ env "NOMAD_ALLOC_PORT_${name}" }}''
      ]
      startScript
  ;

  # Convert from a Node's "topology.json" with all addresses being "127.0.0.01"
  # to one with all addresses being a placeholder like "{{NOMAD_IP_node-X}}".
  #
  # The workbench creates JSON valid topology files with "127.0.0.1" and ports
  # `basePort + nodeId` (Usually 30000 + Node number). The problem this function
  # resolves is that a topology file with Nomad variables won't be valid JSON,
  # a port number will be "{{ SOMETHING }}", so we handle them as strings and
  # replace the values with Nomad template variables.
  #
  # Input is this node's entry in workbench's "topology.json" and the P2P flag.
  topologyToGoTemplate = nodeTopology: p2p:
    let
      nodesReferencesStr =
          "["
        + (builtins.concatStringsSep
            ","
            (nodeNamesToGoTemplateList nodeTopology.producers p2p)
          )
        + "]"
      ;
      valency = builtins.length nodeTopology.producers;
    in
      if p2p
      then
        ''
        { "localRoots": [
            { "accessPoints": ${nodesReferencesStr}
            , "advertise": false
            , "valency": ${builtins.toString valency}
            }
          ]
        , "publicRoots": []
        , "useLedgerAfterSlot": -1
        }
        ''
      else
        ''
        { "Producers": ${nodesReferencesStr}
        }
        ''
  ;

  # Convert from generator's "run-script.json" with all addresses being
  # "127.0.0.01" to one with all addresses being a placeholder like
  # "{{NOMAD_IP_node-X}}".
  #
  # The workbench creates JSON valid script files with "127.0.0.1" and ports
  # `basePort + nodeId` (Usually 30000 + Node number). The problem this function
  # resolves is that a topology file with Nomad variables won't be valid JSON,
  # a port number will be "{{ SOMETHING }}", so we handle them as strings and
  # replace the values with Nomad template variable
  runScriptToGoTemplate =
    runScript: producerNodes: builtins.replaceStrings
      ["\"targetNodes\":null"]
      [''
        "targetNodes": [
          ${builtins.concatStringsSep
            ","
            # The tx-generator always uses the non-p2p / peers format.
            # targetNodes are like:
            # "targetNodes": [
            #   {
            #     "addr": "127.0.0.1",
            #     "port": 30000
            #   }
            # ]
            (nodeNamesToGoTemplateList producerNodes false)
          }
        ]
      '']
      (lib.generators.toJSON {} (runScript // {targetNodes=null;}))
  ;

  # Convert from a list of node names ("node-0", "node-1", etc) to a list of
  # strings having non-valid JSON syntax to use in the topology files of nodes.
  nodeNamesToGoTemplateList = nodeNames: p2p:
    let
      # From node name to node reference object as expected in "topology.json".
      nodeReferenceToStr = nodeName:
        let
          # What Nomad templates will run for interpolation of address and port.
          # Here we must use the definitions in the "service" Nomad Job stanza
          # to resolve dynamically, once deployed, the other nodes' IPs and
          # PORTs. Nomad stanza envars of a Nomad Client will only contain
          # information about the node it has deployed, the node the "template"
          # will run.
          addr = ''{{range nomadService "${(nodeNameToServicePortName nodeName)}"}}{{.Address}}{{end}}'';
          port = ''{{range nomadService "${(nodeNameToServicePortName nodeName)}"}}{{.Port}}{{end}}'';
        in
          # Builds a string with a node's JSON object containing the Nomad
          # template variables to interpolate address and port.
          if p2p
          then
            # The format of a Node inside "localRoots:[{accessPoints:[ NODE ]}]"
            ''
            {
              "address": "${addr}"
            , "port": ${port}
            }
            ''
          else
            # The format of a Node inside "Producers:[ NODE ]"
            ''
            {
              "addr": "${addr}"
            , "port": ${port}
            , "valency": 1
            }
            ''
      ;
    in
      builtins.map
        (nodeName: nodeReferenceToStr nodeName)
        nodeNames
  ;

in lib.generators.toJSON {} clusterJob
