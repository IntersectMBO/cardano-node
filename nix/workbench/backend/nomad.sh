usage_nomad() {
     usage "nomad" "Backend:  manages a local cluster using 'nomad' (and 'podman')" <<EOF

    Please see documentation for 'wb backend' for the supported commands.

    Nomad-specific:

    service-start      RUN-DIR SERVICE
    service-stop       RUN-DIR SERVICE
    is-service-running RUN-DIR SERVICE

EOF
}

backend_nomad() {
op=${1:?$(usage_nomad)}; shift

case "$op" in

    name )
        echo 'nomad'
        ;;

    is-running )
        # Hack: Look for node-0's default port!
        test "$(sleep 0.5s; netstat -pltn 2>/dev/null | grep ':30000 ' | wc -l)" != "0"
        ;;

    setenv-defaults )
        local usage="USAGE: wb nomad $op PROFILE-DIR"
        local profile_dir=${1:?$usage}

        # Look up `supervisord` config file produced by Nix (run profile).
        setenvjqstr 'supervisord_conf' "$profile_dir"/supervisor.conf
        # The `--serverurl` argument is needed in every call to `nomad exec`.
        # The problem is that if we use "127.0.0.1:9001" as parameter (without
        # the "http" part) the container returns:
        # error: <class 'ValueError'>, Unknown protocol for serverurl 127.0.0.1:9001: file: /nix/store/izqhlj5i1x9ldyn43d02kcy4mafmj3ci-python3.9-supervisor-4.2.4/lib/python3.9/site-packages/supervisor/xmlrpc.py line: 508
        # Without using the `--serverurl` parameter at all (using INI config
        # file's [inet_http_server] port stanza) also without "http://":
        # error: <class 'socket.gaierror'>, [Errno -2] Name or service not known: file: /nix/store/hb1lzaisgx2m9n29hqhh6yp6hasplq1v-python3-3.9.10/lib/python3.9/socket.py line: 954
        # If I add the "http" part to the INI file, when starting `supervisord`
        # inside the container I get (from journald):
        # Nov 02 11:44:36 hostname cluster-18f3852f-e067-6394-8159-66a7b8da2ecc[1088457]: Error: Cannot open an HTTP server: socket.error reported -2
        # Nov 02 11:44:36 hostname cluster-18f3852f-e067-6394-8159-66a7b8da2ecc[1088457]: For help, use /nix/store/izqhlj5i1x9ldyn43d02kcy4mafmj3ci-python3.9-supervisor-4.2.4/bin/supervisord -h
        setenvjqstr 'supervisord_url' "http://127.0.0.1:9001"
        # Look up `cluster` OCI image's name and tag (also Nix profile).
        setenvjqstr 'oci_image_name' ${WB_OCI_IMAGE_NAME:-$(cat "$profile_dir/clusterImageName")}
        setenvjqstr 'oci_image_tag'  ${WB_OCI_IMAGE_TAG:-$(cat  "$profile_dir/clusterImageTag")}
        # Script that creates the OCI image from nix2container layered output.
        setenvjqstr 'oci_image_skopeo_script' "$profile_dir/clusterImageCopyToPodman"
        # Set cluster's podman container defaults.
        # The workbench is expecting an specific hierarchy of folders and files.
        setenvjqstr 'container_workdir' "/tmp/cluster/"
        setenvjqstr 'container_mountpoint' "/tmp/cluster/run/current"
        # The `supervisord` binary is installed inside the container but not
        # added to $PATH (resides in /nix/store), so a desired location is
        # passed to the container as an environment variable to create a symlink
        # to it.
        setenvjqstr 'container_supervisor_nix' "/tmp/cluster/run/current/supervisor/nix-store"
        # The container need to know where `supervisord` config file is located
        # so it can be started. This is passed as an environment variable.
        setenvjqstr 'container_supervisord_conf' "/tmp/cluster/run/current/supervisor/supervisord.conf"
        ;;

    # Man pages for Podman configuration files:
    # https://man.archlinux.org/man/community/podman/podman.1.en
    # https://man.archlinux.org/man/containers.conf.5
    # https://man.archlinux.org/man/containers-storage.conf.5
    # https://man.archlinux.org/man/containers-policy.json.5

    allocate-run )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_docker;;
               * ) break;; esac; shift; done

        # The `genesis/utxo-keys` directory is used as a volume for the
        # `generator` service but it's not always present/created.
        if ! test -e "$dir"/genesis/utxo-keys
        then
            mkdir -p "$dir"/genesis/utxo-keys
        else
          # HACK: UGLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          ############### FIXME: Fix it in `genesis.sh` ###############
          mv "$dir"/genesis/utxo-keys "$dir"/genesis/utxo-keys.bak
          # The `genesis/utxo-keys` directory is used as a volume for the
          # `generator` service but it's not always present/created.
          mkdir -p "$dir"/genesis/utxo-keys
          cp -r "$dir"/genesis/utxo-keys.bak/* "$dir"/genesis/utxo-keys/
        fi

        # Populate the files needed by the `supervisord` instance running inside
        # the container.
        local supervisord_conf=$(envjqr 'supervisord_conf')
        mkdir -p                 "$dir"/supervisor
        # If $dir is being mounted inside the container the file must be copied
        # because if it references something outside the container's mounted
        # volume the container probably won't be able to access it.
        cp -f "$supervisord_conf" "$dir"/supervisor/supervisord.conf

        # Create the "cluster" OCI image.
        local oci_image_name=$(         envjqr 'oci_image_name')
        local oci_image_tag=$(          envjqr 'oci_image_tag')
        local oci_image_skopeo_script=$(envjqr 'oci_image_skopeo_script')
        msg "Creating OCI image ..."
        # Forced the `overlay` storage driver or podman won't see the image.
        # https://docs.podman.io/en/latest/markdown/podman.1.html#note-unsupported-file-systems-in-rootless-mode
        STORAGE_DRIVER=overlay "$oci_image_skopeo_script"
        # Check that `podman` can see the "cluster" OCI image.
        if ! podman image exists "${oci_image_name}:${oci_image_tag}"
        then
            fatal "OCI image ${oci_image_name}:${oci_image_tag} cannot be found by podman"
        else
            msg "OCI image named \"${oci_image_name}:${oci_image_tag}\" created"
        fi

        # Configure `nomad` and the `podman` plugin/task driver.
        nomad_create_folders_and_config "$dir"
        msg "Preparing podman API service for nomad driver \`nomad-driver-podman\` ..."
        nomad_start_podman_service "$dir"

        # Start `nomad` agent in "-dev-` mode`".
        msg "Starting nomad agent ..."
        # The Nomad agent is a long running process which runs on every machine
        # that is part of the Nomad cluster. The behavior of the agent depends
        # on if it is running in client or server mode. Clients are responsible
        # for running tasks, while servers are responsible for managing the
        # cluster.
        # -dev: Start the agent in development mode. This enables a
        # pre-configured dual-role agent (client + server) which is useful for
        # developing or testing Nomad. No other configuration is required to
        # start the agent in this mode, but you may pass an optional
        # comma-separated list of mode configurations
        nomad agent -config="$dir/nomad/config" -dev -log-level=INFO >> "$dir/nomad/stdout" 2>> "$dir/nomad/stderr" &
        echo "$!" > "$dir/nomad/nomad.pid"
        setenvjqstr 'nomad_pid' $(cat $dir/nomad/nomad.pid)
        msg "Nomad started with PID $(cat $dir/nomad/nomad.pid)"

        # Wait for nomad agent:
        msg "Waiting for the listening HTTP server ..."
        local i=0
        local patience=25
        until curl -Isf 127.0.0.1:4646 2>&1 | head --lines=1 | grep --quiet "HTTP/1.1"
        do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge $patience
            then echo
                progress "nomad agent" "$(red FATAL):  workbench:  nomad agent:  patience ran out after ${patience}s, 127.0.0.1:4646"
                backend_nomad stop-cluster "$dir"
                fatal "nomad agent startup did not succeed:  check logs"
            fi
            echo -ne "\b\b\b"
        done >&2

        # Create and start the nomad job.
        nomad_create_job_file "$dir"
        msg "Starting nomad job ..."
        # Upon successful job submission, this command will immediately enter
        # an interactive monitor. This is useful to watch Nomad's internals make
        # scheduling decisions and place the submitted work onto nodes. The
        # monitor will end once job placement is done. It is safe to exit the
        # monitor early using ctrl+c.
        # On successful job submission and scheduling, exit code 0 will be
        # returned. If there are job placement issues encountered (unsatisfiable
        # constraints, resource exhaustion, etc), then the exit code will be 2.
        # Any other errors, including client connection issues or internal
        # errors, are indicated by exit code 1.
        nomad job run -verbose "$dir/nomad/cluster-job.hcl"
        # Assuming that `nomad` placement is enough wait.
        local nomad_alloc_id=$(nomad job allocs -json cluster | jq -r '.[0].ID')
        setenvjqstr 'nomad_alloc_id' "$nomad_alloc_id"
        msg "Nomad job allocation ID is: $nomad_alloc_id"
        # Show `--status` of `supervisorctl` inside the container.
        local supervisord_url=$(envjqr 'supervisord_url')
        local container_supervisor_nix=$(  envjqr 'container_supervisor_nix')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        msg "Supervisor status inside container ..."
        # Print the command used for debugging purposes.
        msg "'nomad alloc exec \"$nomad_alloc_id\" \"$container_supervisor_nix\"/bin/supervisorctl --serverurl \"$supervisord_url\" --configuration \"$container_supervisord_conf\" status'"
        # Execute the actual command.
        nomad alloc exec "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$supervisord_url" --configuration "$container_supervisord_conf" status || true
        ;;

    describe-run )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}

        echo "  - Nomad job: $(realpath $dir)/nomad/cluster-job.hcl"
        ;;

    # Nomad-specific
    service-start )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        local nomad_alloc_id=$(envjqr 'nomad_alloc_id')
        local supervisord_url=$(envjqr 'supervisord_url')
        local container_supervisor_nix=$(envjqr 'container_supervisor_nix')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        nomad alloc exec "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$supervisord_url" --configuration "$container_supervisord_conf" start "$service"
        ;;

    # Nomad-specific
    service-stop )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        local nomad_alloc_id=$(envjqr 'nomad_alloc_id')
        local supervisord_url=$(envjqr 'supervisord_url')
        local container_supervisor_nix=$(envjqr 'container_supervisor_nix')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        nomad alloc exec "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$supervisord_url" --configuration "$container_supervisord_conf" stop "$service"
        ;;

    # Nomad-specific
    is-service-running )
        local usage="USAGE: wb nomad $op RUN-DIR DOCKER-SERVICE"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        local nomad_alloc_id=$(envjqr 'nomad_alloc_id')
        local supervisord_url=$(envjqr 'supervisord_url')
        local container_supervisor_nix=$(envjqr 'container_supervisor_nix')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        nomad alloc exec "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$supervisord_url" --configuration "$container_supervisord_conf" status "$service" > /dev/null && true
        ;;

    start-node )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        backend_nomad service-start "$dir" $node
        # Always wait for the node to be ready.
        backend_nomad wait-node "$dir" $node
        ;;

    stop-node )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        backend_nomad service-stop "$dir" $node
        ;;

    wait-node )
        local usage="USAGE: wb nomad $op RUN-DIR [NODE-NAME]"
        local dir=${1:?$usage}; shift
        local node=${1:-$(dirname $CARDANO_NODE_SOCKET_PATH | xargs basename)}; shift
        local socket=$(backend_nomad get-node-socket-path "$dir" $node)

        local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' $dir/profile.json) i=0
        echo -n "workbench:  nomad:  waiting ${patience}s for socket of $node: " >&2
        while test ! -S $socket
        do printf "%3d" $i; sleep 1
           i=$((i+1))
           if test $i -ge $patience
           then echo
                progress "nomad" "$(red FATAL):  workbench:  nomad:  patience ran out for $(white $node) after ${patience}s, socket $socket"
                backend_nomad stop-cluster "$dir"
                fatal "$node startup did not succeed:  check logs in $(dirname $socket)/stdout & stderr"
           fi
           echo -ne "\b\b\b"
        done >&2
        echo " $node up (${i}s)" >&2
        ;;

    start-nodes )
        local usage="USAGE: wb nomad $op RUN-DIR [HONOR_AUTOSTART=]"
        local dir=${1:?$usage}; shift
        local honor_autostart=${1:-}

        local nodes=($(jq_tolist keys "$dir"/node-specs.json))
        for node in ${nodes[*]}
        do
            if test -n "$honor_autostart"
            then
                if jqtest ".\"$node\".autostart" "$dir"/node-specs.json
                then
                    backend_nomad start-node "$dir" "$node"
                fi
            else
                backend_nomad start-node "$dir" "$node"
            fi
        done

        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$(backend_nomad get-node-socket-path "$dir" 'node-0')
        fi
        ;;

    start )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        if jqtest ".node.tracer" "$dir"/profile.json
        then
          backend_nomad service-start "$dir" tracer
          # Wait for tracer socket
          # If tracer fails here, the rest of the cluster is brought up without
          # any problems.
          local socket=$(jq -r '.network.contents' "$dir/tracer/config.json")
          local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' "$dir/profile.json") i=0
          echo -n "workbench:  nomad:  waiting ${patience}s for socket of tracer: " >&2
          while test ! -S "$dir/tracer/$socket"
          do printf "%3d" $i; sleep 1
             i=$((i+1))
             if test $i -ge $patience
             then echo
                  progress "nomad" "$(red FATAL):  workbench:  nomad:  patience ran out for $(white tracer) after ${patience}s, socket $socket"
                  backend_nomad stop-cluster "$dir"
                  fatal "$node startup did not succeed:  check logs in $(dirname $socket)/stdout & stderr"
             fi
             echo -ne "\b\b\b"
          done >&2
          echo " tracer up (${i}s)" >&2
        fi
        ;;

    get-node-socket-path )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}
        local node_name=${2:?$usage}

        echo -n $dir/$node_name/node.socket
        ;;

    start-generator )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_docker;;
               * ) break;; esac; shift; done

        backend_nomad service-start "$dir" generator
        ;;

    wait-node-stopped )
        local usage="USAGE: wb nomad $op RUN-DIR NODE"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        progress_ne "docker" "waiting until $node stops:  ....."
        local i=0
        while backend_nomad is-service-running "$dir" "$node"
        do
          echo -ne "\b\b\b\b\b"; printf "%5d" $i >&2; i=$((i+1))
          sleep 1
        done >&2
        echo -e "\b\b\b\b\bdone, after $(with_color white $i) seconds" >&2
        ;;

    wait-pools-stopped )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local i=0 pools=$(jq .composition.n_pool_hosts $dir/profile.json) start_time=$(date +%s)
        msg_ne "nomad:  waiting until all pool nodes are stopped: 000000"
        touch $dir/flag/cluster-termination

        for ((pool_ix=0; pool_ix < $pools; pool_ix++))
        do
          while backend_nomad is-service-running "$dir" "node-${pool_ix}" && test -f $dir/flag/cluster-termination
          do
            echo -ne "\b\b\b\b\b\b"; printf "%6d" $((i + 1)); i=$((i+1))
            sleep 1
          done
          echo -ne "\b\b\b\b\b\b"; echo -n "node-${pool_ix} 000000"
        done >&2
        echo -ne "\b\b\b\b\b\b"
        local elapsed=$(($(date +%s) - start_time))
        if test -f $dir/flag/cluster-termination
        then echo " All nodes exited -- after $(yellow $elapsed)s" >&2
        else echo " Termination requested -- after $(yellow $elapsed)s" >&2; fi
        ;;

    stop-cluster )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local nomad_alloc_id=$(envjqr 'nomad_alloc_id')
        local supervisord_url=$(envjqr 'supervisord_url')
        local container_supervisor_nix=$(envjqr 'container_supervisor_nix')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        nomad alloc exec "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$supervisord_url" --configuration "$container_supervisord_conf" stop all || true > /dev/null

        nomad job stop cluster
        local nomad_pid=$(envjqr 'nomad_pid')
        kill -SIGINT "$nomad_pid"
        # FIXME: $ podman WARN[0066] StopSignal SIGTERM failed to stop container cluster-fa7c89c7-43a2-07bb-cf63-c4848cf70466 in 5 seconds, resorting to SIGKILL
        ;;

    cleanup-cluster )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}; shift

        msg "nomad:  resetting cluster state in:  $dir"
        rm -f $dir/*/std{out,err} $dir/node-*/*.socket $dir/*/logs/* 2>/dev/null || true
        rm -fr $dir/node-*/state-cluster/
        # Clean nomad logs.
        rm -f $dir/nomad/nomad.log $dir/nomad/std{out,err}
        rm -rf $dir/nomad/data/*
        ;;

    * ) usage_docker;; esac
}

# Configure `nomad` and its `podman` plugin / task driver
# (Task Drivers are also called plugins because they are pluggable).
#
# WARNING: `podman`/`skopeo` are run using default parameters. Every workbench
# user is responsible for its local/global configurations.
# TODO: Unless this breaks reproducibility and with every call config files
# and parameters need to be overriden.
# For example:
# Local version of /etc/containers/containers.conf
#     mkdir -p $HOME/.config/containers/
#     touch $HOME/.config/containers/containers.conf
#     CONTAINERS_CONF=$HOME/.config/containers/containers.conf
# Local version of /etc/containers/storage.conf
# https://www.mankier.com/5/containers-storage.conf
#     mkdir -p $HOME/.local/share/containers/storage/volumes
#     touch $HOME/.config/containers/storage.conf
#     CONTAINERS_STORAGE_CONF=$HOME/.config/containers/storage.conf
# Local version of /etc/containers/policy.json
# https://www.mankier.com/5/containers-policy.json
#     mkdir -p $HOME/.config/containers/
#     touch $HOME/.config/containers/policy.json
nomad_create_folders_and_config() {
    local dir=$1
    # Folders:
    mkdir -p "$dir/nomad/config"
    mkdir -p "$dir/nomad/data"
    mkdir -p "$dir/nomad/data/plugins"
    # Podman Task Driver - Client Requirements:
    # "Ensure that Nomad can find the plugin, refer to `plugin_dir`."
    # https://www.nomadproject.io/plugins/drivers/podman#client-requirements
    ln -s "$(which nomad-driver-podman)" "$dir/nomad/data/plugins/nomad-driver-podman"
    # Config:
    # - `nomad` configuration docs:
    # - - https://www.nomadproject.io/docs/configuration
    # - Generic `nomad` plugins / task drivers configuration docs:
    # - - https://www.nomadproject.io/plugins/drivers
    # - - https://www.nomadproject.io/docs/configuration/plugin
    # - Specific `nomad` `podman` plugin / task driver configuration docs:
    # - - https://www.nomadproject.io/plugins/drivers/podman#plugin-options
    # - - https://github.com/hashicorp/nomad-driver-podman#driver-configuration
    cat > "$dir/nomad/config/nomad.hcl" <<- EOF
        region = "workbench"
        datacenter = "workbench"
        name = "workbench"
        data_dir  = "$dir/nomad/data"
        plugin_dir  = "$dir/nomad/data/plugins"
        bind_addr = "127.0.0.1"
        ports = {
          http = 4646
        }
        log_level = "INFO"
        log_json = true
        log_file = "$dir/nomad/"
        leave_on_interrupt = true
        leave_on_terminate = true
        plugin "nomad-driver-podman" {
          args = []
          config {
            # TODO: Use custom socket location!
            # socket_path = "unix:$dir/nomad/podman.sock"
            volumes {
              enabled = true
            }
          }
        }
EOF
}

# Start the `podman` API service needed by `nomad`.
nomad_start_podman_service() {
    local dir=$1
    # TODO: Use custom socket location!
    # podman --url "unix:$dir/nomad/podman.sock" system service --time 60 "unix:$dir/nomad/podman.sock" &
    local socket="/run/user/$UID/podman/podman.sock"
#    if test -S "$socket"
#    then
#        msg "Podman API service was already running"
#    else
        # The session is kept open waiting for a new connection for 60 seconds.
        # https://discuss.hashicorp.com/t/nomad-podman-rhel8-driver-difficulties/21877/4
        # `--time`: Time until the service session expires in seconds. Use 0
        # to disable the timeout (default 5).
        podman system service --time 60 &
        local i=0
        local patience=5
        while test ! -S "$socket"
        do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge $patience
            then echo
                progress "nomad-driver-podman" "$(red FATAL):  workbench:  nomad-driver-podman:  patience ran out after ${patience}s, socket $socket"
                backend_nomad stop-cluster "$dir"
                fatal "nomad-driver-podman startup did not succeed:  check logs"
            fi
            echo -ne "\b\b\b"
        done >&2
#    fi
    msg "Podman API service started"
}

nomad_create_job_file() {
    local dir=$1
    local oci_image_name=$(            envjqr 'oci_image_name')
    local oci_image_tag=$(             envjqr 'oci_image_tag')
    local container_workdir=$(         envjqr 'container_workdir')
    local container_mountpoint=$(      envjqr 'container_mountpoint')
    local container_supervisor_nix=$(  envjqr 'container_supervisor_nix')
    local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
    # CARDANO_MAINNET_MIRROR
    if test -n "$CARDANO_MAINNET_MIRROR"
    then
      local subpath=$(readlink -f "$CARDANO_MAINNET_MIRROR"/immutable)
      local optional_volumes="[
          \"$CARDANO_MAINNET_MIRROR:$CARDANO_MAINNET_MIRROR:ro\"
        , \"$subpath:$subpath:ro\"
      ]"
    else
      local optional_volumes="[]"
    fi
    # Volumes
    local jq_filter="
      [
        \"${dir}:/tmp/cluster/run/current:rw,exec\"
      ]
      +
      ( . | keys | map( \"${dir}/genesis:${container_mountpoint}/\" + . + \"/genesis:ro\" ) )
      +
      ( . | keys | map( \"${dir}/\" + . + \":${container_mountpoint}/generator/\" + . + \":ro\" ) )
      +
      ( . | keys | map( \"${dir}/genesis:${container_mountpoint}/generator/\" + . + \"/genesis:ro\" ) )
      +
      [
          \"${dir}/genesis:${container_mountpoint}/generator/genesis:ro\"
        , \"${dir}/genesis/utxo-keys:${container_mountpoint}/generator/genesis/utxo-keys:ro\"
      ]
      +
      \$optional_volumes
    "
    local podman_volumes=$(jq "$jq_filter" --argjson optional_volumes "$optional_volumes" "$dir"/profile/node-specs.json)
    # Create the task to run in `nomad` using `podman` driver.
    # https://www.nomadproject.io/docs/job-specification
    # https://www.nomadproject.io/docs/job-specification/job
    # https://github.com/hashicorp/nomad-driver-podman#task-configuration
    cat > "$dir/nomad/cluster-job.hcl" <<- EOF
    job "cluster" {
      region = "workbench"
      datacenters = [ "workbench" ]
      type = "service"
      reschedule {
        attempts = 0
        unlimited = false
      }
      # A group defines a series of tasks that should be co-located
      # on the same client (host). All tasks within a group will be
      # placed on the same host.
      group "cluster" {
        restart {
          attempts = 0
          mode = "fail"
        }
        network {
          mode = "host"
        }
        # The task stanza creates an individual unit of work, such as a
        # Docker container, web application, or batch processing.
        task "cluster" {
          driver = "podman"
          config {
            image = "${oci_image_name}:${oci_image_tag}"
            force_pull = false
            # TODO/FIXME: Don't know how to make podman log to nomad
            # instead of journald.
            # No argument or block type is named \"logging\"
            #logging = {
            #  driver = "nomad"
            #}
            tmpfs = [
              "/tmp"
            ]
            volumes = ${podman_volumes}
            working_dir = "${container_workdir}"
    #        hostname = "cluster"
          }
          env = {
            SUPERVISOR_NIX = "${container_supervisor_nix}"
            SUPERVISORD_CONFIG = "${container_supervisord_conf}"
          }
        }
      }
    }
EOF
}
