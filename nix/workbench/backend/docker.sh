usage_docker() {
     usage "docker" "Backend:  manages a local cluster using 'docker-compose'" <<EOF

    Please see documentation for 'wb backend' for the supported commands.

    Docker-specific:

    service-up         RUN-DIR SERVICE
    service-stop       RUN-DIR SERVICE
    is-service-running RUN-DIR SERVICE

EOF
}

backend_docker() {
op=${1:?$(usage_docker)}; shift

case "$op" in

    name )
        echo 'docker'
        ;;

    is-running )
        # Hack: Look for node-0's default port!
        test "$(sleep 0.5s; netstat -pltn 2>/dev/null | grep ':30000 ' | wc -l)" != "0"
        ;;

    setenv-defaults )
        local usage="USAGE: wb docker $op PROFILE-DIR"
        local profile_dir=${1:?$usage}
        ;;

    # Note on `docker-compose` invocation:
    #
    # The Compose project name is used as prefix to name containers, volumes and
    # networks. Here every `docker-compose` command is run with
    # `--file "$(realpath $file)` with `$file` having the location of the
    # `docker-compose.yaml` so the project name is automagically set to the
    # parent folder (Here is the `WB_RUNDIR_TAG` and `realpath` is used to
    # ensure it is set to an actual tag and not just to `current`) (the
    # alternatives options are to use `--project-directory` and let docker look
    # for a file named `docker-compose.yml` or use the `-p|--project-name` flag).
    #
    # To avoid name clashing with containers outside the `docker-compose.yaml`
    # being used, `container_name: "SOMETHING"` SHOULD NOT be used when defining
    # the cluster. As long as `docker-compose` knows the project name, services
    # can be referenced using their name on the `service` object.
    #
    # Further reading:
    # - https://docs.docker.com/engine/reference/commandline/compose/
    # - - "If flags are explicitly set on command line, associated environment variable is ignored"
    # - - https://docs.docker.com/engine/reference/commandline/compose/#use--f-to-specify-name-and-path-of-one-or-more-compose-files
    # - - https://docs.docker.com/engine/reference/commandline/compose/#use--p-to-specify-a-project-name
    # - https://docs.docker.com/compose/reference/envvars/
    # - - https://docs.docker.com/compose/reference/envvars/#compose_project_name
    # - - https://docs.docker.com/compose/reference/envvars/#compose_file

    allocate-run )
        local usage="USAGE: wb docker $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_docker;;
               * ) break;; esac; shift; done

        # Check that the `cardano-node` OCI image exists.
        local nodeImageName=${WB_NODE_IMAGE_NAME:-$(cat "$dir/profile/cardanoNodeImageName")}
        local nodeImageTag=${WB_NODE_IMAGE_TAG:-$(cat "$dir/profile/cardanoNodeImageTag")}
        if ! docker images --format "{{.Repository}}:{{.Tag}}" | grep --quiet "${nodeImageName}:${nodeImageTag}"
        then
            fatal "Docker image ${nodeImageName}:${nodeImageTag} does not exists"
        fi
        # Check that the `cardano-tracer` OCI image exists.
        local tracerImageName=${WB_TRACER_IMAGE_NAME:-$(cat "$dir/profile/cardanoTracerImageName")}
        local tracerImageTag=${WB_TRACER_IMAGE_TAG:-$(cat "$dir/profile/cardanoTracerImageTag")}
        if ! docker images --format "{{.Repository}}:{{.Tag}}" | grep --quiet "${tracerImageName}:${tracerImageTag}"
        then
            fatal "Docker image ${tracerImageName}:${tracerImageTag} does not exists"
        fi
        # Check that the `tx-generator` OCI image exists.
        local generatorImageName=${WB_GENERATOR_IMAGE_NAME:-$(cat "$dir/profile/txGeneratorImageName")}
        local generatorImageTag=${WB_GENERATOR_IMAGE_TAG:-$(cat "$dir/profile/txGeneratorImageTag")}
        if ! docker images --format "{{.Repository}}:{{.Tag}}" | grep --quiet "${generatorImageName}:${generatorImageTag}"
        then
            fatal "Docker image ${generatorImageName}:${generatorImageTag} does not exists"
        fi
        # Info about the `--format` parameter and Go templates:
        # https://docs.docker.com/config/formatting/

        # Every volume is defined in the `volumes` yaml collection as opposed to
        # many times in the `services` collections that use it. This allows to
        # have every volume the cluster needs defined in only one place.
        # The `genesis/utxo-keys` directory is used as a volume for the
        # `generator` service but it's not always present/created. Due to
        # Docker's behaviour of creating all the volumes even if they re not
        # needed we always create this directory here.
        mkdir -p "$dir/genesis/utxo-keys"

        # Copy the docker-compose.yaml file created by Nix using docker's
        # `config` subcommand so its components are always located using the
        # same pre-defined order (easier to compare/diff) and environment
        # variables are resolved once (no need to pass envars every time).
        WB_RUNDIR=$(realpath "$dir") docker-compose --file "$(realpath $dir/profile)/docker-compose.yaml" config > "$dir"/docker-compose.yaml

        # Keep track of all the `services` that should be `up`.
        # See "# Note on `services` lifecycle:" below.
        yq '.services | with_entries(.value |= {"up":false})' "$dir"/docker-compose.yaml > "$dir/docker-compose.status.json"
        ;;

    describe-run )
        local usage="USAGE: wb docker $op RUN-DIR"
        local dir=${1:?$usage}

        echo "  - docker-compose.yaml: $(realpath $dir)/docker-compose.yaml"
        ;;

    # Note on `services` lifecycle:
    #
    # For the workbench we need to be able to start and stop nodes on demand to
    # create `scenarios`, we can't just always do `docker-compose up` and start
    # the whole cluster. And even without the `scenarios` requirement the
    # special case of `generator` starting before `node-#` and failing because
    # the node's socket is not ready needs to be addressed somehow.
    #
    # Very important: A strong requirement is for the whole cluster to fail
    # loudly when any service inside the cluster fails.
    #
    # Following `docker-compose` man pages you can either do:
    # - `docker-compose up --abort-on-container-exit`
    # - `docker-compose up --detach`
    # BUT NOT BOTH.
    #
    # When you use `--detach` the process is sent to the background as soon as
    # services are in the `up` state (entrypoint commands have run, nothing to
    # do with a daemon being ready to accept new connections) but nobody keeps
    # watching for processes events. On the other hand `--abort-on-container-exit`
    # watchs for container events indicating an exit but runs in the foreground.
    # The UNDOCUMENTED feature of `docker-compose` (maybe it's an assumption
    # from how `docker attach|up` behave) is that you can start a new service
    # and watch for exit events of services already running if you pass as
    # arguments all the services and `--no-recreate`. This makes the whole
    # cluster fail as needed when any of the services fail!
    #
    # When doing `down` we use `--volumes --remove-orphans` to make sure
    # everything related to the actual `docker-compose` file is destroyed.
    #
    # Further reading:
    # - https://docs.docker.com/engine/reference/commandline/compose_up/
    # - https://docs.docker.com/compose/faq/#whats-the-difference-between-up-run-and-start
    # - https://github.com/docker/compose/issues/6160#issuecomment-417781865
    # - https://github.com/docker/compose/issues/3909

    service-up )
        local usage="USAGE: wb docker $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        # Update $service in the file tracking services status.
        jq ". + {\"${service}\":{\"up\":true}}" "$dir/docker-compose.status.json" | sponge "$dir/docker-compose.status.json"
        # Create the variable array with all the services that should be `up`.
        local services_up=($(jq_tolist '. | with_entries( select(.value.up) ) | keys' "$dir/docker-compose.status.json"))
        # Annotate `docker-compose.stderr` before sending to the background.
        echo "'docker-compose up' ${services_up[*]} ..." >> "$dir/docker-compose.stderr"
        # Again `up` but using `--abort-on-container-exit` and `--no-recreate`.
        docker-compose --file $(realpath "$dir")/docker-compose.yaml up --abort-on-container-exit --no-recreate ${services_up[*]} > /dev/null 2>> "$dir/docker-compose.stderr" &
        ;;

    # Note on logs:
    #
    # We are running services in the background but the output is still needed.
    #
    # Services use one of the many log drivers (from syslog to splunk):
    # - https://docs.docker.com/compose/compose-file/compose-file-v3/#logging
    # - https://docs.docker.com/config/containers/logging/configure/
    # with the default being `json-file`, the one we are using.
    #
    # This json logs are stored, for each container, in:
    # `docker inspect --format='{{.LogPath}}' CONTAINER_ID|CONTAINER_NAME`
    # somewhere of the likes of `/var/lib/docker/containers/CONTAINER_ID/*`.
    # 1) These files are only viewable by the root user.
    # 2) Files are destroyed after running `docker-compose down`.
    # 3) Logs can be seen with `docker-compose logs` but only if the service
    #    is already up and running generating race conditions.
    #
    # To circumvent these problems the logs are copied just before stopping a
    # service or cluster by dumping the outputs of `docker-compose logs` for
    # each service the cluster runs.
    # These JSON log files have tags with `stdout` or `stderr` for every
    # message, so we assume the `logs` command outputs the log using `stdout`
    # and `stderr` correspondingly.
    #
    # Further reading:
    # - https://docs.docker.com/compose/compose-file/compose-file-v3/#logging
    # - https://docs.docker.com/config/containers/logging/configure/
    # - https://docs.docker.com/config/containers/logging/json-file/
    # - https://docs.docker.com/engine/reference/commandline/compose_logs/

    service-stop )
        local usage="USAGE: wb docker $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        # Update $service in the file tracking services status.
        jq ". + {\"${service}\":{\"up\":false}}" "$dir/docker-compose.status.json" | sponge "$dir/docker-compose.status.json"
        # Annotate `docker-compose.stderr`.
        echo "'docker-compose stop' ${service} ..." >> "$dir/docker-compose.stderr"
        # First copy the service's log to its subfolder because it will be deleted.
        docker-compose --file $(realpath "$dir")/docker-compose.yaml logs --no-color --no-log-prefix "$service" > "$dir/$service/stdout" 2> "$dir/$service/stderr"
        # Take the service down and remove everything that is not needed anymore.
        docker-compose --file $(realpath "$dir")/docker-compose.yaml stop $service
        ;;

    start-node )
        local usage="USAGE: wb docker $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        backend_docker service-up "$dir" $node
        # Always wait for the node to be ready. not just a running container.
        backend_docker wait-node "$dir" $node
        ;;

    stop-node )
        local usage="USAGE: wb docker $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        backend_docker service-stop "$dir" $node
        ;;

    wait-node )
        local usage="USAGE: wb docker $op RUN-DIR [NODE-NAME]"
        local dir=${1:?$usage}; shift
        local node=${1:-$(dirname $CARDANO_NODE_SOCKET_PATH | xargs basename)}; shift
        local socket=$(backend_docker get-node-socket-path "$dir" $node)

        local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' $dir/profile.json) i=0
        echo -n "workbench:  docker:  waiting ${patience}s for socket of $node: " >&2
        while test ! -S $socket
        do printf "%3d" $i; sleep 1
           i=$((i+1))
           if test $i -ge $patience
           then echo
                progress "docker" "$(red FATAL):  workbench:  docker:  patience ran out for $(white $node) after ${patience}s, socket $socket"
                backend_docker stop-cluster "$dir"
                fatal "$node startup did not succeed:  check logs in $(dirname $socket)/stdout & stderr"
           fi
           echo -ne "\b\b\b"
        done >&2
        echo " $node up (${i}s)" >&2
        ;;

    start-nodes )
        local usage="USAGE: wb docker $op RUN-DIR [HONOR_AUTOSTART=]"
        local dir=${1:?$usage}; shift
        local honor_autostart=${1:-}

        local nodes=($(jq_tolist keys "$dir"/node-specs.json))
        for node in ${nodes[*]}
        do
            if test -n "$honor_autostart"
            then
                if jqtest ".\"$node\".autostart" "$dir"/node-specs.json
                then
                    backend_docker start-node "$dir" "$node"
                fi
            else
                backend_docker start-node "$dir" "$node"
            fi
        done

        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$(backend_docker get-node-socket-path "$dir" 'node-0')
        fi
        ;;

    start )
        local usage="USAGE: wb docker $op RUN-DIR"
        local dir=${1:?$usage}; shift

        if jqtest ".node.tracer" "$dir"/profile.json
        then
          backend_docker service-up "$dir" tracer
          # Wait for tracer socket
          # If tracer fails here, the rest of the cluster is brought up without
          # any problems.
          local socket=$(jq -r '.network.contents' "$dir/tracer/config.json")
          local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' "$dir/profile.json") i=0
          echo -n "workbench:  docker:  waiting ${patience}s for socket of tracer: " >&2
          while test ! -S "$dir/tracer/$socket"
          do printf "%3d" $i; sleep 1
             i=$((i+1))
             if test $i -ge $patience
             then echo
                  progress "docker" "$(red FATAL):  workbench:  docker:  patience ran out for $(white tracer) after ${patience}s, socket $socket"
                  backend_docker stop-cluster "$dir"
                  fatal "$node startup did not succeed:  check logs in $(dirname $socket)/stdout & stderr"
             fi
             echo -ne "\b\b\b"
          done >&2
          echo " tracer up (${i}s)" >&2
        fi
        ;;

    get-node-socket-path )
        local usage="USAGE: wb docker $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}
        local node_name=${2:?$usage}

        echo -n $dir/$node_name/node.socket
        ;;

    start-generator )
        local usage="USAGE: wb docker $op RUN-DIR"
        local dir=${1:?$usage}; shift

        while test $# -gt 0
        do case "$1" in
               --* ) msg "FATAL:  unknown flag '$1'"; usage_docker;;
               * ) break;; esac; shift; done

        backend_docker service-up "$dir" generator
        ;;

    # Docker-specific
    is-service-running )
        local usage="USAGE: wb docker $op RUN-DIR DOCKER-SERVICE"
        local dir=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        # More info about the Docker ps subcommand:
        # - https://forums.docker.com/t/what-filters-are-available-for-docker-compose-ps/88818
        # - https://github.com/docker/compose/issues/5996
        docker-compose --file $(realpath "$dir")/docker-compose.yaml ps --services --filter "status=running" | grep --quiet "$service"
        ;;

    wait-node-stopped )
        local usage="USAGE: wb docker $op RUN-DIR NODE"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        progress_ne "docker" "waiting until $node stops:  ....."
        local i=0
        while backend_docker is-service-running "$dir" "$node"
        do
          echo -ne "\b\b\b\b\b"; printf "%5d" $i >&2; i=$((i+1))
          sleep 1
        done >&2
        echo -e "\b\b\b\b\bdone, after $(with_color white $i) seconds" >&2
        ;;

    wait-pools-stopped )
        local usage="USAGE: wb docker $op RUN-DIR"
        local dir=${1:?$usage}; shift

        local i=0 pools=$(jq .composition.n_pool_hosts $dir/profile.json) start_time=$(date +%s)
        msg_ne "docker:  waiting until all pool nodes are stopped: 000000"
        touch $dir/flag/cluster-termination

        for ((pool_ix=0; pool_ix < $pools; pool_ix++))
        do
          while backend_docker is-service-running "$dir" "node-${pool_ix}" && test -f $dir/flag/cluster-termination
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
        local usage="USAGE: wb docker $op RUN-DIR"
        local dir=${1:?$usage}; shift

        # Take down the services flagged as up.
        local services_up=($(jq_tolist '. | with_entries( select(.value.up) ) | keys' "$dir/docker-compose.status.json"))
        for service in ${services_up[*]}
        do
            backend_docker service-stop "$dir" "$service"
        done
        # Annotate `docker-compose.stderr`.
        echo "'docker-compose down' ..." >> "$dir/docker-compose.stderr"
        # Make sure everything is down.
        docker-compose --file $(realpath "$dir")/docker-compose.yaml down --volumes --remove-orphans 2>> "$dir/docker-compose.stderr"
        ;;

    cleanup-cluster )
        local usage="USAGE: wb docker $op RUN-DIR"
        local dir=${1:?$usage}; shift

        msg "docker:  resetting cluster state in:  $dir"
        rm -f $dir/*/std{out,err} $dir/node-*/*.socket $dir/*/logs/* 2>/dev/null || true
        rm -fr $dir/node-*/state-cluster/
        ;;

    * ) usage_docker;; esac
}
