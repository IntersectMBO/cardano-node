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
        local usage="USAGE: wb nomad $op BACKEND-DIR"
        local backend_dir=${1:?$usage}

        # TODO: stateful nomad ?
        # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
        # ${XDG_STATE_HOME:-$HOME/.local/state}

        # The output files of the profiles Nix derivation:
        ## The one provided by the profile, the one used may suffer changes (jq).
        local profile_nomad_job_file="$backend_dir"/nomad-job.json
        setenvjqstr 'profile_nomad_job_file' "$profile_nomad_job_file"
        ## Look up `cluster` OCI image's name and tag (also Nix profile).
        setenvjqstr 'oci_image_name' ${WB_OCI_IMAGE_NAME:-$(jq -r '. ["clusterNode"]["imageName"]' "$backend_dir"/oci-images.json)}
        setenvjqstr 'oci_image_tag'  ${WB_OCI_IMAGE_TAG:-$(jq -r '. ["clusterNode"]["imageTag"]' "$backend_dir"/oci-images.json)}
        ## Script that creates the OCI image from nix2container layered output.
        setenvjqstr 'oci_image_skopeo_script' $(jq -r '. ["clusterNode"]["copyToPodman"]' "$backend_dir"/oci-images.json)

        # Socket of the process that connects nomad-driver-podman with podman.
        # Can't reside inside $dir, can't use a path longer than 108 characters!
        # See: https://man7.org/linux/man-pages/man7/unix.7.html
        # char        sun_path[108];            /* Pathname */
        setenvjqstr 'podman_socket_path' "/run/user/$UID/workbench-podman.sock"

        # Fetch all the default values that are inside the meta stanza:
        ## Get the job and group name from the job's JSON description.
        local nomad_job_name=$(jq -r '. ["job"] | keys[0]' "$profile_nomad_job_file")
        setenvjqstr 'nomad_job_name' "$nomad_job_name"
        local nomad_job_group_name=$(jq -r ". [\"job\"][\"$nomad_job_name\"][\"group\"] | keys[0]" "$profile_nomad_job_file")
        setenvjqstr 'nomad_job_group_name' "$nomad_job_group_name"
        ## The workbench is expecting an specific hierarchy of folders and files.
        setenvjqstr 'container_workdir' $(jq -r ". [\"job\"][\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"meta\"][\"WORKING_DIRECTORY\"]" "$profile_nomad_job_file")
        setenvjqstr 'container_mountpoint' $(jq -r ". [\"job\"][\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"meta\"][\"STATE_DIRECTORY\"]" "$profile_nomad_job_file")
        ## The `supervisord` binary is installed inside the container but not
        ## added to $PATH (resides in /nix/store), so a desired location is
        ## passed to the container as an environment variable to create a symlink
        ## to it.
        setenvjqstr 'container_supervisor_nix' $(jq -r ". [\"job\"][\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"meta\"][\"SUPERVISOR_NIX\"]" "$profile_nomad_job_file")
        ## The `--serverurl` argument is needed in every call to `nomad exec`.
        setenvjqstr 'container_supervisord_url' $(jq -r ". [\"job\"][\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"meta\"][\"SUPERVISORD_URL\"]" "$profile_nomad_job_file")
        ## The container needs to know where the `supervisord` config file is
        ## located so it can be started. This is passed as an environment var.
        setenvjqstr 'container_supervisord_conf' $(jq -r ". [\"job\"][\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"meta\"][\"SUPERVISORD_CONFIG\"]" "$profile_nomad_job_file")
        ## The logging level at which supervisor should write to the activity
        ## log. Valid levels are trace, debug, info, warn, error and critical.
        setenvjqstr 'container_supervisord_loglevel' $(jq -r ". [\"job\"][\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"meta\"][\"SUPERVISORD_LOGLEVEL\"]" "$profile_nomad_job_file")
        ## One tracer for all or one tracer per node?
        setenvjq    'one_tracer_per_node' $(jq ". [\"job\"][\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"meta\"][\"ONE_TRACER_PER_NODE\"]" "$profile_nomad_job_file")
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

        local trac_dir="$dir"/tracer
        mkdir -p "$trac_dir"
        # When only one shared tracer is used, the tracer directory is the only
        # task/service that is still mounted as a volume for the nomad backend.
        # Generator and nodes folders contents are defined in the nomad job file
        # and are created inside the container when started.
        # * "genesis" and "CARDANO_MAINNET_MIRROR" are the exceptions!
        local one_tracer_per_node=$(envjq 'one_tracer_per_node')
        if ! test "$one_tracer_per_node" = "true"
        then
          local trac=$dir/profile/tracer-service.json
          cp $(jq '."tracer-config"'  -r $trac) "$trac_dir"/tracer-config.json
          cp $(jq '."service-config"' -r $trac) "$trac_dir"/service-config.json
          cp $(jq '."config"'         -r $trac) "$trac_dir"/config.json
          cp $(jq '."start"'          -r $trac) "$trac_dir"/start.sh
        fi
        # Else a symlink to every tracer folder will be created inside trac_dir.

        # Create the "cluster" OCI image.
        local oci_image_name=$(         envjqr 'oci_image_name')
        local oci_image_tag=$(          envjqr 'oci_image_tag')
        local oci_image_skopeo_script=$(envjqr 'oci_image_skopeo_script')
        msg "Creating OCI image ..."
        # TODO: for further research.
        # STORAGE_DRIVER=overlay "$oci_image_skopeo_script"
        # If podman 4.2.1 and nomad v1.3.5 this fix is not needed anymore
        # Forced the `overlay` storage driver or podman won't see the image.
        # https://docs.podman.io/en/latest/markdown/podman.1.html#note-unsupported-file-systems-in-rootless-mode
        # Error was: workbench:  FATAL: OCI image registry.workbench.iog.io/cluster:2l7wi7sh1zyp2mnl24m13ibnh2wsjvwg cannot be found by podman
        "$oci_image_skopeo_script"
        # Check that `podman` can see the "cluster" OCI image.
        if ! podman image exists "${oci_image_name}:${oci_image_tag}"
        then
            fatal "OCI image ${oci_image_name}:${oci_image_tag} cannot be found by podman"
        else
            msg "OCI image named \"${oci_image_name}:${oci_image_tag}\" created"
        fi

        # Create config files for Nomad and the Podman plugin/task driver.
        nomad_create_folders_and_config "$dir"

        # Create the Nomad job file.
        nomad_create_job_file "$dir"
        ;;

    describe-run )
        local usage="USAGE: wb nomad $op RUN-DIR"
        local dir=${1:?$usage}

        echo "  - Nomad job: $(realpath $dir)/nomad/nomad-job.json"
        ;;

    # Nomad-specific
    service-start )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local task=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        msg "Starting supervisord service \"$service\" inside nomad task/container \"$task\" ..."
        backend_nomad nomad-alloc-exec-supervisorctl "$dir" "$task" start "$service"
        ;;

    # Nomad-specific
    service-stop )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local task=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        msg "Stopping supervisord service \"$service\" inside nomad task/container \"$task\" ..."
        backend_nomad nomad-alloc-exec-supervisorctl "$dir" "$task" stop "$service"
        ;;

    # Nomad-specific
    is-service-running )
        local usage="USAGE: wb nomad $op RUN-DIR DOCKER-SERVICE"
        local dir=${1:?$usage}; shift
        local task=${1:?$usage}; shift
        local service=${1:?$usage}; shift

        backend_nomad nomad-alloc-exec-supervisorctl "$dir" "$task" status "$service" > /dev/null && true
        ;;

    # Nomad-specific
    nomad-alloc-exec-supervisorctl )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local task=${1:?$usage}; shift
        local action=${1:?$usage}; shift

        local nomad_alloc_id=$(envjqr 'nomad_alloc_id')
        local container_supervisor_nix=$(envjqr 'container_supervisor_nix')
        local container_supervisord_url=$(envjqr 'container_supervisord_url')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        nomad alloc exec --task "$task" "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$container_supervisord_url" --configuration "$container_supervisord_conf" "$action" $@
        ;;

    start-node )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        backend_nomad service-start "$dir" $node $node
        # Always wait for the node to be ready.
        backend_nomad wait-node "$dir" $node
        ;;

    stop-node )
        local usage="USAGE: wb nomad $op RUN-DIR NODE-NAME"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        backend_nomad service-stop "$dir" $node $node
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
        local one_tracer_per_node=$(envjq 'one_tracer_per_node')

        msg "Preparing podman API service for nomad driver \`nomad-driver-podman\` ..."
        nomad_start_podman_service "$dir"

        # Start `nomad` agent".
        msg "Starting nomad agent ..."
        # The Nomad agent is a long running process which runs on every machine
        # that is part of the Nomad cluster. The behavior of the agent depends
        # on if it is running in client or server mode. Clients are responsible
        # for running tasks, while servers are responsible for managing the
        # cluster.
        #
        # The Nomad agent supports multiple configuration files, which can be
        # provided using the -config CLI flag. The flag can accept either a file
        # or folder. In the case of a folder, any .hcl and .json files in the
        # folder will be loaded and merged in lexicographical order. Directories
        # are not loaded recursively.
        #   -config=<path>
        # The path to either a single config file or a directory of config files
        # to use for configuring the Nomad agent. This option may be specified
        # multiple times. If multiple config files are used, the values from
        # each will be merged together. During merging, values from files found
        # later in the list are merged over values from previously parsed file.
        #
        # Running a dual-role agent (client + server) but not "-dev" mode.
        nomad agent -config="$dir/nomad/config" >> "$dir/nomad/stdout" 2>> "$dir/nomad/stderr" &
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
                cat "$dir/nomad/stderr"
                backend_nomad stop-cluster "$dir"
                fatal "nomad agent startup did not succeed:  check logs"
            fi
            echo -ne "\b\b\b"
        done >&2

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
        nomad job run -verbose "$dir/nomad/nomad-job.json" || true
        # Assuming that `nomad` placement is enough wait.
        local nomad_job_name=$(envjqr 'nomad_job_name')
        local nomad_alloc_id=$(nomad job allocs -json "$nomad_job_name" | jq -r '.[0].ID')
        setenvjqstr 'nomad_alloc_id' "$nomad_alloc_id"
        msg "Nomad job allocation ID is: $nomad_alloc_id"

        # A supervisord server is run for every Nomad task/container.
        # A symlink to every supervisor folder will be created inside this folder.
        mkdir -p "$dir"/supervisor
        # For every node ...
        local nodes=($(jq_tolist keys "$dir"/node-specs.json))
        for node in ${nodes[*]}
        do
          ln -s "$dir/nomad/data/alloc/$nomad_alloc_id/$node/local/run/current/$node" "$dir/$node"
          ln -s "$dir/nomad/data/alloc/$nomad_alloc_id/$node/local/run/current/supervisor" "$dir/supervisor/$node"
          # Tracer(s).
          if jqtest ".node.tracer" "$dir"/profile.json && test "$one_tracer_per_node" = "true"
          then
            # A symlink to every tracer folder.
            ln -s "$dir/nomad/data/alloc/$nomad_alloc_id/$node/local/run/current/tracer" "$dir/tracer/$node"
          fi
        done
        # Generator runs inside task/supervisord "node-0"
        ln -s "$dir/nomad/data/alloc/$nomad_alloc_id/node-0/local/run/current/generator" "$dir/generator"

        # Show `--status` of `supervisorctl` inside the container.
        local container_supervisor_nix=$(  envjqr 'container_supervisor_nix')
        local container_supervisord_url=$( envjqr 'container_supervisord_url')
        local container_supervisord_conf=$(envjqr 'container_supervisord_conf')
        msg "Supervisor status inside container ..."
        # Print the command used for debugging purposes.
        msg "'nomad alloc exec --task node-0 \"$nomad_alloc_id\" \"$container_supervisor_nix\"/bin/supervisorctl --serverurl \"$container_supervisord_url\" --configuration \"$container_supervisord_conf\" status'"
        # Execute the actual command.
        nomad alloc exec --task node-0 "$nomad_alloc_id" "$container_supervisor_nix"/bin/supervisorctl --serverurl "$container_supervisord_url" --configuration "$container_supervisord_conf" status || true

        # Start tracer(s).
        if jqtest ".node.tracer" "$dir"/profile.json
        then
          local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' "$dir/profile.json")
          if test "$one_tracer_per_node" = "true"
          then
            local nodes=($(jq_tolist keys "$dir"/node-specs.json))
            for node in ${nodes[*]}
            do
              if ! backend_nomad service-start "$dir" "$node" tracer
              then
                fatal "check logs in $dir/tracer/$node/stderr"
              fi
              # Wait for tracer socket
              local socket_path_relative=$(jq -r '.network.contents' "$dir/tracer/$node/config.json")
              local socket_path_absolute="$dir/tracer/$node/$socket_path_relative"
              echo -n "workbench:  nomad:  waiting ${patience}s for socket of $node's tracer: " >&2
              local i=0
              while test ! -S "$socket_path_absolute"
              do printf "%3d" $i; sleep 1
                i=$((i+1))
                if test $i -ge $patience
                then echo
                  progress "nomad" "$(red FATAL):  workbench:  nomad:  patience ran out for $(white tracer) after ${patience}s, socket $socket_path_absolute"
                  backend_nomad stop-cluster "$dir"
                  fatal "$node's tracer startup did not succeed:  check logs in $dir/tracer/$node/[stdout & stderr]"
                fi
                echo -ne "\b\b\b"
              done >&2
              echo " $node's tracer up (${i}s)" >&2
            done
          else
            if ! backend_nomad service-start "$dir" tracer tracer
            then
              fatal "check logs in $dir/tracer/stderr"
            fi
            # Wait for tracer socket
            local socket_path_relative=$(jq -r '.network.contents' "$dir/tracer/config.json")
            local socket_path_absolute="$dir/tracer/$socket_path_relative"
            echo -n "workbench:  nomad:  waiting ${patience}s for socket of tracer: " >&2
            local i=0
            while test ! -S "$socket_path_absolute"
            do printf "%3d" $i; sleep 1
              i=$((i+1))
              if test $i -ge $patience
              then echo
                progress "nomad" "$(red FATAL):  workbench:  nomad:  patience ran out for $(white tracer) after ${patience}s, socket $socket_path_absolute"
                backend_nomad stop-cluster "$dir"
                fatal "tracer startup did not succeed:  check logs in $dir/tracer/[stdout & stderr]"
              fi
              echo -ne "\b\b\b"
            done >&2
            echo " tracer up (${i}s)" >&2
          fi
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

        backend_nomad service-start "$dir" node-0 generator
        ;;

    wait-node-stopped )
        local usage="USAGE: wb nomad $op RUN-DIR NODE"
        local dir=${1:?$usage}; shift
        local node=${1:?$usage}; shift

        progress_ne "docker" "waiting until $node stops:  ....."
        local i=0
        while backend_nomad is-service-running "$dir" "$node" "$node"
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
          while backend_nomad is-service-running "$dir" "node-${pool_ix}" "node-${pool_ix}" && test -f $dir/flag/cluster-termination
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
        local nomad_job_name=$(envjqr 'nomad_job_name')

        # Stop generator.
        backend_nomad service-stop "$dir" node-0 generator || true
        # Stop tracer(s).
        local one_tracer_per_node=$(envjq 'one_tracer_per_node')
        if jqtest ".node.tracer" "$dir"/profile.json
        then
          if test "$one_tracer_per_node" = "true"
          then
            local nodes=($(jq_tolist keys "$dir"/node-specs.json))
            for node in ${nodes[*]}
            do
              backend_nomad service-stop "$dir" "$node" tracer || true
            done
          else
            backend_nomad service-stop "$dir" tracer tracer || true
          fi
        fi
        # Stop nodes.
        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do
          backend_nomad service-stop "$dir" "$node" "$node" || true
        done

        msg "Stopping nomad job ..."
        # FIXME:
        # ERRO[0087] Unable to get cgroup path of container: cannot get cgroup path unless container b2f4fea15a4a56591231fae10e3c3e55fd485b2c0dfb231c073e2a3c9efa0e42 is running: container is stopped
        # {"@level":"debug","@message":"Could not get container stats, unknown error","@module":"podman.podmanHandle","@timestamp":"2022-12-14T14:34:03.264133Z","driver":"podman","error":"\u0026json.SyntaxError{msg:\"unexpected end of JSON input\", Offset:0}","timestamp":"2022-12-14T14:34:03.264Z"}
        # {"@level":"debug","@message":"Could not get container stats, unknown error","@module":"podman.podmanHandle","@timestamp":"2022-12-14T14:34:16.320494Z","driver":"podman","error":"\u0026url.Error{Op:\"Get\", URL:\"http://u/v1.0.0/libpod/containers/a55f689be4d2898225c76fa12716cfa0c0dedd54a1919e82d44523a35b8d07a4/stats?stream=false\", Err:(*net.OpError)(0xc000ba5220)}","timestamp":"2022-12-14T14:34:16.320Z"}
        nomad job stop -global -no-shutdown-delay -purge -yes -verbose "$nomad_job_name" >> "$dir/nomad/stdout" 2>> "$dir/nomad/stderr"

        local nomad_pid=$(envjqr 'nomad_pid')
        msg "Killing nomad agent (PID $nomad_pid)..."
        kill -SIGINT "$nomad_pid"
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

# Start the `podman` API service needed by `nomad`.
nomad_start_podman_service() {
    local dir=$1
    local podman_socket_path=$(envjqr 'podman_socket_path')
#    if test -S "$socket"
#    then
#        msg "Podman API service was already running"
#    else
        # The session is kept open waiting for a new connection for 60 seconds.
        # https://discuss.hashicorp.com/t/nomad-podman-rhel8-driver-difficulties/21877/4
        # `--time`: Time until the service session expires in seconds. Use 0
        # to disable the timeout (default 5).
        podman system service --time 60 "unix://$podman_socket_path" &
        local i=0
        local patience=5
        while test ! -S "$podman_socket_path"
        do printf "%3d" $i; sleep 1
            i=$((i+1))
            if test $i -ge $patience
            then echo
                progress "nomad-driver-podman" "$(red FATAL):  workbench:  nomad-driver-podman:  patience ran out after ${patience}s, socket $podman_socket_path"
                backend_nomad stop-cluster "$dir"
                fatal "nomad-driver-podman startup did not succeed:  check logs"
            fi
            echo -ne "\b\b\b"
        done >&2
#    fi
    msg "Podman API service started"
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
    # - - https://developer.hashicorp.com/nomad/docs/configuration
    # - Generic `nomad` plugins / task drivers configuration docs:
    # - - https://www.nomadproject.io/plugins/drivers
    # - - https://www.nomadproject.io/docs/configuration/plugin
    # - Specific `nomad` `podman` plugin / task driver configuration docs:
    # - - https://www.nomadproject.io/plugins/drivers/podman#plugin-options
    # - - https://github.com/hashicorp/nomad-driver-podman#driver-configuration
    local podman_socket_path=$(envjqr 'podman_socket_path')
    cat > "$dir/nomad/config/nomad.hcl" <<- EOF
# Names:
########
# Specifies the region the Nomad agent is a member of. A region typically maps
# to a geographic region, for example us, with potentially multiple zones, which
# map to datacenters such as us-west and us-east.
region = "workbench-region"
# Specifies the data center of the local agent. All members of a datacenter
# should share a local LAN connection.
datacenter = "workbench-datacenter-1"
# Specifies the name of the local node. This value is used to identify
# individual agents. When specified on a server, the name must be unique within
# the region.
name = "workbench-nomad-agent-1"

# Paths:
########
# Specifies a local directory used to store agent state. Client nodes use this
# directory by default to store temporary allocation data as well as cluster
# information. Server nodes use this directory to store cluster state, including
# the replicated log and snapshot data. This must be specified as an absolute
# path.
data_dir  = "$dir/nomad/data"
# Specifies the directory to use for looking up plugins. By default, this is the
# top-level data_dir suffixed with "plugins", like "/opt/nomad/plugins". This
# must be an absolute path.
plugin_dir  = "$dir/nomad/data/plugins"

# Network:
##########
# Specifies which address the Nomad agent should bind to for network services,
# including the HTTP interface as well as the internal gossip protocol and RPC
# mechanism. This should be specified in IP format, and can be used to easily
# bind all network services to the same address. It is also possible to bind the
# individual services to different addresses using the "addresses" configuration
# option. Dev mode (-dev) defaults to localhost.
bind_addr = "127.0.0.1"
# Specifies the network ports used for different services required by the Nomad
# agent.
ports = {
  # The port used to run the HTTP server.
  http = 4646
  # The port used for internal RPC communication between agents and servers, and
  # for inter-server traffic for the consensus algorithm (raft).
  rpc  = 4647
  # The port used for the gossip protocol for cluster membership. Both TCP and
  # UDP should be routable between the server nodes on this port.
  serf = 4648
}
# Specifies the advertise address for individual network services. This can be
# used to advertise a different address to the peers of a server or a client
# node to support more complex network configurations such as NAT. This
# configuration is optional, and defaults to the bind address of the specific
# network service if it is not provided. Any values configured in this stanza
# take precedence over the default "bind_addr".
# If the bind address is 0.0.0.0 then the IP address of the default private
# network interface advertised. The advertise values may include an alternate
# port, but otherwise default to the port used by the bind address. The values
# support go-sockaddr/template format.
# Needed becasue of the below error message:
# "Defaulting advertise to localhost is unsafe, please set advertise manually"
advertise {
  # The address to advertise for the HTTP interface. This should be reachable by
  # all the nodes from which end users are going to use the Nomad CLI tools.
  http = "127.0.0.1:4646"
  # The address used to advertise to Nomad clients for connecting to Nomad
  # servers for RPC. This allows Nomad clients to connect to Nomad servers from
  # behind a NAT gateway. This address much be reachable by all Nomad client
  # nodes. When set, the Nomad servers will use the advertise.serf address for
  # RPC connections amongst themselves. Setting this value on a Nomad client has
  # no effect.
  rpc = "127.0.0.1:4647"
  # The address advertised for the gossip layer. This address must be reachable
  # from all server nodes. It is not required that clients can reach this
  # address. Nomad servers will communicate to each other over RPC using the
  # advertised Serf IP and advertised RPC Port.
  serf = "127.0.0.1:4648"
}
# The tls stanza configures Nomad's TLS communication via HTTP and RPC to
# enforce secure cluster communication between servers, clients, and between.
tls {
  # Specifies if TLS should be enabled on the HTTP endpoints on the Nomad agent,
  # including the API.
  http = false
  # Specifies if TLS should be enabled on the RPC endpoints and Raft traffic
  # between the Nomad servers. Enabling this on a Nomad client makes the client
  # use TLS for making RPC requests to the Nomad servers.
  rpc  = false
  # Specifies agents should require client certificates for all incoming HTTPS
  # requests. The client certificates must be signed by the same CA as Nomad.
  verify_https_client = false
  # Specifies if outgoing TLS connections should verify the server's hostname.
  verify_server_hostname = false
}

# Logging:
##########
# Specifies the verbosity of logs the Nomad agent will output. Valid log levels
# include WARN, INFO, or DEBUG in increasing order of verbosity.
log_level = "INFO"
# Output logs in a JSON format.
log_json = true
# Specifies the path for logging. If the path does not includes a filename, the
# filename defaults to nomad.log. This setting can be combined with
# "log_rotate_bytes" and "log_rotate_duration" for a fine-grained log rotation
# control.
log_file = "$dir/nomad/nomad.log"
# Specifies if the agent should log to syslog. This option only works on Unix
# based systems.
enable_syslog = false
# Specifies if the debugging HTTP endpoints should be enabled. These endpoints
# can be used with profiling tools to dump diagnostic information about Nomad's
# internals.
enable_debug = false

# Termination:
##############
# Specifies if the agent should gracefully leave when receiving the interrupt
# signal. By default, the agent will exit forcefully on any signal. This value
# should only be set to true on server agents if it is expected that a
# terminated server instance will never join the cluster again.
leave_on_interrupt = true
# Specifies if the agent should gracefully leave when receiving the terminate
# signal. By default, the agent will exit forcefully on any signal. This value
# should only be set to true on server agents if it is expected that a
# terminated server instance will never join the cluster again.
leave_on_terminate = true

# Server:
#########
# https://developer.hashicorp.com/nomad/docs/configuration/server
server {
  # Specifies if this agent should run in server mode. All other server options depend on this value being set.
  enabled = true
  # Specifies the directory to use for server-specific data, including the
  # replicated log. By default, this is the top-level "data_dir" suffixed with
  # "server", like "/opt/nomad/server". The top-level option must be set, even
  # when setting this value. This must be an absolute path.
  data_dir = "$dir/nomad/data/server"
  # Specifies the number of server nodes to wait for before bootstrapping. It is
  # most common to use the odd-numbered integers 3 or 5 for this value,
  # depending on the cluster size. A value of 1 does not provide any fault
  # tolerance and is not recommended for production use cases.
  bootstrap_expect = 1
  # Specifies how long a node must be in a terminal state before it is garbage
  # collected and purged from the system. This is specified using a label suffix
  # like "30s" or "1h".
  node_gc_threshold = "60s"
  # Specifies the interval between the job garbage collections. Only jobs who
  # have been terminal for at least job_gc_threshold will be collected. Lowering
  # the interval will perform more frequent but smaller collections. Raising the
  # interval will perform collections less frequently but collect more jobs at a
  # time. Reducing this interval is useful if there is a large throughput of
  # tasks, leading to a large set of dead jobs. This is specified using a label
  # suffix like "30s" or "3m". job_gc_interval was introduced in Nomad 0.10.0.
  job_gc_interval = "2s"
  # Specifies the minimum time a job must be in the terminal state before it is
  # eligible for garbage collection. This is specified using a label suffix like
  # "30s" or "1h".
  job_gc_threshold = "2s"
  # Specifies the minimum time an evaluation must be in the terminal state
  # before it is eligible for garbage collection. This is specified using a
  # label suffix like "30s" or "1h".
  eval_gc_threshold = "2s"
  # Specifies the minimum time a deployment must be in the terminal state before
  # it is eligible for garbage collection. This is specified using a label
  # suffix like "30s" or "1h".
  deployment_gc_threshold = "2s"
  # Specifies if Nomad will ignore a previous leave and attempt to rejoin the
  # cluster when starting. By default, Nomad treats leave as a permanent intent
  # and does not attempt to join the cluster again when starting. This flag
  # allows the previous state to be used to rejoin the cluster.
  rejoin_after_leave = false
}

# Client:
#########
# https://developer.hashicorp.com/nomad/docs/configuration/client
client {
  enabled = true
  # Specifies the directory to use for allocation data. By default, this is the
  # top-level data_dir suffixed with "alloc", like "/opt/nomad/alloc". This must
  # be an absolute path.
  alloc_dir = "$dir/nomad/data/alloc"
  # Specifies the directory to use to store client state. By default, this is
  # the top-level "data_dir" suffixed with "client", like "/opt/nomad/client".
  # This must be an absolute path.
  state_dir = "$dir/nomad/data/client"
  # Specifies an array of addresses to the Nomad servers this client should join.
  # This list is used to register the client with the server nodes and advertise
  # the available resources so that the agent can receive work. This may be
  # specified as an IP address or DNS, with or without the port. If the port is
  # omitted, the default port of 4647 is used.
  servers = [ "127.0.0.1:4647" ]
  # Specifies the maximum amount of time a job is allowed to wait to exit.
  # Individual jobs may customize their own kill timeout, but it may not exceed
  # this value.
  max_kill_timeout = "30s"
  # Specifies the interval at which Nomad attempts to garbage collect terminal
  # allocation directories.
  gc_interval = "2s"
}

# Plugins:
##########
# https://developer.hashicorp.com/nomad/plugins/drivers/podman#plugin-options
plugin "nomad-driver-podman" {
  args = []
  # https://github.com/hashicorp/nomad-driver-podman#driver-configuration
  config {
    # Defaults to "unix:///run/podman/podman.sock" when running as root or a
    # cgroup V1 system, and "unix:///run/user/<USER_ID>/podman/podman.sock" for
    # rootless cgroup V2 systems.
    socket_path = "unix://$podman_socket_path"
    # Allows tasks to bind host paths (volumes) inside their container.
    volumes {
      enabled = true
    }
    # This option can be used to disable Nomad from removing a container when
    # the task exits.
    gc {
      container = true
    }
    # Allows the driver to start and reuse a previously stopped container after
    # a Nomad client restart. Consider a simple single node system and a
    # complete reboot. All previously managed containers will be reused instead
    # of disposed and recreated.
    recover_stopped = false
    # Setting this to true will disable Nomad logs collection of Podman tasks.
    # If you don't rely on nomad log capabilities and exclusively use host based
    # log aggregation, you may consider this option to disable nomad log
    # collection overhead. Beware to you also loose automatic log rotation.
    disable_log_collection = false
  }
}

# Misc:
#######
# The vault stanza configures Nomad's integration with HashiCorp's Vault. When
# configured, Nomad can create and distribute Vault tokens to tasks
# automatically. For more information on the architecture and setup, please see
# the Nomad and Vault integration documentation.
vault {
  # Specifies if the Vault integration should be activated.
  enabled = false
}
# The acl stanza configures the Nomad agent to enable ACLs and tunes various ACL
# parameters. Learn more about configuring Nomad's ACL system in the Secure
# Nomad with Access Control guide.
acl {
  # Specifies if ACL enforcement is enabled. All other ACL configuration options
  # depend on this value. Note that the Nomad command line client will send
  # requests for client endpoints such as alloc exec directly to Nomad clients
  # whenever they are accessible. In this scenario, the client will enforce
  # ACLs, so both servers and clients should have ACLs enabled.
  enabled = false
}
# The audit stanza configures the Nomad agent to configure Audit logging
# behavior. Audit logging is an Enterprise-only feature.
audit {
  # Specifies if audit logging should be enabled. When enabled, audit logging
  # will occur for every request, unless it is filtered by a filter.
  enabled = true
}
# The consul stanza configures the Nomad agent's communication with Consul for
# service discovery and key-value integration. When configured, tasks can
# register themselves with Consul, and the Nomad cluster can automatically
# bootstrap itself.
consul {
}
# Specifies if Nomad should not check for updates and security bulletins. This
# defaults to true in Nomad Enterprise.
disable_update_check = true
EOF
}

# Need to use HCL instead of JSON. The only workaround is to send commands to
# Nomad using `curl` instead of the command line (`nomad job ...`).
# - "The nomad job run command currently accepts only HCL"
# [https://github.com/hashicorp/nomad/issues/6758#issuecomment-794116722]
nomad_create_job_file() {
    local dir=$1
    local profile_nomad_job_file=$(envjqr 'profile_nomad_job_file')
    local nomad_job_name=$(envjqr         'nomad_job_name')
    local nomad_job_group_name=$(envjqr   'nomad_job_group_name')
    local one_tracer_per_node=$(envjq     'one_tracer_per_node')
    cp $profile_nomad_job_file $dir/nomad/nomad-job.json
    chmod +w $dir/nomad/nomad-job.json
    # If CARDANO_MAINNET_MIRROR is present generate a list of needed volumes.
    if test -n "$CARDANO_MAINNET_MIRROR"
    then
      # The nix-store path contains 3 levels of symlinks. This is a hack to
      # avoid creating a container image with all these files.
      local immutable_store=$(readlink -f "$CARDANO_MAINNET_MIRROR"/immutable)
      local mainnet_mirror_volumes="[
          \"$CARDANO_MAINNET_MIRROR:$CARDANO_MAINNET_MIRROR:ro\"
        , \"$immutable_store:$immutable_store:ro\"
        $(find -L "$immutable_store" -type f -exec realpath {} \; | xargs dirname | sort | uniq | xargs -I "{}" echo ", \"{}:{}:ro\"")
      ]"
    else
      local mainnet_mirror_volumes="[]"
    fi
    # Hint:
    # - Working dir is: /tmp/cluster/
    # - Mount point is: /tmp/cluster/run/current
    local container_mountpoint=$(      envjqr 'container_mountpoint')
    # Nodes
    for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
    do
      local task_stanza_name="$node"
      # Every node needs access to "./genesis/" and tracer when only 1 is used.
      local jq_filter="
        [
            \"${dir}/genesis:${container_mountpoint}/genesis:ro\"
          , \"${dir}/genesis/utxo-keys:${container_mountpoint}/genesis/utxo-keys:ro\"
        ]
        +
        (
          if \$one_tracer_per_node == true
          then
            [ ]
          else
            [ \"${dir}/tracer:${container_mountpoint}/tracer:rw\" ]
          end
        )
        +
        \$mainnet_mirror_volumes
      "
      local podman_volumes=$(jq "$jq_filter" --argjson one_tracer_per_node "$one_tracer_per_node" --argjson mainnet_mirror_volumes "$mainnet_mirror_volumes" "$dir"/profile/node-specs.json)
      jq ".job[\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"task\"][\"$node\"][\"config\"][\"volumes\"] = \$podman_volumes" --argjson podman_volumes "$podman_volumes" $dir/nomad/nomad-job.json | sponge $dir/nomad/nomad-job.json
    done
    # Tracer
    if jqtest ".node.tracer" "$dir"/profile.json && ! test "$one_tracer_per_node" = "true"
    then
      local task_stanza_name_t="tracer"
      # Tracer only needs access to itself (its shared folder).
      local jq_filter_t="
        [
          \"${dir}/tracer:${container_mountpoint}/tracer:rw\"
        ]
      "
      local podman_volumes_t=$(jq "$jq_filter_t" "$dir"/profile/node-specs.json)
      jq ".job[\"$nomad_job_name\"][\"group\"][\"$nomad_job_group_name\"][\"task\"][\"tracer\"][\"config\"][\"volumes\"] = \$podman_volumes_t" --argjson podman_volumes_t "$podman_volumes_t" $dir/nomad/nomad-job.json | sponge $dir/nomad/nomad-job.json
    fi
}
