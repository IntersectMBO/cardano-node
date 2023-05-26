usage_nomadbackend() {
  usage "nomad" "Backend: manages a Cardano cluster using Nomad" <<EOF

    Please see documentation for 'wb backend' for the supported commands.

    - Nomad backend specific:

    $(blue Subcommands that need a RUN-DIR parameter:)

    $(helpcmd task-program-start      RUN-DIR NOMAD-TASK-NAME SUPERVISORCTL-PROGRAM)
    $(helpcmd task-program-stop       RUN-DIR NOMAD-TASK-NAME SUPERVISORCTL-PROGRAM)
    $(helpcmd is-task-program-running RUN-DIR NOMAD-TASK-NAME SUPERVISORCTL-PROGRAM)
    $(helpcmd task-supervisorctl      RUN-DIR NOMAD-TASK-NAME SUPERVISORCTL-ACTION [ARGS])

    $(blue Subcommands that don\'t need a RUN-DIR parameter:)

    $(red nomad $(helpcmd \(job\|agents\|server\|client\|plugin\|all\)))

EOF
}

backend_nomad() {

  op=${1:?$(usage_nomadbackend)}; shift

  case "$op" in

    ############################################################################
    # Configure cluster functions:
    # - setenv-defaults                        BACKEND-DIR
    # - setenv-nomad                           CONTAINER-SPECS-FILE (Nomad only)
    # - allocate-run                           RUN-DIR
    # - allocate-run-directory-nomad           RUN-DIR              (Nomad only)
    # - allocate-run-directory-supervisor      RUN-DIR              (Nomad only)
    # - allocate-run-directory-nodes           RUN-DIR              (Nomad only)
    # - allocate-run-directory-generator       RUN-DIR              (Nomad only)
    # - allocate-run-directory-tracers         RUN-DIR              (Nomad only)
    # - allocate-run-nomad-job-patch-name      RUN-DIR NAME         (Nomad only)
    # - allocate-run-nomad-job-patch-namespace RUN-DIR NAME         (Nomad only)
    # - allocate-run-nomad-job-patch-nix       RUN-DIR              (Nomad only)
    # - deploy-genesis                         RUN-DIR
    # - describe-run                           RUN-DIR
    ############################################################################
    # * Functions in the backend "interface" must use `fatal` when errors!

    # After `allocate-run` the Nomad is running waiting for the genesis to be
    # deployed and tracer/cardano-nodes/generator to be started.
    #
    # "generator", "tracer" and "node" folders contents (start.sh, config files,
    # etc) are included in the Nomad Job spec file as "template" stanzas and are
    # materialized inside the container when the job is started. This is how it
    # works for every environment combination (podman/exec-(local/cloud)).
    #
    # But "genesis" and "CARDANO_MAINNET_MIRROR" are the exceptions:
    # - "CARDANO_MAINNET_MIRROR": is added as a Nix dependency using the
    # `nix_installables` stanza when using the "exec" driver and is mounted as a
    # local volume for "podman" that currently is only allowed to run locally.
    # - "genesis": it's too big for a "template" stanza so we are mounting it
    # locally for "podman" and uploading it to a cloud storage to download using
    # "nomad exec" when the "exec" task driver is used, the latter means
    # creating an HTTP server for local runs and using Amazon S2 for cloud runs.
    allocate-run )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      while test $# -gt 0
      do case "$1" in
        --* ) msg "FATAL:  unknown flag '$1'"; usage_nomadbackend;;
          * ) break;; esac; shift; done

      # Create the dispatcher's local directories hierarchy.
      backend_nomad allocate-run-directory-nomad      "${dir}"
      backend_nomad allocate-run-directory-supervisor "${dir}"
      backend_nomad allocate-run-directory-nodes      "${dir}"
      backend_nomad allocate-run-directory-generator  "${dir}"
      backend_nomad allocate-run-directory-tracers    "${dir}"

      # These ones are decided at "setenv-defaults" of each sub-backend.
      local nomad_environment=$(envjqr 'nomad_environment')
      local nomad_task_driver=$(envjqr 'nomad_task_driver')
      # TODO: Store them on disk for later subcommands run from a different shell.
      # echo "{\"nomad_environment\": $nomad_environment, }" > "$dir"/env.json

      # Update the Nomad Job specs file accordingly
      ## - Job Name
      ### Must match `^[a-zA-Z0-9-]{1,128}$)` or it won't be possible to use it
      ### as namespace.: "invalid name "2023-02-10-06.34.f178b.ci-test-bage.nom"".
      local nomad_job_name=$(basename "${dir}")
      backend_nomad allocate-run-nomad-job-patch-name "${dir}" "${nomad_job_name}"

      backend_nomad start-nomad-job "${dir}"
    ;;

    allocate-run-directory-nomad )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_task_driver=$(envjqr   'nomad_task_driver')
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
      # Creates Nomad specific folders to download the entrypoints scripts and
      # its logs for every Nomad Task.
      # The top level "nomad" folder is created at "allocate-run" of each
      # sub-backend. ("podman", "exec", "cloud") and filled with the Nomad job
      # spec file to use.
      local nodes=($(jq_tolist keys "${dir}"/node-specs.json))
      for node in ${nodes[*]}
      do
        mkdir "${dir}"/nomad/"${node}"
      done
      if jqtest ".node.tracer" "${dir}"/profile.json
      then
        if ! test "${one_tracer_per_node}" = "true"
        then
          mkdir "${dir}"/nomad/tracer
        fi
      fi
    ;;

    allocate-run-directory-supervisor )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
      # One `supervisord` server is run for every Nomad task/container.
      mkdir -p "${dir}"/supervisor
      # The folders were the `supervisord` config and log files will be downloaded!
      local nodes=($(jq_tolist keys "${dir}"/node-specs.json))
      for node in ${nodes[*]}
      do
        mkdir "${dir}"/supervisor/"${node}"
      done
      # A "tracer"(s) is optional.
      if jqtest ".node.tracer" "${dir}"/profile.json
      then
        # "tracer" is run as a standalone Nomad Task ? Or part of another Task ?
        if ! test "${one_tracer_per_node}" = "true"
        then
          mkdir "${dir}"/supervisor/tracer
        fi
      fi
    ;;

    allocate-run-directory-nodes )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      # For every node ...
      local nodes=($(jq_tolist keys "${dir}"/node-specs.json))
      for node in ${nodes[*]}
      do
        # Files "start.sh" and "topology.sh" that usually go in here are copied
        # from the Task/container once it's started because the contents are
        # created or patched using Nomad's "template" stanza in the job spec
        # and we want to hold a copy of what was actually run.
        mkdir "${dir}"/"${node}"
      done
    ;;

    allocate-run-directory-generator )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      # Generator always runs inside Task/container "node-0". Not much to do!
      mkdir -p "${dir}"/generator
    ;;

    allocate-run-directory-tracers )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_task_driver=$(envjqr   'nomad_task_driver')
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
      # A "tracer"(s) is optional.
      if jqtest ".node.tracer" "${dir}"/profile.json
      then
        # Create tracer(s) director(y/ies).
        mkdir -p "${dir}"/tracer
        # If running "local" without "one_tracer_per_node" this directory will
        # be populated using the workbench!
        # Right now where are forcing "one_tracer_per_node=true" with "exec"
        # For podman, than only runs local, this tracer directory is populated
        # and mounted to every container when only one shared tracer is used.
        if test "${one_tracer_per_node}" = "true"
        then
          local nodes=($(jq_tolist keys "${dir}"/node-specs.json))
          for node in ${nodes[*]}
          do
            mkdir -p "${dir}"/tracer/"${node}"
          done
        elif test "${nomad_task_driver}" = "podman"
        then
          # FIXME: Looks like I'm not using these ones!!!
         #cp $(jq '."tracer-config"'  -r ${dir}/profile/tracer-service.json) "${dir}"/tracer/tracer-config.json
         #cp $(jq '."service-config"' -r ${dir}/profile/tracer-service.json) "${dir}"/tracer/service-config.json
          cp $(jq '."config"'         -r ${dir}/profile/tracer-service.json) "${dir}"/tracer/config.json
          cp $(jq '."start"'          -r ${dir}/profile/tracer-service.json) "${dir}"/tracer/start.sh
        fi
      fi
    ;;

    # Change the Nomad job name to the current run tag. This allows to run
    # multiple clusters simulatenously (as long as the network isolation mode
    # and/or topology.json allows no port clashing)
    allocate-run-nomad-job-patch-name )
      local usage="USAGE: wb backend $op RUN-DIR JOB-NAME"
      local dir=${1:?$usage};      shift
      local new_name=${1:?$usage}; shift
      local old_name=$(jq -r '. ["job"] | keys[0]' "${dir}"/nomad/nomad-job.json)
      jq ".[\"job\"][\"${new_name}\"] = .[\"job\"][\"${old_name}\"] | del(.[\"job\"][\"${old_name}\"])" "${dir}"/nomad/nomad-job.json | sponge "${dir}"/nomad/nomad-job.json
    ;;

    allocate-run-nomad-job-patch-namespace )
      local usage="USAGE: wb backend $op RUN-DIR NAMESPACE"
      local dir=${1:?$usage};       shift
      local namespace=${1:?$usage}; shift
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
      msg "Setting Nomad job namespace to \"${namespace}\""
      jq ".job[\"${nomad_job_name}\"][\"namespace\"] = \"${namespace}\"" "${dir}"/nomad/nomad-job.json | sponge "${dir}"/nomad/nomad-job.json
    ;;

    allocate-run-nomad-job-patch-nix )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_environment=$(envjqr 'nomad_environment')
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
      # When running local, we can take some shortcuts!
      local installables_array
      if test "${nomad_environment}" != "cloud"
      then
        installables_array=$(jq '.containerPkgs | map(."nix-store-path")' "${dir}"/container-specs.json)
      else
        installables_array=$(jq '.containerPkgs | map(.installable)' "${dir}"/container-specs.json)
      fi
      # nix_installables
      local groups_array=$(jq -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"] | keys | join (\" \")" "${dir}"/nomad/nomad-job.json)
      for group_name in ${groups_array[*]}
      do
        local tasks_array=$(jq -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"task\"] | keys | join (\" \")" "${dir}"/nomad/nomad-job.json)
        for task_name in ${tasks_array[*]}
        do
          jq ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"task\"][\"${task_name}\"][\"config\"][\"nix_installables\"] = \$installables_array" --argjson installables_array "${installables_array}" "${dir}"/nomad/nomad-job.json | sponge "${dir}"/nomad/nomad-job.json
        done
      done
    ;;

    allocate-run-nomad-job-patch-group-constraints )
      local usage="USAGE: wb backend $op RUN-DIR CONSTRAINTS-JSON-ARRAY"
      local dir=${1:?$usage}; shift
      local constraints_array=${1:?$usage}; shift
      local nomad_environment=$(envjqr 'nomad_environment')
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
      jq ".[\"job\"][\"${nomad_job_name}\"][\"group\"] |= with_entries(.value.constraint = \$constraints_array)" --argjson constraints_array "${constraints_array}" "${dir}"/nomad/nomad-job.json | sponge "${dir}"/nomad/nomad-job.json
    ;;

    # Called by the sub-backends, don't use `fatal` and let them do the cleaning
    deploy-genesis-wget )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local uri=${1:?$usage}; shift
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)

      # Upload and unpack all "genesis" files in parallel!
      local nodes=($(jq_tolist keys "$dir"/node-specs.json))
      local wget_path="$(jq -r ".containerPkgs.wget.\"nix-store-path\"" "${dir}"/container-specs.json)"
      local uploads_array=()
      for node in ${nodes[*]}
      do
        msg "$(blue Downloading) $(yellow "\"${uri}\"") from $(yellow "node \"${node}\"") ..."
        backend_nomad task-exec "${dir}" "${node}"               \
          "${wget_path}"/bin/wget                                \
            --output-document=/local/run/current/genesis.tar.zst \
            "${uri}"                                             \
            --no-verbose                                         \
        > /dev/null                                              \
        &
        uploads_array+=("$!")
      done
      # Wait and check!
      if test -n "${uploads_array}"
      then
        if ! wait_fail_any "${uploads_array[@]}"
        then
          msg "$(red "Failed to upload some genesis files")"
          return 1
        else
          # Unpack!
          local coreutils_path="$(jq -r ".containerPkgs.coreutils.\"nix-store-path\"" "${dir}"/container-specs.json)"
          local tar_path="$(jq -r ".containerPkgs.gnutar.\"nix-store-path\"" "${dir}"/container-specs.json)"
          local zstd_path="$(jq -r ".containerPkgs.zstd.\"nix-store-path\"" "${dir}"/container-specs.json)"
          local unpacks_array=()
          for node in ${nodes[*]}
          do
            backend_nomad task-exec "${dir}" "${node}"         \
              ${tar_path}/bin/tar --extract                    \
                --use-compress-program="${zstd_path}"/bin/zstd \
                --file=/local/run/current/genesis.tar.zst      \
                --one-top-level=/local/run/current/genesis     \
                --same-permissions                             \
                --no-same-owner                                \
                --numeric-owner                                \
                --owner="$(${coreutils_path}/bin/id --user)"   \
                --group="$(${coreutils_path}/bin/id --group)"  \
            &
            unpacks_array+=("$!")
          done
          # Wait and check!
          if test -n "${unpacks_array}"
          then
            if ! wait_fail_any "${unpacks_array[@]}"
            then
              msg "$(red "Failed to unpack some genesis files")"
              return 1
            fi
          fi
        fi
      fi
    ;;

    describe-run )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}

      echo "  - Nomad job: $(realpath ${dir})/nomad/nomad-job.json"
    ;;

    ############################################################################
    # Start/stop Nomad job functions:
    # - start-nomad-job RUN-DIR                                     (Nomad only)
    # - stop-nomad-job  RUN-DIR                                     (Nomad only)
    ############################################################################
    # * Functions in the backend "interface" must use `fatal` when errors!

    start-nomad-job )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      backend_nomad nomad-agents-topology "${dir}"

      local nomad_environment=$(envjqr 'nomad_environment')
      local nomad_task_driver=$(envjqr 'nomad_task_driver')
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
      local server_name=$(envjqr 'nomad_server_name')
      local client_name=$(envjqr 'nomad_client_name')
      local server_state_dir=$(wb_nomad server state-dir-path "${server_name}")
      local client_state_dir=$(wb_nomad client state-dir-path "${client_name}")
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "$dir"/nomad/nomad-job.json)

      # Reuse an already running cardano-workbench Nomad server!
      local nomad_agents_were_already_running
      if test "${nomad_environment}" = "cloud"
      then
        nomad_agents_were_already_running="true"
      else
        if wb_nomad server is-running "${server_name}" && wb_nomad client is-running "${client_name}"
        then
          # TODO/ENHANCE: Check matching needed capabilities before reuse?
          nomad_agents_were_already_running="true"
          setenvjqstr 'nomad_agents_were_already_running' "true"
          msg "$(yellow "Reusing already up and running Nomad agents (server and client found)")"
        else
          nomad_agents_were_already_running="false"
          setenvjqstr 'nomad_agents_were_already_running' "false"
          # Start server, client and plugins.
          wb_nomad agents start \
            "${server_name}" "${client_name}" "${nomad_task_driver}"
        fi
      fi

      # TODO: Genesis when "cloud"!
      if test "${nomad_environment}" != "cloud"
      then
        # Links to Nomad agents logs.
        ln -s "${server_state_dir}"/nomad.log "${dir}"/nomad/server-"${server_name}".log
        ln -s "${server_state_dir}"/stdout    "${dir}"/nomad/server-"${server_name}".stdout
        ln -s "${server_state_dir}"/stderr    "${dir}"/nomad/server-"${server_name}".stderr
        ln -s "${client_state_dir}"/nomad.log "${dir}"/nomad/client-"${client_name}".log
        ln -s "${client_state_dir}"/stdout    "${dir}"/nomad/client-"${client_name}".stdout
        ln -s "${client_state_dir}"/stderr    "${dir}"/nomad/client-"${client_name}".stderr
      fi

      msg "$(blue Starting) Nomad $(yellow "Job \"${nomad_job_name}\"")..."
      if ! wb_nomad job start "$dir/nomad/nomad-job.json" "${nomad_job_name}"
      then
        if test "${nomad_agents_were_already_running}" = "false"
        then
          wb_nomad agents stop \
            "${server_name}" "${client_name}" "${nomad_task_driver}"
        fi
        fatal "Failed to start Nomad job"
      else
        touch "${dir}"/nomad/started
      fi

      # Create a symlink to the allocations IDs inside the run directory and
      # the supervisord log files.
      if test "${nomad_environment}" != "cloud"
      then
        mkdir "${dir}"/nomad/alloc
        # For each node's Nomad Task
        local nodes=($(jq_tolist keys "$dir"/node-specs.json))
        for node in ${nodes[*]}
        do
          local alloc_id alloc_dir
          alloc_id=$(wb_nomad job task-name-allocation-id \
            "${dir}/nomad/nomad-job.json"                            \
            "${node}")
          alloc_dir="${client_state_dir}"/data/alloc/"${alloc_id}"/"${node}"
          ln -s "${alloc_dir}" "${dir}"/nomad/alloc/"${node}"
          ln -s                                                                      \
            ../../nomad/alloc/"${node}"/local/run/current/supervisor/supervisord.log \
            "${dir}"/supervisor/"${node}"/supervisord.log
        done
        # For the tracer's Nomad Task if any
        if test "${one_tracer_per_node}" != "true"
        then
          local alloc_id alloc_dir
          alloc_id=$(wb_nomad job task-name-allocation-id \
            "${dir}/nomad/nomad-job.json"                            \
            tracer)
          alloc_dir="${client_state_dir}"/data/alloc/"${alloc_id}"/tracer
          ln -s "${alloc_dir}" "${dir}"/nomad/alloc/tracer
          ln -s                                                                   \
            ../../nomad/alloc/tracer/local/run/current/supervisor/supervisord.log \
            "${dir}"/supervisor/tracer/supervisord.log
        fi
      fi

      # TODO: Send the download everything job to the background!
      backend_nomad start-download "$dir"
    ;;

    # Download all Nomad dynamically generated files, the "template" stanzas.
    start-download )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')

      msg "Fetch Nomad generated files ..."
      local jobs_array=()
      # Only used for debugging!
      backend_nomad download-config-generator "${dir}" &
      jobs_array+=("$!")
      # For every node ...
      local nodes=($(jq_tolist keys "$dir"/node-specs.json))
      for node in ${nodes[*]}
      do
        # Only used for debugging!
        backend_nomad download-config-node "${dir}" "${node}" &
        jobs_array+=("$!")
      done
      # This same script looks for the socket path inside the tracer config
      if test "${one_tracer_per_node}" = "true"
      then
        local nodes=($(jq_tolist keys "$dir"/node-specs.json))
        for node in ${nodes[*]}
        do
          backend_nomad download-config-tracer "${dir}" "${node}" &
          jobs_array+=("$!")
        done
      else
        backend_nomad download-config-tracer   "${dir}" "tracer" &
        jobs_array+=("$!")
      fi

      # Wait and check!
      if test -n "${jobs_array}"
      then
        if ! wait_fail_any "${jobs_array[@]}"
        then
          backend_nomad stop-nomad-job "${dir}"
          fatal "Downloads failed!"
        else
          msg "Finished fetching Nomad generated files"
        fi
      fi
    ;;

    nomad-agents-topology )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_task_driver=$(envjqr 'nomad_task_driver')
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
      # The server
      echo "{                                ">> "${dir}"/nomad/agents-topolgy.json
      echo "    \"servers\": {               ">> "${dir}"/nomad/agents-topolgy.json
      echo "      \"srv1\": {                ">> "${dir}"/nomad/agents-topolgy.json
      echo "          \"region\": \"r1\"     ">> "${dir}"/nomad/agents-topolgy.json
      echo "        , \"datacenter\": \"d1\" ">> "${dir}"/nomad/agents-topolgy.json
      echo "        , \"ports\": {           ">> "${dir}"/nomad/agents-topolgy.json
      echo "              \"http\": 4646     ">> "${dir}"/nomad/agents-topolgy.json
      echo "            , \"rpc\":  4647     ">> "${dir}"/nomad/agents-topolgy.json
      echo "            , \"serf\": 4648     ">> "${dir}"/nomad/agents-topolgy.json
      echo "          }                      ">> "${dir}"/nomad/agents-topolgy.json
      echo "      }                          ">> "${dir}"/nomad/agents-topolgy.json
      echo "    }                            ">> "${dir}"/nomad/agents-topolgy.json
      echo "  , \"clients\": {               ">> "${dir}"/nomad/agents-topolgy.json
      # Task driver based suffix for clients (exec clients run with `sudo`)
      local suffix
      if test "${nomad_task_driver}" = "podman"
      then
        suffix="pod"
      else
        suffix="exe"
      fi
      local i=0 c=""
      local groups_array=$(jq -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"] | keys | join (\" \")" "${dir}"/nomad/nomad-job.json)
      for group_name in ${groups_array[*]}
      do
        local client_name=("cli${group_name}-${suffix}")
      echo " ${c} \"${client_name}\": {      ">> "${dir}"/nomad/agents-topolgy.json
      echo "          \"region\": \"r1\"     ">> "${dir}"/nomad/agents-topolgy.json
      echo "        , \"datacenter\": \"d1\" ">> "${dir}"/nomad/agents-topolgy.json
        i=$(( "${i}" + 1 ))
      echo "        , \"ports\": {           ">> "${dir}"/nomad/agents-topolgy.json
      echo "              \"http\": ${i}4646 ">> "${dir}"/nomad/agents-topolgy.json
      echo "            , \"rpc\":  ${i}4647 ">> "${dir}"/nomad/agents-topolgy.json
      echo "            , \"serf\": ${i}4648 ">> "${dir}"/nomad/agents-topolgy.json
      echo "          }                      ">> "${dir}"/nomad/agents-topolgy.json
      echo "        , \"servers\": [         ">> "${dir}"/nomad/agents-topolgy.json
      echo "            \"srv1\"             ">> "${dir}"/nomad/agents-topolgy.json
      echo "          ]                      ">> "${dir}"/nomad/agents-topolgy.json
      echo "      }                          ">> "${dir}"/nomad/agents-topolgy.json
        c=","
      done
      echo "    }                            ">> "${dir}"/nomad/agents-topolgy.json
      echo "}                                ">> "${dir}"/nomad/agents-topolgy.json

      # TODO: Once we start testing multicluster deployments, the Nomad cluster
      # must be created or obtained.
      setenvjq 'nomad_topology' \
        "{ \
            \"servers\": { \
              \"srv1\": { \
                 \"region\": \"r1\" \
               , \"datacenter\": \"dc1\" \
               , \"ports\": { \
                   \"http\": 4646 \
                 , \"rpc\":  4647 \
                 , \"serf\": 4648 \
                } \
              } \
            } \
          , \"clients\": { \
              \"cli1\": { \
                 \"region\": \"r22\" \
               , \"datacenter\": \"dc0\" \
               , \"ports\":{ \
                     \"http\": 14646 \
                   , \"rpc\":  14647 \
                 } \
               , \"servers\": [ \"srv1\" ] \
              } \
            } \
         }"
    ;;

    stop-nomad-job )
      local dir=${1:?$usage}; shift
      local nomad_task_driver=$(envjqr                 'nomad_task_driver')
      local server_name=$(envjqr                       'nomad_server_name')
      local client_name=$(envjqr                       'nomad_client_name')
      local nomad_agents_were_already_running=$(envjqr 'nomad_agents_were_already_running')
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
      touch "${dir}"/nomad/stopped
      wb_nomad job stop "${dir}/nomad/nomad-job.json" "${nomad_job_name}" > "${dir}/nomad/job.stop.stdout" 2> "$dir/nomad/job.stop.stderr" || true
      if test "${nomad_agents_were_already_running}" = "false"
      then
        wb_nomad agents stop \
          "${server_name}" "${client_name}" "${nomad_task_driver}"
      fi
    ;;

    ############################################################################
    # Start/stop Cardano cluster functions:
    # - is-running      RUN-DIR
    # - start           RUN-DIR
    # - stop-cluster    RUN-DIR
    # - cleanup-cluster RUN-DIR
    ############################################################################
    # * Functions in the backend "interface" must use `fatal` when errors!

    is-running )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      ## Our contract is to print all running component names.
      ##
      if test -f "${dir}"/nomad/nomad-job.json
      then
        local nomad_job_name=$(jq -r ". [\"job\"]? | keys[0]?" "${dir}"/nomad/nomad-job.json)
        if test -z "${nomad_job_name}" && test "${nomad_job_name}" != "null"
        then
          if nomad job status >/dev/null 2>&1
          then
            for allocation_file in "${dir}"/"${nomad_job_name}".run/allocation.*.final; do
              local allocation_id=$(jq .ID "${allocation_file}")
              local alloc_response=$(nomad alloc status -json "${allocation_id}")
              # With only one allocation running we return true (zero return code)
              if test $(echo "${alloc_status}" | jq .State) = "running"
              then echo "nomad-allocation-$allocation_id"
              fi
            done
          else
            msg "Can't access the Nomad server, assuming not running"
            msg "(Remember: assumption is the mother/father of all f*** ups!)"
          fi
          # Use the hack for the local, non-cloud, cluster?
          # Hack: Look for node-0's default port!
          # test "$(sleep 0.5s; netstat -pltn 2>/dev/null | grep ':30000 ' | wc -l)" != "0"
        fi
      fi
    ;;

    # All or clean up everything!
    # Called by `scenario.sh` without exit trap (`scenario_setup_exit_trap`)!
    start )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      touch "${dir}"/starting
      # Start tracer(s).
      if jqtest ".node.tracer" "${dir}"/profile.json
      then
        if ! backend_nomad start-tracers "${dir}"
        then
          backend_nomad stop-nomad-job "${dir}"
          fatal "Backend start failed!"
        fi
      fi
      rm "${dir}"/starting; touch "${dir}"/started
    ;;

    # All or clean up everything!
    # Called by `scenario.sh` without exit trap (`scenario_setup_exit_trap`)!
    stop-cluster )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_environment=$(envjqr 'nomad_environment')
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)

      # Concurrent thread may fail and trigger a `stop-cluster` simultaneously
      if ! test -f "${dir}"/started || test -f "${dir}"/stopping || test -f "${dir}"/stopped
      then
        return 0
      else
        touch "${dir}"/stopping
      fi

      # Stop generator.
      #################
      backend_nomad stop-cluster-generator "${dir}" "node-0"
      # Stop node(s).
      ###############
      local jobs_nodes_array=()
      for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
      do
        backend_nomad stop-cluster-node "${dir}" "${node}" &
        jobs_nodes_array+=("$!")
      done
      if ! wait_fail_any "${jobs_nodes_array[@]}"
      then
        fatal "Failed to stop node(s)"
      fi
      # Stop tracer(s).
      #################
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
      if test "${one_tracer_per_node}" = "true"
      then
        local jobs_tracers_array=()
        local nodes=($(jq_tolist keys "${dir}"/node-specs.json))
        for node in ${nodes[*]}
        do
          backend_nomad stop-cluster-tracer "${dir}" "${node}" &
          jobs_tracers_array+=("$!")
        done
        if ! wait_fail_any "${jobs_tracers_array[@]}"
        then
          fatal "Failed to stop tracer(s)"
        fi
      else
        backend_nomad stop-cluster-node "${dir}" "tracer"
      fi

      # Download logs!
      backend_nomad stop-cluster-download "${dir}"

      msg "$(blue Stopping) Nomad $(yellow "Job \"${nomad_job_name}\"")..."
      # TODO: Show output or do something if it fails?
      wb_nomad job stop "${dir}/nomad/nomad-job.json" "${nomad_job_name}" > "$dir/nomad/job.stop.stdout" 2> "$dir/nomad/job.stop.stderr" || true

      local nomad_agents_were_already_running=$(envjqr 'nomad_agents_were_already_running')
      if test "$nomad_agents_were_already_running" = "false"
      then
        local nomad_server_name=$(envjqr 'nomad_server_name')
        local nomad_client_name=$(envjqr 'nomad_client_name')
        local nomad_task_driver=$(envjqr 'nomad_task_driver')
        wb_nomad agents stop \
          "${nomad_server_name}" "${nomad_client_name}" "${nomad_task_driver}"
      fi

      # TODO: Always stop it? It's not always started!
      #wb_nomad webfs stop || true

      local oci_image_was_already_available=$(envjqr 'oci_image_was_already_available')
      #TODO: Remove it?

      rm "${dir}"/stopping; touch "${dir}"/stopped
    ;;

    stop-cluster-generator )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      if test -f "${dir}"/generator/started && !(test -f "${dir}"/"${node}"/stopped || test -f "${dir}"/"${node}"/quit)
      then
        if backend_nomad is-task-program-running "${dir}" "${task}" generator
        then
          if ! backend_nomad task-program-stop "${dir}" "${task}" generator
          then
            # A race condition were we try to stop the generator just after it
            # quits automatically can happen.
            msg "$(yellow "WARNING: Program \"generator\" inside Task \"${task}\" failed to stop")"
          else
            touch "${dir}"/generator/stopped
            msg "$(green "supervisord program \"generator\" inside Nomad Task \"${task}\" down!")"
          fi
        else
          touch "${dir}"/generator/quit
          if backend_nomad is-task-program-failed "${dir}" "${task}" generator
          then
            # If the node quits (due to `--shutdown_on_slot_synced X` or
            # `--shutdown_on_block_synced X`) the generator quits with an error.
            local generator_can_fail=$(jq ".\"${task}\".shutdown_on_slot_synced or .\"${task}\".shutdown_on_block_synced" "${dir}"/node-specs.json)
            if test "${generator_can_fail}" = "false" || backend_nomad is-task-program-running "${dir}" "${task}" "${task}"
            then
              # Do not fail here, because nobody will be able to stop the cluster!
              msg "$(red "FATAL: \"generator\" quit unexpectedly")"
            else
              msg "$(yellow "INFO: Program \"generator\" inside Task \"${task}\" failed, but expected when \"${task}\" automatically exits first")"
            fi
          else
            msg "$(yellow "WARNING: Program \"generator\" inside Task \"${task}\" was not running, should it?")"
          fi
        fi
      fi
    ;;

    stop-cluster-node )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local node=${1:?$usage}; shift
      if test -f "${dir}"/"${node}"/started && !(test -f "${dir}"/"${node}"/stopped || test -f "${dir}"/"${node}"/quit)
      then
        if backend_nomad is-task-program-running "${dir}" "${node}" "${node}"
        then
          # The `|| true` is to avoid a race condition were we try to stop
          # the node just after it quits automatically.
          if ! backend_nomad task-program-stop "${dir}" "${node}" "${node}"
          then
            msg "$(yellow "WARNING: Program \"${node}\" inside Task \"${node}\" failed to stop")"
          else
            touch "${dir}"/"${node}"/stopped
            msg "$(green "supervisord program \"${node}\" inside Nomad Task \"${node}\" down!")"
          fi
        else
          touch "${dir}"/"${node}"/quit
          # Node may have already quit (due to --shutdown_on_slot_synced X or
          # --shutdown_on_block_synced X).
          local node_can_quit=$(jq ".\"${node}\".shutdown_on_slot_synced or .\"${node}\".shutdown_on_block_synced" "${dir}"/node-specs.json)
          if test "${node_can_quit}" = "false"
          then
            # Do not fail here, because nobody will be able to stop the cluster!
            msg "$(red "FATAL: \"${node}\" quit unexpectedly")"
          else
            msg "$(yellow "WARNING: Program \"${node}\" inside Task \"${node}\" was not running, should it?")"
          fi
        fi
      fi
    ;;

    stop-cluster-tracer )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local task_dir
      if test "${task}" = "tracer"
      then
        task_dir="${dir}"/tracer
      else
        task_dir="${dir}"/tracer/"${task}"
      fi
      if test -f "${task_dir}"/started && !(test -f "${task_dir}"/stopped || test -f "${task_dir}"/quit)
      then
        # Tracers that receive connections should never quit by itself.
        if backend_nomad is-task-program-running "${dir}" "${task}" tracer
        then
          if ! backend_nomad task-program-stop "${dir}" "${task}" tracer
          then
            msg "$(yellow "WARNING: Program \"tracer\" inside Task \"${task}\" failed to stop")"
          else
            touch "${task_dir}"/stopped
            msg "$(green "supervisord program \"tracer\" inside Nomad Task \"${task}\" down!")"
          fi
        else
          touch "${task_dir}"/quit
          msg "$(red "FATAL: \"${task}\"'s \"tracer\" quit unexpectedly")"
        fi
      fi
    ;;

    stop-cluster-download )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_environment=$(envjqr 'nomad_environment')
      local nomad_task_driver=$(envjqr   'nomad_task_driver')
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')

      # TODO: Make it in parallel ?
      msg "Fetch logs ..."

      # Download generator logs.
      ##########################
      # Remove "live" symlinks before downloading the "originals"
      if test "${nomad_environment}" != "cloud"
      then
        rm -f "${dir}"/generator/{stdout,stderr,exit_code}
        rm -f "${dir}"/supervisor/node-0/supervisord.log
      fi
      # Download retry "infinite" loop.
      while ! backend_nomad download-logs-generator "${dir}" "node-0"
      do
        msg "Retrying \"generator\" logs download"
      done
      msg "$(green "Finished downloading \"generator\" logs")"
      # Download node(s) logs.
      ########################
      # Remove "live" symlinks before downloading the "originals"
      if test "${nomad_environment}" != "cloud"
      then
        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do
          rm -f "${dir}"/"${node}"/{stdout,stderr,exit_code}
          rm -f "${dir}"/nomad/"${node}"/{stdout,stderr}
          rm -f "${dir}"/supervisor/"${node}"/supervisord.log
        done
      fi
      # Download retry "infinite" loop.
      local nodes_array
      nodes_array="$(jq_tolist 'keys' "$dir"/node-specs.json)"
      while test -n "${nodes_array:-}"
      do
        local nodes_jobs_array=()
        for node in ${nodes_array[*]}
        do
          backend_nomad download-logs-node "${dir}" "${node}" &
          nodes_jobs_array+=("$!")
        done
        if test -n "${nodes_jobs_array:-}" # If = () "unbound variable" error
        then
          # Wait until all jobs finish, don't use `wait_fail_any` that kills
          # Returns the exit code of the last job, ignore it!
          wait "${nodes_jobs_array[@]}"  || true
        fi
        # Fetch the nodes that don't have all the log files in its directory
        nodes_array="$(backend_nomad stop-cluster-download-nodes "${dir}")"
        if test -n "${nodes_array:-}"
        then
          msg "Retrying node(s) [${nodes_array[@]}] logs download"
        fi
      done
      msg "$(green "Finished downloading node(s) logs")"
      # Download tracer(s) logs.
      ##########################
      if jqtest ".node.tracer" "${dir}"/profile.json
      then
        # Remove "live" symlinks before downloading the "originals"
        if test "${nomad_environment}" != "cloud"
        then
          if test "${one_tracer_per_node}" = "true"
          then
            for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
            do
              rm -f "${dir}"/tracer/"${node}"/{stdout,stderr,exit_code}
            done
          else
            # When "local" and "podman" "tracer" folder is mounted
            if ! test "${nomad_task_driver}" = "podman"
            then
              rm -f "${dir}"/tracer/{stdout,stderr,exit_code}
            fi
            rm -f "${dir}"/supervisor/tracer/supervisord.log
          fi
        fi
        # Download retry "infinite" loop.
        if test "${one_tracer_per_node}" = "true"
        then
          local tracers_array
          tracers_array="$(jq_tolist 'keys' "$dir"/node-specs.json)"
          while test -n "${tracers_array:-}"
          do
            local tracers_jobs_array=()
            for node in ${tracers_array[*]}
            do
              backend_nomad download-logs-tracer "${dir}" "${node}" &
              tracers_jobs_array+=("$!")
            done
            if test -n "${tracers_jobs_array:-}" # If = () "unbound variable" error
            then
              # Wait until all jobs finish, don't use `wait_fail_any` that kills
              # Returns the exit code of the last job, ignore it!
              wait "${tracers_jobs_array[@]}" || true
            fi
            # Fetch the nodes that don't have all the log files in its directory
            tracers_array="$(backend_nomad stop-cluster-download-nodes "${dir}")"
            if test -n "${tracers_array:-}"
            then
              msg "Retrying tracer(s) [${tracers_array[@]}] logs download"
            fi
          done
          msg "$(green "Finished downloading tracer(s) logs")"
          # TODO: These files are needed at all?
          if test -n "${NOMAD_DEBUG:-}"
          then
            for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
            do
              if ! backend_nomad download-zstd-tracer-logRoot "${dir}" "${node}"
              then
                msg "$(red "Failed to download \"tracer\" logRoot from \"${node}\"")"
              fi
            done
          fi
        else
          while ! backend_nomad download-logs-tracer "${dir}" "tracer"
          do
            msg "Retrying \"tracer\" logs download from \"tracer\""
          done
          msg "$(green "Finished downloading \"tracer\" logs from \"tracer\"")"
          # TODO: These files are needed at all?
          if test -n "${NOMAD_DEBUG:-}"
          then
            if ! backend_nomad download-zstd-tracer-logRoot "${dir}" "tracer"
            then
              msg "$(red "Failed to download \"tracer\" logRoot from \"tracer\"")"
            fi
          fi
        fi
      fi

      # TODO: Check downloads
      # ls run/current/nomad/{node-{0..51},explorer}/{stdout,stderr}            || msg ""
      # ls run/current/{node-{0..51},explorer}/{exit_code,stdout,stderr}        || msg ""
      # ls run/current/{node-{0..51},explorer}/{exit_code,stdout,stderr}        || msg ""
      # ls run/current/tracer/{node-{0..51},explorer}/{exit_code,stdout,stderr} || msg ""

      msg "$(green "Finished fetching logs")"
    ;;

    # Array of nodes that don't have all the log files in its directory
    stop-cluster-download-nodes )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nodes_array=()
      for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
      do
        local node_ok="true"
        # Check the existance of all the wanted files:
        if ! test -f "${dir}"/"${node}"/exit_code
        then
          node_ok="false"
        fi
        if ! test -f "${dir}"/"${node}"/stdout
        then
          node_ok="false"
        fi
        if ! test -f "${dir}"/"${node}"/stderr
        then
          node_ok="false"
        fi
        if ! test -f "${dir}"/nomad/"${node}"/stdout
        then
          node_ok="false"
        fi
        if ! test -f "${dir}"/nomad/"${node}"/stderr
        then
          node_ok="false"
        fi
        if ! test -f "${dir}"/supervisor/"${node}"/supervisord.log
        then
          node_ok="false"
        fi
        # Below like errors can end in truncated files, a proper flag is used!
        # failed to exec into task: read tcp 10.0.0.115:33840->3.72.231.105:443: read: connection reset by peer
        # tar: Unexpected EOF in archive
        # tar: Unexpected EOF in archive
        # tar: Error is not recoverable: exiting now
        if test -f "${dir}"/nomad/"${node}"/download_failed
        then
          node_ok="false"
        fi
        # If any error add this node to the array
        if test "${node_ok}" = "false"
        then
          nodes_array+=("${node}")
        fi
      done
      # Return array
      echo "${nodes_array[@]}"
    ;;

    # Only to be called with one_tracer_per_node = true
    # Array of tracers' nodes that don't have all the log files in its directory
    stop-cluster-download-tracers )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local tracers_array=()
      for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
      do
        local tracer_ok="true"
        # Check the existance of all the wanted files:
        if ! test -f "${dir}"/tracer/"${node}"/exit_code
        then
          tracer_ok="false"
        fi
        if ! test -f "${dir}"/tracer/"${node}"/stdout
        then
          tracer_ok="false"
        fi
        if ! test -f "${dir}"/tracer/"${node}"/stderr
        then
          tracer_ok="false"
        fi
        # Below like errors can end in truncated files, a proper flag is used!
        # failed to exec into task: read tcp 10.0.0.115:33840->3.72.231.105:443: read: connection reset by peer
        # tar: Unexpected EOF in archive
        # tar: Unexpected EOF in archive
        # tar: Error is not recoverable: exiting now
        if test -f "${dir}"/nomad/"${node}"/download_failed
        then
          tracer_ok="false"
        fi
        # If any error add this node to the array
        if test "${tracer_ok}" = "false"
        then
          tracers_array+=("${node}")
        fi
      done
      # Return array
      echo "${tracers_array[@]}"
    ;;

    cleanup-cluster )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      msg "nomad:  resetting cluster state in:  ${dir}"
      # Generic stuff
      rm -f  "${dir}"/*/std{out,err}  "${dir}"/*/exit_code 2>/dev/null || true
      rm -f  "${dir}"/node-*/*.socket "${dir}"/*/logs/*    2>/dev/null || true
      rm -fr "${dir}"/node-*/state-cluster/
      # Nomad stuff
      rm -f  "${dir}"/nomad/{server,client}.{log,stdout,stderr}
      rm -f  "${dir}"/nomad/nomad-job.json
      rm -fr "${dir}"/nomad/nomad-job.json.run/
    ;;

    ############################################################################
    # Start/stop individual cluster "programs" functions:
    # - start-node      RUN-DIR NODE-NAME
    # - start-generator RUN-DIR
    # - start-tracer    RUN-DIR              (Nomad backend specific subcommand)
    # - wait-node       RUN-DIR NODE_NAME    (Nomad backend specific subcommand)
    # - wait-tracer     RUN-DIR TASK-NAME    (Nomad backend specific subcommand)
    # - stop-node       RUN-DIR NODE-NAME
    # - start-nodes     RUN-DIR
    # - start-tracers   RUN-DIR              (Nomad backend specific subcommand)
    #
    # TODO:
    # - stop-generator  RUN-DIR              (Nomad backend specific subcommand)
    # - stop-tracer     RUN-DIR              (Nomad backend specific subcommand)
    ############################################################################
    # * Functions in the backend "interface" must use `fatal` when errors!

    # Called by `scenario.sh` with the exit trap (`scenario_setup_exit_trap`) set!
    start-node )
      local usage="USAGE: wb backend $op RUN-DIR NODE-NAME"
      local dir=${1:?$usage};  shift
      local node=${1:?$usage}; shift

      if ! backend_nomad task-program-start "${dir}" "${node}" "${node}"
      then
        msg "$(red "FATAL: Program \"${node}\" (always inside \"${node}\") startup failed")"
        # TODO: Let the download fail when everything fails?
        backend_nomad download-logs-node "${dir}" "${node}" || true
        # Should show the output/log of `supervisord` (runs as "entrypoint").
        msg "$(yellow "${dir}/nomad/${node}/stdout:")"
        cat                                                             \
          <(echo "-------------------- log start --------------------") \
          "${dir}"/nomad/"${node}"/stdout                               \
          <(echo "-------------------- log end   --------------------")
        msg "$(yellow "${dir}/nomad/${node}/stderr:")"
        cat                                                             \
          <(echo "-------------------- log start --------------------") \
          "${dir}"/nomad/"${node}"/stderr                               \
          <(echo "-------------------- log end   --------------------")
        # Depending on when the start command failed, logs may not be available!
        if test -f "${dir}"/"${node}"/stdout
        then
          msg "$(yellow "${dir}/${node}/stdout:")"
          cat                                                             \
            <(echo "-------------------- log start --------------------") \
            "$dir"/"$node"/stdout                                         \
            <(echo "-------------------- log end   --------------------")
        fi
        # Depending on when the start command failed, logs may not be available!
        if test -f "${dir}"/"${node}"/stderr
        then
          msg "$(yellow "${dir}/${node}/stderr:")"
          cat                                                             \
            <(echo "-------------------- log start --------------------") \
            "${dir}"/"${node}"/stderr                                     \
            <(echo "-------------------- log end   --------------------")
        fi
        fatal "Failed to start program \"${node}\""
      else
        # Link to "live" logs only available when running "exec" and local.
        local nomad_environment=$(envjqr 'nomad_environment')
        if test "${nomad_environment}" != "cloud"
        then
          # A link to the alloc must be already created inside the RUN-DIR
          ln -s                                                            \
            ../nomad/alloc/"${node}"/local/run/current/"${node}"/stdout    \
            "${dir}"/"${node}"/stdout
          ln -s                                                            \
            ../nomad/alloc/"${node}"/local/run/current/"${node}"/stderr    \
            "${dir}"/"${node}"/stderr
          ln -s                                                            \
            ../nomad/alloc/"${node}"/local/run/current/"${node}"/exit_code \
            "${dir}"/"${node}"/exit_code
        fi
        # Always wait for the node to be ready.
        if backend_nomad wait-node "${dir}" "${node}"
        then
          # It was "intentionally started and should not automagically stop" flag!
          touch "${dir}"/"${node}"/started
        else
          # Failed to start, mostly timeout before listening socket was found.
          backend_nomad stop-cluster "${dir}"
          fatal "Node \"${node}\" startup did not succeed"
        fi
      fi
    ;;

    # Called by `scenario.sh` with the exit trap (`scenario_setup_exit_trap`) set!
    start-generator )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      while test $# -gt 0
      do case "$1" in
        --* ) msg "FATAL:  unknown flag '$1'"; usage_nomadbackend;;
          * ) break;; esac; shift; done

      if ! backend_nomad task-program-start "${dir}" node-0 generator
      then
        msg "$(red "FATAL: Program \"generator\" (always inside \"node-0\") startup failed")"
        # TODO: Let the download fail when everything fails?
        backend_nomad download-logs-generator "${dir}" "node-0" || true
        # Should show the output/log of `supervisord` (runs as "entrypoint").
        msg "$(yellow "${dir}/nomad/node-0/stdout:")"
        cat                                                             \
          <(echo "-------------------- log start --------------------") \
          "${dir}"/nomad/node-0/stdout                                  \
          <(echo "-------------------- log end   --------------------")
        msg "$(yellow "${dir}/nomad/node-0/stderr:")"
        cat                                                             \
          <(echo "-------------------- log start --------------------") \
          "${dir}"/nomad/node-0/stderr                                  \
          <(echo "-------------------- log end   --------------------")
        # Depending on when the start command failed, logs may not be available!
        if test -f "${dir}"/generator/stdout
        then
          msg "$(yellow "${dir}/generator/stdout:")"
          cat                                                           \
          <(echo "-------------------- log start --------------------") \
          "$dir"/generator/stdout                                       \
          <(echo "-------------------- log end   --------------------")
        fi
        # Depending on when the start command failed, logs may not be available!
        if test -f "${dir}"/generator/stderr
        then
          msg "$(yellow "${dir}/generator/stderr:")"
          cat                                                             \
            <(echo "-------------------- log start --------------------") \
            "${dir}"/generator/stderr                                     \
            <(echo "-------------------- log end   --------------------")
        fi
        fatal "Failed to start program \"generator\""
      else
        # Link to "live" logs only available when running local.
        local nomad_environment=$(envjqr 'nomad_environment')
        if test "${nomad_environment}" != "cloud"
        then
          ln -s                                                         \
            ../nomad/alloc/node-0/local/run/current/generator/stdout    \
            "${dir}"/generator/stdout
          ln -s                                                         \
            ../nomad/alloc/node-0/local/run/current/generator/stderr    \
            "${dir}"/generator/stderr
          ln -s                                                         \
            ../nomad/alloc/node-0/local/run/current/generator/exit_code \
            "${dir}"/generator/exit_code
        fi
        # It was "intentionally started and should not automagically stop" flag!
        touch "${dir}"/generator/started
      fi
    ;;

    # Called by "start" that has no exit trap, don't use fatal here!
    start-tracer ) # Nomad backend specific subcommands
      local usage="USAGE: wb backend $op RUN-DIR TASK"
      local dir=${1:?$usage};  shift
      local task=${1:?$usage}; shift
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')

      if ! backend_nomad task-program-start "${dir}" "${task}" tracer
      then
        msg "$(red "FATAL: Program \"tracer\" (inside \"${task}\") startup failed")"
        # TODO: Let the download fail when everything fails?
        backend_nomad download-logs-tracer "${dir}" "${task}" || true
        if test "$one_tracer_per_node" = "true" || test "${task}" != "tracer"
        then
          # Should show the output/log of `supervisord` (runs as "entrypoint").
          msg "$(yellow "${dir}/nomad/${task}/stdout:")"
          cat                                                             \
            <(echo "-------------------- log start --------------------") \
            "${dir}"/nomad/"${task}"/stdout                               \
            <(echo "-------------------- log end   --------------------")
          msg "$(yellow "${dir}/nomad/${task}/stderr:")"
          cat                                                             \
            <(echo "-------------------- log start --------------------") \
            "${dir}"/nomad/"${task}"/stderr                               \
            <(echo "-------------------- log end   --------------------")
          # Depending on when the start command failed, logs may not be available!
          if test -f "${dir}"/tracer/"${task}"/stdout
          then
            msg "$(yellow "${dir}/tracer/${task}/stdout:")"
            cat                                                             \
              <(echo "-------------------- log start --------------------") \
              "${dir}"/tracer/"${task}"/stdout                              \
              <(echo "-------------------- log end   --------------------")
          fi
          # Depending on when the start command failed, logs may not be available!
          if test -f "${dir}"/tracer/"${task}"/stderr
          then
            msg "$(yellow "${dir}/tracer/${task}/stderr:")"
            cat                                                             \
              <(echo "-------------------- log start --------------------") \
              "${dir}"/tracer/"${task}"/stderr                              \
              <(echo "-------------------- log end   --------------------")
          fi
        else
          # Should show the output/log of `supervisord` (runs as "entrypoint").
          msg "$(yellow "${dir}/nomad/tracer/stdout:")"
          cat                                                             \
            <(echo "-------------------- log start --------------------") \
            "${dir}"/nomad/tracer/stdout                                  \
            <(echo "-------------------- log end   --------------------")
          msg "$(yellow "${dir}/nomad/tracer/stderr:")"
          cat                                                             \
            <(echo "-------------------- log start --------------------") \
            "${dir}"/nomad/tracer/stderr                                  \
            <(echo "-------------------- log end   --------------------")
          # Depending on when the start command failed, logs may not be available!
          if test -f "${dir}"/tracer/stdout
          then
            msg "$(yellow "${dir}/tracer/stdout:")"
            cat                                                             \
              <(echo "-------------------- log start --------------------") \
              "$dir"/tracer/stdout                                          \
              <(echo "-------------------- log end   --------------------")
          fi
          # Depending on when the start command failed, logs may not be available!
          if test -f "${dir}"/tracer/stderr
          then
            msg "$(yellow "${dir}/tracer/stderr:")"
            cat                                                             \
              <(echo "-------------------- log start --------------------") \
              "${dir}"/tracer/stderr                                        \
              <(echo "-------------------- log end   --------------------")
          fi
        fi
        # Let "start" parse the response code and handle the cleanup!
        msg "$(red "FATAL: Failed to start \"tracer\"")"
        return 1
      else
        # Link to "live" logs only available when running local.
        local nomad_environment=$(envjqr 'nomad_environment')
        local nomad_task_driver=$(envjqr 'nomad_task_driver')
        if test "${nomad_environment}" != "cloud"
        then
          if test "${one_tracer_per_node}" = "true" || test "${task}" != "tracer"
          then
            ln -s                                                            \
              ../../nomad/alloc/"${task}"/local/run/current/tracer/stdout    \
              "${dir}"/tracer/"${task}"/stdout
            ln -s                                                            \
              ../../nomad/alloc/"${task}"/local/run/current/tracer/stderr    \
              "${dir}"/tracer/"${task}"/stderr
            ln -s                                                            \
              ../../nomad/alloc/"${task}"/local/run/current/tracer/exit_code \
              "${dir}"/tracer/"${task}"/exit_code
          else
            # When "local" and "podman" "tracer" folder is mounted
            if ! test "${nomad_task_driver}" = "podman"
            then
              ln -s                                                            \
                ../nomad/alloc/tracer/local/run/current/tracer/stdout          \
                "${dir}"/tracer/stdout
              ln -s                                                            \
                ../nomad/alloc/tracer/local/run/current/tracer/stderr          \
                "${dir}"/tracer/stderr
              ln -s                                                            \
                ../../nomad/alloc/"${task}"/local/run/current/tracer/exit_code \
                "${dir}"/tracer/"${task}"/exit_code
            fi
          fi
        fi
        # Always wait for the tracer to be ready.
        if backend_nomad wait-tracer "${dir}" "${task}"
        then
          # It was "intentionally started and should not automagically stop" flag!
          if test "${one_tracer_per_node}" = "true" || test "${task}" != "tracer"
          then
            touch "${dir}"/tracer/"${task}"/started
          else
            touch "${dir}"/tracer/started
          fi
        else
          # Failed to start, mostly timeout before listening socket was found.
          # Don't use fatal here, let `start-tracers` decide!
          msg "$(red "Task \"${task}\"'s tracer startup did not succeed")"
          return 1
        fi
      fi
    ;;

    # Called by "start-node" that has no exit trap, don't use fatal here!
    wait-node )
      local usage="USAGE: wb backend $op RUN-DIR [NODE-NAME]"
      local dir=${1:?$usage}; shift
      local node=${1:-$(dirname $CARDANO_NODE_SOCKET_PATH | xargs basename)}; shift

      local socket=$(backend_nomad get-node-socket-path "${dir}" ${node})
      local socket_path_absolute=/"${node}"/local/run/current/"${node}"/node.socket
      local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' ${dir}/profile.json)
      msg "$(blue Waiting) ${patience}s for socket of supervisord $(yellow "program \"${node}\"") inside Nomad $(yellow "Task \"${node}\"") ..."
      local i=0
      local node_alloc_id
      node_alloc_id=$(wb_nomad job task-name-allocation-id \
        "$dir/nomad/nomad-job.json"                        \
        "${node}")
      # Always keep checking that the supervisord program is still running!
      while \
            backend_nomad is-task-program-running "${dir}" "${node}" "${node}"                                                         \
        &&                                                                                                                             \
          ! nomad alloc fs -stat -H "${node_alloc_id}" "${socket_path_absolute}" 2>/dev/null | grep --quiet "application/octet-stream"
      # TODO: Add the "timer" `printf "%3d" $i;` but for concurrent processes!
      do
        sleep 1
        i=$((i+1))
        if test "${i}" -ge "${patience}"
        then
          msg "$(red "Patience ran out for \"${node}\" after ${patience}s")"
          msg "$(yellow "check logs in ${dir}/${node}/[stdout & stderr]")"
          # Don't use fatal here, let `start-node` or `start-nodes` decide!
          return 1
        fi
      done
      msg "$(green "supervisord program \"${node}\" inside Nomad Task \"${node}\" up (${i}s)!")"
      return 0
    ;;

    # Called by "start-tracer" that has no exit trap, don't use fatal here!
    wait-tracer )
      local usage="USAGE: wb backend $op RUN-DIR TASK"
      local dir=${1:?$usage};  shift
      local task=${1:?$usage}; shift

      local nomad_environment=$(envjqr   'nomad_environment')
      local nomad_task_driver=$(envjqr   'nomad_task_driver')
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')

      local patience=$(jq '.analysis.cluster_startup_overhead_s | ceil' "${dir}/profile.json")
      # When "local" and "podman" "tracer" folder is mounted
      if test "${nomad_task_driver}" = "podman" && test "${nomad_environment}" = "local"
      then
        # If the folder is mounted, its contents are not available on the
        # Nomad Client allocation folder
        local socket_path_relative=$(jq -r '.network.contents' "${dir}/tracer/config.json")
        local socket_path_absolute="${dir}"/tracer/"${socket_path_relative}"
        # Wait for tracer socket
        #local socket_path_absolute="$dir/tracer/$node/$socket_path_relative"
        msg "$(blue Waiting) ${patience}s for socket of supervisord $(yellow "program \"tracer\"") inside Nomad $(yellow "Task \"${task}\"") ..."
        local i=0
        while ! test -S "${socket_path_absolute}"
        # TODO: Add the "timer" `printf "%3d" $i;` but for concurrent processes!
        do
          sleep 1
          i=$((i+1))
          if test "${i}" -ge "${patience}"
          then
            msg "$(red "Patience ran out for Task \"${task}\"'s tracer after ${patience}s")"
            msg "$(yellow "check logs in ${dir}/tracer/[stdout & stderr]")"
            # Don't use fatal here, let `start-tracer` decide!
            return 1
          fi
        done
      else
        if test "${one_tracer_per_node}" = "true" || test "${task}" != "tracer"
        then
          local socket_path_relative=$(jq -r '.network.contents' "${dir}/tracer/${task}/config.json")
          local socket_path_absolute=/"${task}"/local/run/current/tracer/"${socket_path_relative}"
        else
          local socket_path_relative=$(jq -r '.network.contents' "${dir}/tracer/config.json")
          local socket_path_absolute=/tracer/local/run/current/tracer/"${socket_path_relative}"
        fi
        # Wait for tracer socket
        #local socket_path_absolute="$dir/tracer/$node/$socket_path_relative"
        msg "$(blue Waiting) ${patience}s for socket of supervisord $(yellow "program \"tracer\"") inside Nomad $(yellow "Task \"${task}\"") ..."
        local i=0
        # while test ! -S "$socket_path_absolute"
        local task_alloc_id
        task_alloc_id=$(wb_nomad job task-name-allocation-id \
          "${dir}/nomad/nomad-job.json"                      \
          "${task}")
        # Always keep checking that the supervisord program is still running!
        while \
              backend_nomad is-task-program-running "${dir}" "${task}" tracer                                                \
          &&                                                                                                                 \
            ! nomad alloc fs -stat -H "${task_alloc_id}" "${socket_path_absolute}" | grep --quiet "application/octet-stream"
        do printf "%3d" $i; sleep 1
          i=$((i+1))
          if test "${i}" -ge "${patience}"
          then
            msg "$(red "Patience ran out for Task \"${task}\"'s tracer after ${patience}s")"
            if test "${one_tracer_per_node}" = "true" || test "${task}" != "tracer"
            then
              msg "$(yellow "Check logs in ${dir}/tracer/${task}/[stdout & stderr]")"
            else
              msg "$(yellow "Check logs in ${dir}/tracer/[stdout & stderr]")"
            fi
            # Don't use fatal here, let `start-node` or `start-nodes` decide!
            return 1
          fi
        done
      fi
      msg "$(green "supervisord program \"tracer\" inside Nomad Task \"${task}\" up (${i}s)!")"
      return 0
    ;;

    stop-node )
      local usage="USAGE: wb backend $op RUN-DIR NODE-NAME"
      local dir=${1:?$usage};  shift
      local node=${1:?$usage}; shift

      backend_nomad task-program-stop "${dir}" "${node}" "${node}"
      touch "${dir}"/"${node}"/stopped
    ;;

    # Called by `scenario.sh` with the exit trap (`scenario_setup_exit_trap`) set!
    start-nodes )
      local usage="USAGE: wb backend $op RUN-DIR [HONOR_AUTOSTART=]"
      local dir=${1:?$usage}; shift
      local honor_autostart=${1:-}

      # Start all "autostart" nodes in parallel!
      local jobs_array=()
      local nodes=($(jq_tolist keys "${dir}"/node-specs.json))
      for node in ${nodes[*]}
      do
        if test -n "${honor_autostart}"
        then
          if jqtest ".\"${node}\".autostart" "${dir}"/node-specs.json
          then
            backend_nomad start-node "${dir}" "${node}" &
            jobs_array+=("$!")
          fi
        else
          backend_nomad start-node "${dir}" "${node}" &
          jobs_array+=("$!")
        fi
      done
      # Wait and check!
      if test -n "${jobs_array}"
      then
        if ! wait_fail_any "${jobs_array[@]}"
        then
          fatal "Failed to start node(s)"
        else
          for node in ${nodes[*]}
          do
            if test -n "${honor_autostart}"
            then
              if jqtest ".\"${node}\".autostart" "${dir}"/node-specs.json
              then
                if ! test -f "${dir}"/"${node}"/started
                then
                  fatal "Node \"${node}\" failed to start!"
                fi
              fi
            else
              if ! test -f "${dir}"/"${node}"/started
              then
                fatal "Node \"${node}\" failed to start!"
              fi
            fi
          done
        fi
      fi

      if test ! -v CARDANO_NODE_SOCKET_PATH
      then
        export CARDANO_NODE_SOCKET_PATH=$(backend_nomad get-node-socket-path "${dir}" 'node-0')
      fi
    ;;

    start-tracers )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
      if ! test "${one_tracer_per_node}" = "true"
      then
        backend_nomad start-tracer "${dir}" "tracer"
      else
        local jobs_array=()
        local nodes=($(jq_tolist keys "$dir"/node-specs.json))
        for node in ${nodes[*]}
        do
          backend_nomad start-tracer "${dir}" "${node}" &
          jobs_array+=("$!")
        done
        # Wait and check!
        if test -n "${jobs_array}"
        then
          if ! wait_fail_any "${jobs_array[@]}"
          then
            # Don't use fatal here, let `start` decide!
            msg "$(red "Failed to start tracer(s)")"
            return 1
          else
            for node in ${nodes[*]}
            do
              if ! test -f "${dir}"/tracer/"${node}"/started
              then
                # Don't use fatal here, let `start` decide!
                msg "$(red "Tracer for \"${node}\" failed to start!")"
                return 1
              fi
            done
          fi
        fi
        return 0
      fi
    ;;

    ############################################################################
    # Cluster "wait" functions:
    # - get-node-socket-path    RUN-DIR NODE-NAME (Will break when cloud running)
    # - wait-node-stopped       RUN-DIR NODE-NAME
    # - wait-pools-stopped      RUN-DIR
    # - cluster-exited-programs RUN-DIR      (Nomad backend specific subcommand)
    ############################################################################
    # * Functions in the backend "interface" must use `fatal` when errors!

    get-node-socket-path )
      local usage="USAGE: wb backend $op RUN-DIR NODE-NAME"
      local dir=${1:?$usage};  shift
      local node=${1:?$usage}; shift

      # TODO: Get socket path from node's config ?
      echo -n "$dir"/"${node}"/node.socket
    ;;

    wait-node-stopped )
      local usage="USAGE: wb backend $op RUN-DIR NODE"
      local dir=${1:?$usage};  shift
      local node=${1:?$usage}; shift

      progress_ne "nomad" "$(blue Waiting) until ${node} stops:  ....."
      local i=0
      while backend_nomad is-task-program-running "${dir}" "${node}" "${node}" > /dev/null
      do
        echo -ne "\b\b\b\b\b"; printf "%5d" $i >&2; i=$((i+1))
        #sleep 1
        # Instead of sleeping check if any other supervisord pgoram has stopped
        # This supervisord servers can be running on many Nomad clients (Task Groups)
        local programs_stopped=( $(cluster-exited-programs "${dir}") )
        if test -n "${programs_stopped}"
        then
          fatal "Programs (${programs_stopped[@]}) exited while waiting for ${node}"
        fi
      done >&2
      echo -e "\b\b\b\b\bdone, after $(with_color white $i) seconds" >&2
    ;;

    wait-pools-stopped )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      local start_time=$(date +%s)
      local pools=$(jq .composition.n_pool_hosts "${dir}"/profile.json)
      msg_ne "nomad: $(blue Waiting) until all pool nodes are stopped: 000000"
      for ((pool_ix=0; pool_ix < ${pools}; pool_ix++))
      do
        while \
            ! test -f "${dir}"/flag/cluster-stopping \
          && \
            backend_nomad is-task-program-running "${dir}" "node-${pool_ix}" "node-${pool_ix}" > /dev/null
        do
          # Always check that a started generator has not FAILED!
          if \
                test -f "${dir}"/generator/started                              \
            &&                                                                  \
              ! test -f "${dir}"/generator/quit                                 \
            &&                                                                  \
              ! backend_nomad is-task-program-running "${dir}" node-0 generator
          then
            if backend_nomad is-task-program-failed   "${dir}" node-0 generator
            then
              # If node-0 quits generators fails with:
              # tx-generator: MuxError MuxBearerClosed "<socket: 12> closed when reading data, waiting on next header True"
              # Service binary 'tx-generator' returned status: 1
              if backend_nomad is-task-program-running "${dir}" node-0 node-0
              then
                # This was not expected!
                # But check it wasn't a race condition of a stopping cluster!
                if ! test -f "${dir}"/flag/cluster-stopping
                then
                  touch "${dir}"/flag/cluster-stopping
                  fatal "Generator quit unexpectedly!!!"
                fi
              else
                # The whole cluster is about to finish!
                touch "${dir}"/generator/quit
              fi
            else
              touch "${dir}"/generator/quit
              # Show the warning and continue with the counter
              echo -e "\n"
              msg "$(yellow "WARNING: supervisord program \"generator\" (always inside Nomad Task \"node-0\" quit with a non-error exit code")"
              msg_ne "nomad: $(blue Waiting) until all pool nodes are stopped: 000000"
            fi
          fi
          local elapsed="$(($(date +%s) - start_time))"
          echo -ne "\b\b\b\b\b\b"
          printf "%6d" "${elapsed}"
          sleep 1
        done   # While
        if ! test -f "${dir}"/flag/cluster-stopping
        then
            echo -ne "\b\b\b\b\b\b"
            echo -n "node-${pool_ix} 000000"
        fi
      done >&2 # For
      echo -ne "\b\b\b\b\b\b"

      local elapsed=$(($(date +%s) - start_time))
      if test -f "${dir}"/flag/cluster-stopping
      then
        echo " Termination requested -- after $(yellow ${elapsed})s" >&2;
      else
        touch "${dir}"/flag/cluster-stopping
        echo " All nodes exited      -- after $(yellow ${elapsed})s" >&2
      fi
    ;;

    cluster-exited-programs )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local array=()
      # Generator
      if ! test -f "${dir}"/generator/started
      then
        backend_nomad is-task-program-running "${dir}" node-0 generator || array+=("generator")
      fi
      # Nodes
      local nodes=($(jq_tolist keys "${dir}"/node-specs.json))
      for node in ${nodes[*]}
      do
        if ! test -f "${dir}"/"${node}"/started
        then
          backend_nomad is-task-program-running "${dir}" "${node}" "${node}" || array+=("${node}")
        fi
      done
      # Tracer(s)
      if jqtest ".node.tracer" "${dir}"/profile.json
      then
        local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
        if test "$one_tracer_per_node" = "true"
        then
          for node in ${nodes[*]}
          do
            if ! test -f "${dir}"/tracer/"${node}"/started
            then
              backend_nomad is-task-program-running "${dir}" "${node}" tracer || array+=("tracer")
            fi
          done
        else
          if ! test -f "${dir}"/tracer/started
          then
            backend_nomad is-task-program-running "${dir}" tracer tracer || array+=("tracer")
          fi
        fi
      fi
      echo "${array[@]}"
    ;;

    # For debugging when something fails, downloads and prints details!
    download-logs-generator )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local download_ok="true"
      # Should show the output/log of `supervisord` (runs as "entrypoint").
      msg "$(blue Fetching) $(yellow "entrypoint's stdout and stderr") of Nomad $(yellow "Task \"${task}\"") ..."
      backend_nomad task-entrypoint-stdout "${dir}" "${task}" \
      > "${dir}"/nomad/"${task}"/stdout                       \
      || download_ok="false"
      backend_nomad task-entrypoint-stderr "${dir}" "${task}" \
      > "${dir}"/nomad/"${task}"/stderr                       \
      || download_ok="false"
      # If the entrypoint was ran till the end, this file should be available!
      msg "$(blue Fetching) $(yellow supervisord.log) of Nomad $(yellow "Task \"${task}\"") ..."
      backend_nomad task-file-contents "${dir}" "${task}"    \
        /local/run/current/supervisor/supervisord.log        \
      > "${dir}"/supervisor/"${task}"/supervisord.log        \
      || download_ok="false"
      # Downloads "exit_code", "stdout", "stderr" and GHC files.
      # Depending on when the start command failed, logs may not be available!
      backend_nomad download-zstd-generator "${dir}" "${task}" \
      || download_ok="false"
      # Return
      if test "${download_ok}" = "false"
      then
        msg "$(red "Failed to download \"generator\" run files from \"${task}\"")"
        return 1
      else
        return 0
      fi
    ;;

    # For debugging when something fails, downloads and prints details!
    download-logs-node )
      local usage="USAGE: wb backend pass $op RUN-DIR NODE-NAME"
      local dir=${1:?$usage}; shift
      local node=${1:?$usage}; shift
      local download_ok="true"
      # Should show the output/log of `supervisord` (runs as "entrypoint").
      msg "$(blue Fetching) $(yellow "entrypoint's stdout and stderr") of Nomad $(yellow "Task \"${node}\"") ..."
      backend_nomad task-entrypoint-stdout "${dir}" "${node}" \
      > "${dir}"/nomad/"${node}"/stdout                       \
      || download_ok="false"
      backend_nomad task-entrypoint-stderr "${dir}" "${node}" \
      > "${dir}"/nomad/"${node}"/stderr                       \
      || download_ok="false"
      # If the entrypoint was ran till the end, this file should be available!
      msg "$(blue Fetching) $(yellow supervisord.log) of Nomad $(yellow "Task \"${node}\"") ..."
      backend_nomad task-file-contents "${dir}" "${node}" \
        /local/run/current/supervisor/supervisord.log     \
      > "${dir}"/supervisor/"${node}"/supervisord.log     \
      || download_ok="false"
      # Downloads "exit_code", "stdout", "stderr" and GHC files.
      # Depending on when the start command failed, logs may not be available!
      backend_nomad download-zstd-node "${dir}" "${node}" \
      || download_ok="false"
      # Return
      if test "${download_ok}" = "false"
      then
        msg "$(red "Failed to download \"${node}\" run files from \"${node}\"")"
        # Below like errors can end in truncated files, a proper flag is needed!
        # failed to exec into task: read tcp 10.0.0.115:33840->3.72.231.105:443: read: connection reset by peer
        # tar: Unexpected EOF in archive
        # tar: Unexpected EOF in archive
        # tar: Error is not recoverable: exiting now
        touch "${dir}"/nomad/"${node}"/download_failed
        return 1
      else
        if test -f "${dir}"/nomad/"${node}"/download_failed
        then
          rm "${dir}"/nomad/"${node}"/download_failed
        fi
        return 0
      fi
    ;;

    # For debugging when something fails, downloads and prints details!
    download-logs-tracer )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      # If the tracer is not a "dedicated" Nomad Task it is just one more
      # supervisord program and no entrypoints' logs are downloaded here
      # because they should be downloaded by the main supervisord program.
      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
      if test "${one_tracer_per_node}" = "true" || test "${task}" != "tracer"
      then
        # Downloads "exit_code", "stdout", "stderr" and GHC files.
        # Depending on when the start command failed, logs may not be available!
        if ! backend_nomad download-zstd-tracer "${dir}" "${task}"
        then
          msg "$(red "Failed to download \"tracer\" run files from \"${task}\"")"
          # Below like errors can end in truncated files, a proper flag is needed!
          # failed to exec into task: read tcp 10.0.0.115:33840->3.72.231.105:443: read: connection reset by peer
          # tar: Unexpected EOF in archive
          # tar: Unexpected EOF in archive
          # tar: Error is not recoverable: exiting now
          touch "${dir}"/tracer/"${task}"/download_failed
          return 1
        else
          if test -f "${dir}"/tracer/"${task}"/download_failed
          then
            rm "${dir}"/tracer/"${task}"/download_failed
          fi
          return 0
        fi
      else
        local download_ok="true"
        # Should show the output/log of `supervisord` (runs as "entrypoint").
        msg "$(blue Fetching) $(yellow "entrypoint's stdout and stderr") of Nomad $(yellow "Task \"tracer\"") ..."
        backend_nomad task-entrypoint-stdout "${dir}" "tracer" \
        > "${dir}"/nomad/tracer/stdout                         \
        || download_ok="false"
        backend_nomad task-entrypoint-stderr "${dir}" "tracer" \
        > "${dir}"/nomad/tracer/stderr                         \
        || download_ok="false"
        # If the entrypoint was ran till the end, this file should be available!
        msg "$(blue Fetching) $(yellow supervisord.log) of Nomad $(yellow "Task \"tracer\"") ..."
        backend_nomad task-file-contents "${dir}" "tracer" \
          /local/run/current/supervisor/supervisord.log    \
        > "${dir}"/supervisor/tracer/supervisord.log       \
        || download_ok="false"
        # When "local" and "podman" "tracer" folder is mounted
        local nomad_task_driver=$(envjqr 'nomad_task_driver')
        if ! test "${nomad_task_driver}" = "podman"
        then
          # Downloads "exit_code", "stdout", "stderr" and GHC files.
          # Depending on when the start command failed, logs may not be available!
          backend_nomad download-zstd-tracer "${dir}" "tracer" \
          || download_ok="false"
        fi
        if test "${download_ok}" = "false"
        then
          msg "$(red "Failed to download \"tracer\" run files from \"tracer\"")"
          # Below like errors can end in truncated files, a proper flag is needed!
          # failed to exec into task: read tcp 10.0.0.115:33840->3.72.231.105:443: read: connection reset by peer
          # tar: Unexpected EOF in archive
          # tar: Unexpected EOF in archive
          # tar: Error is not recoverable: exiting now
          touch "${dir}"/tracer/download_failed
          return 1
        else
          if test -f "${dir}"/tracer/download_failed
          then
            rm "${dir}"/tracer/download_failed
          fi
          return 0
        fi
      fi
    ;;

    download-zstd-generator )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift

      msg "$(blue Fetching) $(yellow "\"generator\"") run files from Nomad $(yellow "Task \"${task}\"") ..."
      # TODO: Add compression, either "--zstd" or "--xz"
        backend_nomad task-exec-program-run-files-tar-zstd        \
          "${dir}" "${task}" "generator"                          \
      | tar --extract                                             \
          --directory="${dir}"/generator/ --file=-                \
          --no-same-owner --no-same-permissions
    ;;

    download-zstd-node )
      local usage="USAGE: wb backend pass $op RUN-DIR NODE-NAME"
      local dir=${1:?$usage}; shift
      local node=${1:?$usage}; shift

      msg "$(blue Fetching) node's $(yellow "\"${node}\"") run files from Nomad $(yellow "Task \"${node}\"") ..."
      # TODO: Add compression, either "--zstd" or "--xz"
        backend_nomad task-exec-program-run-files-tar-zstd        \
          "${dir}" "${node}" "${node}"                            \
      | tar --extract                                             \
          --directory="${dir}"/"${node}"/ --file=-                \
          --no-same-owner --no-same-permissions
    ;;

    download-zstd-tracer )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift

      local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
      if test "${one_tracer_per_node}" = "true" || test "${task}" != "tracer"
      then
        msg "$(blue Fetching) $(yellow "\"tracer\"") run files from Nomad $(yellow "Task \"${task}\"") ..."
        # TODO: Add compression, either "--zstd" or "--xz"
          backend_nomad task-exec-program-run-files-tar-zstd    \
            "${dir}" "${task}" "tracer"                         \
        | tar --extract                                         \
            --directory="${dir}"/tracer/"${task}" --file=-      \
            --no-same-owner --no-same-permissions
      else
        # When "local" and "podman" "tracer" folder is mounted
        local nomad_task_driver=$(envjqr 'nomad_task_driver')
        if ! test "${nomad_task_driver}" = "podman"
        then
          msg "$(blue Fetching) $(yellow "\"tracer\"") run files from Nomad $(yellow "Task \"tracer\"") ..."
          # TODO: Add compression, either "--zstd" or "--xz"
            backend_nomad task-exec-program-run-files-tar-zstd  \
              "${dir}" "tracer" "tracer"                        \
          | tar --extract                                       \
              --directory="${dir}"/tracer/ --file=-             \
              --no-same-owner --no-same-permissions
        fi
      fi
    ;;

    download-zstd-tracer-logRoot )
      local usage="USAGE: wb backend pass $op RUN-DIR NODE-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      if jqtest ".node.tracer" "$dir"/profile.json
      then
        local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
        if test "${one_tracer_per_node}" = "true" || test "${task}" != "tracer"
        then
          # Logs will only be available if the tracer was started at least once!
          if test -f "${dir}"/tracer/${task}/started
          then
            msg "$(blue Fetching) $(yellow "\"tracer\"") logRoot from Nomad $(yellow "Task \"${task}\"") ..."
            # TODO: Add compression, either "--zstd" or "--xz"
              backend_nomad task-exec-tracer-folders-tar-zstd           \
              "${dir}" "${task}"                                        \
            | tar --extract                                             \
                --directory="${dir}"/tracer/ --file=-                   \
                --no-same-owner --no-same-permissions
          fi
        else
          # When "local" and "podman" "tracer" folder is mounted
          local nomad_task_driver=$(envjqr 'nomad_task_driver')
          if ! test "${nomad_task_driver}" = "podman"
          then
            # Logs will only be available if the tracer was started at least once!
            if test -f "${dir}"/tracer/started
            then
              msg "$(blue Fetching) $(yellow "\"tracer\"") logRoot from Nomad $(yellow "Task \"tracer\"") ..."
              # TODO: Add compression, either "--zstd" or "--xz"
                backend_nomad task-exec-tracer-folders-tar-zstd         \
                "${dir}" "${node}"                                      \
              | tar --extract                                           \
                  --directory="${dir}"/tracer/ --file=-                 \
                  --no-same-owner --no-same-permissions
            fi
          fi
        fi
      fi
    ;;

    download-config-generator )
      local usage="USAGE: wb backend pass $op RUN-DIR"
      local dir=${1:?$usage}; shift
      # Generator runs inside task/supervisord "node-0"
      # Node files that may suffer interpolation/sed replace.
      backend_nomad task-file-contents "${dir}" "node-0" \
        /local/run/current/generator/start.sh            \
      > "${dir}"/generator/start.sh
      backend_nomad task-file-contents "${dir}" "node-0" \
        /local/run/current/generator/run-script.json     \
      > "${dir}"/generator/run-script.json
    ;;

    download-config-node )
      local usage="USAGE: wb backend pass $op RUN-DIR NODE-NAME"
      local dir=${1:?$usage}; shift
      local node=${1:?$usage}; shift
      # Node files that may suffer interpolation/sed replace.
      backend_nomad task-file-contents "${dir}" "${node}" \
        /local/run/current/"${node}"/start.sh             \
      > "${dir}"/"${node}"/start.sh
      backend_nomad task-file-contents "${dir}" "${node}" \
        /local/run/current/"${node}"/config.json          \
      > "${dir}"/"${node}"/config.json
      backend_nomad task-file-contents "${dir}" "${node}" \
        /local/run/current/"${node}"/topology.json        \
      > "${dir}"/"${node}"/topology.json
      # This Task's supervisor files
      backend_nomad task-file-contents "${dir}" "${node}" \
        /local/run/current/supervisor/supervisord.conf    \
      > "${dir}"/supervisor/"${node}"/supervisord.conf
      # Dynamically modified file, store to be able to debug!
      backend_nomad task-file-contents "${dir}" "${node}" \
        /local/entrypoint.sh                              \
      > "${dir}"/nomad/"${node}"/entrypoint.sh
      # Dynamically generated file with the envars of the entrypoint!
      backend_nomad task-file-contents "${dir}" "${node}" \
        /local/entrypoint.env                             \
      > "${dir}"/nomad/"${node}"/entrypoint.env
      # Dynamically generated file with all the services/addresses found!
      backend_nomad task-file-contents "${dir}" "${node}" \
        /local/networking.json                            \
      > "${dir}"/nomad/"${node}"/networking.json
    ;;

    download-config-tracer )
      local usage="USAGE: wb backend pass $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      if jqtest ".node.tracer" "$dir"/profile.json
      then
        local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
        if test "${one_tracer_per_node}" = "true" || test "${task}" != "tracer"
        then
          # Node files that may suffer interpolation/sed replace.
          backend_nomad task-file-contents "${dir}" "${task}" \
            /local/run/current/tracer/start.sh                \
          > "${dir}"/tracer/"${task}"/start.sh
          backend_nomad task-file-contents "${dir}" "${task}" \
            /local/run/current/tracer/config.json             \
          > "${dir}"/tracer/"${task}"/config.json
        else
          local nomad_task_driver=$(envjqr 'nomad_task_driver')
          # When "local" and "podman" "tracer" folder is mounted and contents
          # created locally by the workbench (obtained from the profile services).
          if ! test "${nomad_task_driver}" = "podman"
          then
            # Node files that may suffer interpolation/sed replace.
            backend_nomad task-file-contents "${dir}" "tracer" \
              /local/run/current/tracer/start.sh               \
            > "${dir}"/tracer/start.sh
            backend_nomad task-file-contents "${dir}" "tracer" \
              /local/run/current/tracer/config.json            \
            > "${dir}"/tracer/config.json
          fi
          # This Task's supervisor files
          backend_nomad task-file-contents "${dir}" "tracer" \
            /local/run/current/supervisor/supervisord.conf   \
          > "${dir}"/supervisor/tracer/supervisord.conf
          # Dynamically modified file, store to be able to debug!
          backend_nomad task-file-contents "${dir}" "tracer" \
            /local/entrypoint.sh                             \
          > "${dir}"/nomad/tracer/entrypoint.sh
          # Dynamically generated file with the envars of the entrypoint!
          backend_nomad task-file-contents "${dir}" "tracer" \
            /local/entrypoint.env                            \
          > "${dir}"/nomad/tracer/entrypoint.env
          # Dynamically generated file with all the services/addresses found!
          backend_nomad task-file-contents "${dir}" "tracer" \
            /local/networking.json                           \
          > "${dir}"/nomad/tracer/networking.json
        fi
      fi
    ;;

    ## Nomad job tasks supervisord queries
    ######################################

    task-program-start )
      local usage="USAGE: wb backend pass $op RUN-DIR SUPERVISOR-PROGRAM"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local program=${1:?$usage}; shift

      msg "$(blue Starting) supervisord $(yellow "program \"${program}\"") inside Nomad $(yellow "Task \"${task}\"") ..."
      backend_nomad task-supervisorctl "${dir}" "${task}" start "${program}" > /dev/null
    ;;

    task-program-stop )
      local usage="USAGE: wb backend pass $op RUN-DIR SUPERVISOR-PROGRAM"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local program=${1:?$usage}; shift

      msg "$(blue Stopping) supervisord $(yellow "program \"${program}\"") inside Nomad $(yellow "Task \"${task}\"") ..."
      backend_nomad task-supervisorctl "${dir}" "${task}" stop "${program}" > /dev/null
    ;;

    is-task-program-running )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME SUPERVISOR-PROGRAM"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local program=${1:?$usage}; shift
      # NOTICE: Only returns zero when RUNNING!
      #> supervisorctl status
      # generator                        RUNNING   pid 83, uptime 0:00:23
      # node-0                           RUNNING   pid 62, uptime 0:00:27
      # node-1                           STOPPED   Not started
      # tracer                           RUNNING   pid 43, uptime 0:00:30
      #> $ echo $?
      # 3
      #> supervisorctl status node-0 >/dev/null; echo $?
      # 0
      #> supervisorctl status node-1 >/dev/null; echo $?
      # 3
      #> supervisorctl status
      # generator                        EXITED    Feb 06 01:48 PM
      # node-0                           EXITED    Feb 06 01:48 PM
      # node-1                           STOPPED   Not started
      # tracer                           RUNNING   pid 43, uptime 0:02:57
      #> $ echo $?
      # 3
      #> supervisorctl status node-0 >/dev/null; echo $?
      # 3
      backend_nomad task-supervisorctl "$dir" "$task" status "$program" > /dev/null
    ;;

    is-task-program-failed )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME SUPERVISOR-PROGRAM"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local program=${1:?$usage}; shift
      # As we are not using any "autorestart" supervisord programs are run as:
      # command=sh -c "./start.sh; echo "$?" > ./exit_code"
      # because we can't obtain the exit codes using `supervisrctl`
      local exit_code
      if exit_code=$(backend_nomad task-file-contents "${dir}" "${task}" \
        /local/run/current/"${program}"/exit_code 2>/dev/null)
      then
        test "${exit_code}" != "0"
      else
        return 0
      fi
    ;;

    task-supervisorctl )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME CMD"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local action=${1:?$usage}; shift

      # The `supervisord` binary is nix-installed inside the container but not
      # added to $PATH (resides in /nix/store)
      local container_supervisor_nix=$(jq -r '.containerPkgs.supervisor."nix-store-path"' "$dir"/container-specs.json)
      # The `--serverurl` argument is needed in every call to `nomad exec`.
      # Uusually a socket/file decided between the container and the Job file.
      local container_supervisord_url=$(jq -r .supervisord.url "$dir"/container-specs.json)
      # The container needs to know where the `supervisord` config file is
      # located so it can be started.
      local container_supervisord_conf=$(jq -r .supervisord.conf "$dir"/container-specs.json)
      # Returns "PROGRAM-NAME: ERROR (no such file)" when `supervisord` is not
      # able to find the command defined in "command=XXX" for PROGRAM-NAME
      # "[program:PROGRAM-NAME]"
      backend_nomad task-exec "$dir" "$task"            \
        "$container_supervisor_nix"/bin/supervisorctl   \
          --serverurl "$container_supervisord_url"      \
          --configuration "$container_supervisord_conf" \
          "$action" $@
    ;;

    ## Nomad job tasks exec queries
    ###############################

    task-exec )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME CMD"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift

      local task_alloc_id
      task_alloc_id=$(wb_nomad job task-name-allocation-id \
        "${dir}/nomad/nomad-job.json"                                 \
        "${task}")
      # If you run it without `-i=false -t=false` supervisord starts an
      # interactive shell (output "supervisor>") and breaks the whole script
      # expecting you to hit enter on every call!
      nomad alloc exec                      \
        -i=false -t=false                   \
        --task "${task}" "${task_alloc_id}" \
        "$@"
    ;;

    task-exec-program-run-files-tar-zstd )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local program=${1:?$usage}; shift

      local bash_path="$(jq -r ".containerPkgs.bashInteractive.\"nix-store-path\"" "${dir}"/container-specs.json)"/bin/bash
      local find_path="$(jq -r ".containerPkgs.findutils.\"nix-store-path\""       "${dir}"/container-specs.json)"/bin/find
      local tar_path="$(jq  -r ".containerPkgs.gnutar.\"nix-store-path\""          "${dir}"/container-specs.json)"/bin/tar
      local cat_path="$(jq  -r ".containerPkgs.coreutils.\"nix-store-path\""       "${dir}"/container-specs.json)"/bin/cat
      local prog_dir=/local/run/current/"${program}"/
      # TODO: Add compression, either "--zstd" or "--xz"
      # tar (child): zstd: Cannot exec: No such file or directory
      # tar (child): Error is not recoverable: exiting now
      # tar (child): xz: Cannot exec: No such file or directory
      # tar (child): Error is not recoverable: exiting now
      # Code example of the files needed: https://github.com/input-output-hk/cardano-ops/blob/bench-master/bench/bench.sh#L646-L670
      backend_nomad task-exec "${dir}" "${task}"         \
        "${bash_path}" -c                                \
        "                                                \
          \"${find_path}\" \"${prog_dir}\"               \
            -mindepth 1 -maxdepth 1 -type f              \
            \(                                           \
                 -name "exit_code"                       \
              -o -name "stdout"                          \
              -o -name "stderr"                          \
              -o -name "*.prof"                          \
              -o -name "*.eventlog"                      \
              -o -name "*.gcstats"                       \
              -o -name "*.log"                           \
            \)                                           \
            -printf \"%P\\n\"                            \
        |                                                \
          \"${tar_path}\" --create                       \
            --directory=\"${prog_dir}\" --files-from=-   \
        |                                                \
          \"${cat_path}\"                                \
        "
    ;;

    task-exec-tracer-folders-tar-zstd )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift

      local bash_path="$(jq -r ".containerPkgs.bashInteractive.\"nix-store-path\"" "${dir}"/container-specs.json)"/bin/bash
      local find_path="$(jq -r ".containerPkgs.findutils.\"nix-store-path\""       "${dir}"/container-specs.json)"/bin/find
      local tar_path="$(jq  -r ".containerPkgs.gnutar.\"nix-store-path\""          "${dir}"/container-specs.json)"/bin/tar
      local cat_path="$(jq  -r ".containerPkgs.coreutils.\"nix-store-path\""       "${dir}"/container-specs.json)"/bin/cat
      # TODO: Fetch the logRoot
      local log_root="$(jq -r ".containerPkgs.findutils.\"nix-store-path\""        "${dir}"/container-specs.json)"/bin/find
      local tracer_dir=/local/run/current/tracer/
      # TODO: Add compression, either "--zstd" or "--xz"
      # tar (child): zstd: Cannot exec: No such file or directory
      # tar (child): Error is not recoverable: exiting now
      # tar (child): xz: Cannot exec: No such file or directory
      # tar (child): Error is not recoverable: exiting now
      backend_nomad task-exec "${dir}" "${task}"         \
        "${bash_path}" -c                                \
        "                                                \
          \"${find_path}\" \"${tracer_dir}\"             \
            -mindepth 1 -type d                          \
            -printf \"%P\\n\"                            \
        |                                                \
          \"${tar_path}\" --create                       \
            --directory=\"${tracer_dir}\" --files-from=- \
        |                                                \
          \"${cat_path}\"                                \
        "
    ;;

    ## Nomad job tasks file queries
    ###############################

    task-entrypoint-stdout )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift

      local task_alloc_id
      task_alloc_id=$(wb_nomad job task-name-allocation-id \
        "$dir/nomad/nomad-job.json"                                   \
        "${task}")
      nomad alloc logs \
        "${task_alloc_id}" "${task}"
    ;;

    task-entrypoint-stderr )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift

      local task_alloc_id
      task_alloc_id=$(wb_nomad job task-name-allocation-id \
        "$dir/nomad/nomad-job.json"                                   \
        "${task}")
      nomad alloc logs -stderr \
        "${task_alloc_id}" "${task}"
    ;;

    task-file-contents )
      local usage="USAGE: wb backend pass $op RUN-DIR TASK-NAME PATH"
      local dir=${1:?$usage}; shift
      local task=${1:?$usage}; shift
      local path=${1:?$usage}; shift

      local task_alloc_id
      task_alloc_id=$(wb_nomad job task-name-allocation-id \
        "$dir/nomad/nomad-job.json"                                   \
        "${task}")
      nomad alloc fs "${task_alloc_id}" \
        /"${task}""${path}"             \
    ;;

    * )
      usage_nomadbackend
    ;;

  esac

}

# The Nomad agent supports multiple configuration files, which can be provided
# using the -config CLI flag. The flag can accept either a file or folder. In
# the case of a folder, any .hcl and .json files in the folder will be loaded
# and merged in lexicographical order. Directories are not loaded recursively.
#   -config=<path>
# The path to either a single config file or a directory of config files to use
# for configuring the Nomad agent. This option may be specified multiple times.
# If multiple config files are used, the values from each will be merged
# together. During merging, values from files found later in the list are merged
# over values from previously parsed file.

# Network Topology
# https://developer.hashicorp.com/nomad/docs/install/production/requirements#network-topology
# Nomad Deployment Guide
# https://developer.hashicorp.com/nomad/tutorials/enterprise/production-deployment-guide-vm-with-consul
# Deployment topology across multiple regions
# https://developer.hashicorp.com/nomad/tutorials/enterprise/production-reference-architecture-vm-with-consul#multi-region

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
nomad_create_server_config() {
  local name=$1
  local http_port=$2 rpc_port=$3 serv_port=$4
  local state_dir=$(wb_nomad server state-dir-path "${name}")
  local config_file=$(wb_nomad server config-file-path "${name}")
  # Config:
  # - Nomad agent configuration docs:
  # - - https://developer.hashicorp.com/nomad/docs/configuration
  # - Server block:
  # - - https://developer.hashicorp.com/nomad/docs/configuration/server
  cat > "${config_file}" <<- EOF
# Names:
########
# Specifies the region the Nomad agent is a member of. A region typically maps
# to a geographic region, for example us, with potentially multiple zones, which
# map to datacenters such as us-west and us-east.
region = "global"
# Specifies the data center of the local agent. All members of a datacenter
# should share a local LAN connection.
# Use one of "eu-central-1", "eu-west-1" or "us-east-2" to mimic SRE
datacenter = "eu-central-1"
# Specifies the name of the local node. This value is used to identify
# individual agents. When specified on a server, the name must be unique within
# the region.
name = "workbench-nomad-server-${name}"

# Paths:
########
# Specifies a local directory used to store agent state. Client nodes use this
# directory by default to store temporary allocation data as well as cluster
# information. Server nodes use this directory to store cluster state, including
# the replicated log and snapshot data. This must be specified as an absolute
# path.
data_dir  = "${state_dir}/data"

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
  http = ${http_port}
  # The port used for internal RPC communication between agents and servers, and
  # for inter-server traffic for the consensus algorithm (raft).
  rpc  = ${rpc_port}
  # The port used for the gossip protocol for cluster membership. Both TCP and
  # UDP should be routable between the server nodes on this port.
  serf = ${serv_port}
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
# Does not make ses to use advertise here or this way, but if not used (IDK):
# > Failed to parse HTTP advertise address (, 127.0.0.1, 4646, false):
# > Defaulting advertise to localhost is unsafe, please set advertise manually
advertise {
  # The address to advertise for the HTTP interface. This should be reachable by
  # all the nodes from which end users are going to use the Nomad CLI tools.
  http = "127.0.0.1"
  # The address used to advertise to Nomad clients for connecting to Nomad
  # servers for RPC. This allows Nomad clients to connect to Nomad servers from
  # behind a NAT gateway. This address much be reachable by all Nomad client
  # nodes. When set, the Nomad servers will use the advertise.serf address for
  # RPC connections amongst themselves. Setting this value on a Nomad client has
  # no effect.
  rpc = "127.0.0.1"
  # The address advertised for the gossip layer. This address must be reachable
  # from all server nodes. It is not required that clients can reach this
  # address. Nomad servers will communicate to each other over RPC using the
  # advertised Serf IP and advertised RPC Port.
  serf = "127.0.0.1"
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
log_file = "${state_dir}/nomad.log"
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

# Client:
#########
# Specifies configuration which is specific to the Nomad client.
# https://developer.hashicorp.com/nomad/docs/configuration/client
client {
  # Specifies if client mode is enabled. All other client configuration options
  # depend on this value.
  enabled = false
}

# Server:
#########
# Specifies configuration which is specific to the Nomad server.
# https://developer.hashicorp.com/nomad/docs/configuration/server
server {
  # Specifies if this agent should run in server mode. All other server options depend on this value being set.
  enabled = true
  # Specifies the directory to use for server-specific data, including the
  # replicated log. By default, this is the top-level "data_dir" suffixed with
  # "server", like "/opt/nomad/server". The top-level option must be set, even
  # when setting this value. This must be an absolute path.
  data_dir = "${state_dir}/data/server"
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

nomad_create_client_config() {
  local name=$1
  local http_port=$2 rpc_port=$3 serv_port=$4
  local nomad_task_driver=$5
  if test "${nomad_task_driver}" = "podman"
  then
    local podman_socket_path=$6
  else
    local podman_socket_path=""
  fi
  local cni_plugins_path=$(dirname $(which bridge))
  local state_dir=$(wb_nomad client state-dir-path "${name}")
  local config_file=$(wb_nomad client config-file-path "${name}")
  # Look for the running servers to connect to ("wired" in the config file).
  local servers_addresses=""
  local nomad_servers_dir="$(wb_nomad dir-path server)"
  for server_name in $(ls "${nomad_servers_dir}"); do
    if wb_nomad server is-running "${server_name}"
    then
      local port=$(wb_nomad server port rpc "${server_name}")
      if test -z "${servers_addresses}"
      then
        servers_addresses="${servers_addresses} \"127.0.0.1:${port}\""
      else
        servers_addresses="${servers_addresses}, \"127.0.0.1:${port}\""
      fi
    fi
  done
  # Config:
  # - Nomad agent configuration docs:
  # - - https://developer.hashicorp.com/nomad/docs/configuration
  # - Client block:
  # - - https://developer.hashicorp.com/nomad/docs/configuration/client
  # - Generic `nomad` plugins / task drivers configuration docs:
  # - - https://www.nomadproject.io/plugins/drivers
  # - - https://www.nomadproject.io/docs/configuration/plugin
  # - Specific `nomad` `podman` plugin / task driver configuration docs:
  # - - https://www.nomadproject.io/plugins/drivers/podman#plugin-options
  # - - https://github.com/hashicorp/nomad-driver-podman#driver-configuration
  cat > "${config_file}" <<- EOF
# Names:
########
# Specifies the region the Nomad agent is a member of. A region typically maps
# to a geographic region, for example us, with potentially multiple zones, which
# map to datacenters such as us-west and us-east.
region = "global"
# Specifies the data center of the local agent. All members of a datacenter
# should share a local LAN connection.
# Use one of "eu-central-1", "eu-west-1" or "us-east-2" to mimic SRE
datacenter = "eu-central-1"
# Specifies the name of the local node. This value is used to identify
# individual agents. When specified on a server, the name must be unique within
# the region.
name = "workbench-nomad-client-${name}"

# Paths:
########
# Specifies a local directory used to store agent state. Client nodes use this
# directory by default to store temporary allocation data as well as cluster
# information. Server nodes use this directory to store cluster state, including
# the replicated log and snapshot data. This must be specified as an absolute
# path.
data_dir  = "${state_dir}/data"
# Specifies the directory to use for looking up plugins. By default, this is the
# top-level data_dir suffixed with "plugins", like "/opt/nomad/plugins". This
# must be an absolute path.
plugin_dir  = "${state_dir}/data/plugins"

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
  http = ${http_port}
  # The port used for internal RPC communication between agents and servers, and
  # for inter-server traffic for the consensus algorithm (raft).
  rpc  = ${rpc_port}
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
# Does not make ses to use advertise here or this way, but if not used (IDK):
# > Failed to parse HTTP advertise address (, 127.0.0.1, 4646, false):
# > Defaulting advertise to localhost is unsafe, please set advertise manually
# Same thing for the RPC address, it's needed for the client.
advertise {
  # The address to advertise for the HTTP interface. This should be reachable by
  # all the nodes from which end users are going to use the Nomad CLI tools.
  http = "127.0.0.1"
  # The address used to advertise to Nomad clients for connecting to Nomad
  # servers for RPC. This allows Nomad clients to connect to Nomad servers from
  # behind a NAT gateway. This address much be reachable by all Nomad client
  # nodes. When set, the Nomad servers will use the advertise.serf address for
  # RPC connections amongst themselves. Setting this value on a Nomad client has
  # no effect.
  rpc = "127.0.0.1"
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
log_file = "${state_dir}/nomad.log"
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
# Specifies configuration which is specific to the Nomad server.
# https://developer.hashicorp.com/nomad/docs/configuration/server
server {
  # Specifies if this agent should run in server mode. All other server options
  # depend on this value being set.
  enabled = false
}

# Client:
#########
# Specifies configuration which is specific to the Nomad client.
# https://developer.hashicorp.com/nomad/docs/configuration/client
client {
  # Specifies if client mode is enabled. All other client configuration options
  # depend on this value.
  enabled = true
  # Specifies the directory to use for allocation data. By default, this is the
  # top-level data_dir suffixed with "alloc", like "/opt/nomad/alloc". This must
  # be an absolute path.
  alloc_dir = "${state_dir}/data/alloc"
  # Specifies the directory to use to store client state. By default, this is
  # the top-level "data_dir" suffixed with "client", like "/opt/nomad/client".
  # This must be an absolute path.
  state_dir = "${state_dir}/data/client"
  # Specifies an array of addresses to the Nomad servers this client should join.
  # This list is used to register the client with the server nodes and advertise
  # the available resources so that the agent can receive work. This may be
  # specified as an IP address or DNS, with or without the port. If the port is
  # omitted, the default port of 4647 is used.
  servers = [ ${servers_addresses} ]
  # Sets the search path that is used for CNI plugin discovery. Multiple paths can
  # be searched using colon delimited paths
  cni_path = "${cni_plugins_path}"
  # Specifies the maximum amount of time a job is allowed to wait to exit.
  # Individual jobs may customize their own kill timeout, but it may not exceed
  # this value.
  max_kill_timeout = "30s"
  # Specifies the interval at which Nomad attempts to garbage collect terminal
  # allocation directories.
  gc_interval = "2s"

  # Specifies an arbitrary string used to logically group client nodes by
  # user-defined class. This can be used during job placement as a filter.
  node_class = "qa" # Using the "world.dev.cardano.org" testing class for "perf".

  # "artifact" parameters (fail fast!!!)
  ######################################
  # https://developer.hashicorp.com/nomad/docs/configuration/client#artifact-parameters
  artifact {
    # Specifies the maximum duration in which an HTTP download request must complete
    # before it is canceled. Set to 0 to not enforce a limit.
    http_read_timeout = "5s"
    # Specifies the maximum duration in which a Google Cloud Storate operation must
    # complete before it is canceled. Set to 0 to not enforce a limit.
    gcs_timeout = "5s"
    # Specifies the maximum duration in which a Git operation must complete before
    # it is canceled. Set to 0 to not enforce a limit.
    git_timeout = "5s"
    # Specifies the maximum duration in which a Mercurial operation must complete
    # before it is canceled. Set to 0 to not enforce a limit.
    hg_timeout = "5s"
    # Specifies the maximum duration in which an S3 operation must complete before
    # it is canceled. Set to 0 to not enforce a limit.
    s3_timeout = "5s"
    # Specifies whether filesystem isolation should be disabled for artifact
    # downloads. Applies only to systems where filesystem isolation via landlock
    # is possible (Linux kernel 5.13+).
    disable_filesystem_isolation = false
  }

  # "template" parameters (fail fast!!!)
  ######################################
  # # https://developer.hashicorp.com/nomad/docs/configuration/client#template-parameters
  template {
    # This controls the retry behavior when an error is returned from Consul.
    # The template runner will not exit in the face of failure. Instead, it uses
    # exponential back-off and retry functions to wait for the Consul cluster to
    # become available, as is customary in distributed systems.
    consul_retry {
      # This specifies the number of attempts to make before giving up. Each
      # attempt adds the exponential backoff sleep time. Setting this to
      # zero will implement an unlimited number of retries.
      attempts = 1
      # This is the base amount of time to sleep between retry attempts. Each
      # retry sleeps for an exponent of 2 longer than this base. For 5 retries,
      # the sleep times would be: 250ms, 500ms, 1s, 2s, then 4s.
      backoff = "250ms"
      # This is the maximum amount of time to sleep between retry attempts.
      # When max_backoff is set to zero, there is no upper limit to the
      # exponential sleep between retry attempts.
      # If max_backoff is set to 10s and backoff is set to 1s, sleep times
      # would be: 1s, 2s, 4s, 8s, 10s, 10s, ...
      max_backoff = "1m"
    }
    # This controls the retry behavior when an error is returned from Vault.
    # Consul Template is highly fault tolerant, meaning it does not exit in the
    # face of failure. Instead, it uses exponential back-off and retry functions
    # to wait for the cluster to become available, as is customary in
    # distributed systems.
    vault_retry {
      # This specifies the number of attempts to make before giving up. Each
      # attempt adds the exponential backoff sleep time. Setting this to
      # zero will implement an unlimited number of retries.
      attempts = 1
      # This is the base amount of time to sleep between retry attempts. Each
      # retry sleeps for an exponent of 2 longer than this base. For 5 retries,
      # the sleep times would be: 250ms, 500ms, 1s, 2s, then 4s.
      backoff = "250ms"
      # This is the maximum amount of time to sleep between retry attempts.
      # When max_backoff is set to zero, there is no upper limit to the
      # exponential sleep between retry attempts.
      # If max_backoff is set to 10s and backoff is set to 1s, sleep times
      # would be: 1s, 2s, 4s, 8s, 10s, 10s, ...
      max_backoff = "1m"
    }
    # This controls the retry behavior when an error is returned from Nomad.
    # Consul Template is highly fault tolerant, meaning it does not exit in the
    # face of failure. Instead, it uses exponential back-off and retry functions
    # to wait for the cluster to become available, as is customary in
    # distributed systems.
    nomad_retry {
      # This specifies the number of attempts to make before giving up. Each
      # attempt adds the exponential backoff sleep time. Setting this to
      # zero will implement an unlimited number of retries.
      attempts = 1
      # This is the base amount of time to sleep between retry attempts. Each
      # retry sleeps for an exponent of 2 longer than this base. For 5 retries,
      # the sleep times would be: 250ms, 500ms, 1s, 2s, then 4s.
      backoff = "250ms"
      # This is the maximum amount of time to sleep between retry attempts.
      # When max_backoff is set to zero, there is no upper limit to the
      # exponential sleep between retry attempts.
      # If max_backoff is set to 10s and backoff is set to 1s, sleep times
      # would be: 1s, 2s, 4s, 8s, 10s, 10s, ...
      max_backoff = "1m"
    }
    # Allows templates access to arbitrary files on the client host via the file
    # function. By default, templates can access files only within the task
    # working directory.
    disable_file_sandbox = false
  }

}

EOF

# TODO: Host Network
# https://developer.hashicorp.com/nomad/docs/configuration/client#host_network-block

if test -n "${podman_socket_path}"
then
  cat >> "${config_file}" <<- EOF
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

EOF
else
  cat >> "${config_file}" <<- EOF
# TODO: Make the exec plugin config optional ???
plugin "exec" {
  # Defaults to "private". Set to "private" to enable PID namespace isolation
  # for tasks by default, or "host" to disable isolation.
  # Warning: If set to "host", other processes running as the same user will be
  # able to access sensitive process information like environment variables.
  default_pid_mode = "host"
  # Defaults to "private". Set to "private" to enable IPC namespace isolation
  # for tasks by default, or "host" to disable isolation.
  # Warning: If set to "host", other processes running as the same user will be
  # able to make use of IPC features, like sending unexpected POSIX signals.
  default_ipc_mode = "host"
  # Defaults to false. When true, the driver uses chroot for file system
  # isolation without pivot_root. This is useful for systems where the root is
  # on a ramdisk.
  no_pivot_root = false
  # A list of allowed Linux capabilities.
  # Defaults to:
  # ["audit_write", "chown", "dac_override", "fowner", "fsetid", "kill",
  #  "mknod", "net_bind_service", "setfcap", "setgid", "setpcap", "setuid",
  #  "sys_chroot"
  # ]
  # which is modeled after the capabilities allowed by docker by default
  # (without NET_RAW). Allows the operator to control which capabilities can be
  # obtained by tasks using cap_add and cap_drop options. Supports the value
  # "all" as a shortcut for allow-listing all capabilities supported by the
  # operating system.
  # Warning: Allowing more capabilities beyond the default may lead to
  # undesirable consequences, including untrusted tasks being able to compromise
  # the host system.
  # https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities
  allow_caps = [ "kill", "mknod", "net_bind_service" ]
}

EOF
fi

cat >> "${config_file}" <<- EOF
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

###############################################################################
# Debugging ###################################################################
###############################################################################

if test -n "${NOMAD_DEBUG:-}"
then
  exec 5> "$(dirname "$(readlink -f "$0")")"/nomad.sh.debug
  BASH_XTRACEFD="5"
  PS4='$LINENO: '
  set -x
fi

debugMsg() {
  if test -n "${NOMAD_DEBUG:-}"
  then
    >&2 echo -e "\n\n\t\t----------$1\n"
  fi
}
