usage_nomadcloud() {
  # Using a unique help message for all Nomad "sub-backends"
  usage_nomad
}

backend_nomadcloud() {

  op=${1:?$(usage_nomadcloud)}; shift

  case "${op}" in

    name )
      # Can be:
      # nomadpodman       (Using podman Task Driver in the cloud is not planned)
      # nomadexec  (Starts Nomad Agents supporting the "nix_installable" stanza)
      # nomadcloud    (SRE managed Nomad Agents on Amazon S3 (dedicated or not))
      echo 'nomadcloud'
    ;;

    # Overrided backend "methods"

    # All sub-backends set these same jq envars.
    setenv-defaults )
      local usage="USAGE: wb backend $op BACKEND-DIR"
      local backend_dir=${1:?$usage}; shift
      # Repeated code / envars set by all sub-backends
      setenvjqstr 'nomad_task_driver'    "exec"
      setenvjqstr 'nomad_environment'    "cloud"
      setenvjqstr 'one_tracer_per_node'  "true"
      # Cloud runs always run the generator inside Nomad Task "explorer"
      setenvjqstr 'generator_task_name'           \
        "$(                                       \
          jq -r                                   \
            .nomadJob.generatorTaskName           \
            "${backend_dir}"/container-specs.json \
        )"
      # Store the location of the Nix-built "container-specs" file.
      # TODO/FIXME: This is the only way to be able to later copy it to "$dir" ?
      setenvjqstr 'profile_container_specs_file' \
        "${backend_dir}"/container-specs.json
      # It "overrides" completely `backend_nomad`'s `setenv-defaults`.
      setenv-defaults-nomadcloud         "${backend_dir}"
    ;;

    allocate-run )
      # Default allocation before calling the backend specific allocation.
      backend_nomad allocate-run            "$@"
      allocate-run-nomadcloud               "$@"
    ;;

    # Called by `run.sh` without exit trap (unlike `scenario_setup_exit_trap`)!
    start-cluster )
      local dir=${1:?$usage};
      backend_nomad start-cluster           "$@"
      # If value/plutus profile topology on the dedicated P&T Nomad cluster on
      # AWS extra checks are run to make sure the topology that was deployed is
      # the correct one.
      # (For this the clients.json / NOMAD_CLIENTS_FILE file is needed)
      if \
            echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"               \
         &&                                                                       \
            jqtest '.composition.topology == "torus-dense"' "${dir}"/profile.json \
         &&                                                                       \
            jqtest '.composition.n_hosts  == 52'            "${dir}"/profile.json
      then
        # Shows a big warning but lets the run continue on mismatch!!!
        check-deployment "${dir}"
      fi
    ;;

    # Called by `run.sh` without exit trap (unlike `scenario_setup_exit_trap`)!
    deploy-genesis )
      # It "overrides" completely `backend_nomad`'s `deploy-genesis`.
      deploy-genesis-nomadcloud             "$@"
    ;;

    wait-pools-stopped )
      # It passes the sleep time (in seconds) required argument.
      # This time is different between local and cloud backends to avoid
      # unnecesary Nomad specific traffic (~99% happens waiting for node-0, the
      # first one it waits to stop inside a loop) and at the same time be less
      # sensitive to network failures.
      backend_nomad wait-pools-stopped     60 "$@"
    ;;

    wait-latencies-stopped )
      # It passes the sleep time (in seconds) required argument.
      # This time is different between local and cloud backends to avoid
      # unnecesary Nomad specific traffic (~99% happens waiting for node-0, the
      # first one it waits to stop inside a loop) and at the same time be less
      # sensitive to network failures.
      backend_nomad wait-latencies-stopped 60 "$@"
    ;;

    fetch-logs )
      local dir=${1:?$usage};
      # If not `nomad exec`, because we need to have a dedicated port open.
      if jqtest '.cluster.nomad.fetch_logs_ssh' "${dir}"/profile.json
      then
        # It "overrides" completely `backend_nomad`'s `fetch-logs`.
        fetch-logs-ssh                      "$@"
      else
        # Generic backend sub-commands, shared code between Nomad sub-backends.
        backend_nomad fetch-logs            "$@"
      fi
    ;;

    # All or clean up everything!
    # Called after `scenario.sh` without an exit trap!
    stop-cluster )
      local dir=${1:?$usage};
      if jqtest '.cluster.keep_running' "${dir}"/profile.json
      then
        # It "overrides" completely `backend_nomad`'s `stop-cluster`.
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift
        local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
        msg "$(yellow "This profile DOES NOT automatically stops and purges Nomad jobs")"
        msg "$(yellow "To stop the Nomad job use:")"
        msg "$(yellow "wb nomad job stop ${dir}/nomad/nomad-job.json ${nomad_job_name}")"
        msg "$(yellow "(With the same NOMAD_ADDR, NOMAD_NAMESPACE and NOMAD_TOKEN used for start-cluster)")"
      else
        # Shared code between Nomad sub-backends that internally only takes care
        # of the Nomad job.
        backend_nomad stop-cluster-internal "$@"
      fi
    ;;

    # Generic backend sub-commands, shared code between Nomad sub-backends.

    describe-run )
      backend_nomad describe-run            "$@"
    ;;

    is-running )
      backend_nomad is-running              "$@"
    ;;

    start-tracers )
      backend_nomad start-tracers           "$@"
    ;;

    start-nodes )
      backend_nomad start-nodes             "$@"
    ;;

    start-generator )
      backend_nomad start-generator         "$@"
    ;;

    start-healthchecks )
      backend_nomad start-healthchecks      "$@"
    ;;

    start-latencies )
      backend_nomad start-latencies         "$@"
    ;;

    start-node )
      backend_nomad start-node              "$@"
    ;;

    stop-node )
      backend_nomad stop-node               "$@"
    ;;

    get-node-socket-path )
      backend_nomad get-node-socket-path    "$@"
    ;;

    wait-node )
      backend_nomad wait-node               "$@"
    ;;

    wait-node-stopped )
      backend_nomad wait-node-stopped       "$@"
    ;;

    stop-all )
      backend_nomad stop-all                "$@"
    ;;

    cleanup-cluster )
      backend_nomad cleanup-cluster         "$@"
    ;;

    * )
      backend_nomad "${op}" "$@"
    ;;

  esac

}

# Sets the envars not shared by all the other sub-backends.
setenv-defaults-nomadcloud() {
  local backend_dir="${1}"

  local profile_container_specs_file
  profile_container_specs_file="${backend_dir}"/container-specs.json

  ##############
  # NOMAD_ADDR #
  ##############
  local nomad_addr
  if test -z "${NOMAD_ADDR+set}"
  then
    # The variable is not set, it's not set to an empty value, just not set!
    ########################################################################
    msg $(blue "INFO: Nomad address \"NOMAD_ADDR\" envar is not set")
  else
    # The variable is set and maybe empty!
    ######################################
    msg $(blue "INFO: Nomad address \"NOMAD_ADDR\" envar is \"${NOMAD_ADDR}\"")
  fi
  ###################
  # NOMAD_NAMESPACE #
  ###################
  local nomad_namespace
  nomad_namespace="$(jq -r .cluster.nomad.namespace "${WB_SHELL_PROFILE_DATA}"/profile.json)"
  if test -z ${NOMAD_NAMESPACE+set}
  then
    # The variable is not set, it's not set to an empty value, just not set!
    ########################################################################
    msg $(blue "INFO: Nomad namespace \"NOMAD_NAMESPACE\" envar is not set")
    export NOMAD_NAMESPACE="${nomad_namespace}"
    msg $(yellow "WARNING: Setting \"NOMAD_NAMESPACE\" to the namespace provided in the profile (\"${NOMAD_NAMESPACE}\")")
  else
    # The variable is set and maybe empty!
    ######################################
    msg $(blue "INFO: Nomad namespace \"NOMAD_NAMESPACE\" envar is \"${NOMAD_NAMESPACE}\"")
    if test "${NOMAD_NAMESPACE}" != "${nomad_namespace}"
    then
      fatal "Nomad namespace \"NOMAD_NAMESPACE\" envar is not \"${nomad_namespace}\""
    fi
  fi
  ###############
  # NOMAD_TOKEN #
  ###############
  if test -z "${NOMAD_TOKEN+set}"
  then
    # The variable is not set, it's not set to an empty value, just not set!
    ########################################################################
    msg $(blue "INFO: Nomad token \"NOMAD_TOKEN\" envar is not set")
    # We don't use tokens for the P&T cluster. Nothing else to do!
  else
    # The variable is set and maybe empty!
    ######################################
    if test -n "${NOMAD_TOKEN}"
    then
      fatal "A non-empty Nomad token \"NOMAD_TOKEN\" envar was provided but none is needed"
    fi
  fi
  #########
  # AWS_* #
  #########
  # Check all the AWS S3 envars needed for the HTTP PUT request
  # Using same names as the AWS CLI
  # https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-envvars.html
  if test -z "${AWS_ACCESS_KEY_ID:-}" || test -z "${AWS_SECRET_ACCESS_KEY:-}"
  then
    msg $(blue "INFO: Amazon S3 \"AWS_ACCESS_KEY_ID\" or \"AWS_SECRET_ACCESS_KEY\" envar is not set")
    msg $(yellow "WARNING: Fetching \"AWS_ACCESS_KEY_ID\" and \"AWS_SECRET_ACCESS_KEY\" from SRE provided Vault for \"Performance and Tracing\"")
    local aws_credentials
    aws_credentials="$(wb_nomad vault world aws-s3-credentials)"
    export AWS_ACCESS_KEY_ID=$(echo "${aws_credentials}" | jq -r .data.access_key)
    export AWS_SECRET_ACCESS_KEY=$(echo "${aws_credentials}" | jq -r .data.secret_key)
  fi
}

# Sub-backend specific allocs and calls `backend_nomad`'s `allocate-run`.
allocate-run-nomadcloud() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift

  # Copy the container specs file (container-specs.json)
  # This is the output file of the Nix derivation
  local profile_container_specs_file=$(envjqr 'profile_container_specs_file')
  # Create a nicely sorted and indented copy
  jq . "${profile_container_specs_file}" > "${dir}"/container-specs.json

  # Select which version of the Nomad job spec file we are running and
  # create a nicely sorted and indented copy in "nomad/nomad-job.json".
  # If not `nomad exec`, because we need to have a dedicated port open.
  if jqtest '.cluster.nomad.fetch_logs_ssh' "${dir}"/profile.json
  then
    jq -r ".nomadJob.cloud.ssh"                  \
      "${dir}"/container-specs.json              \
    > "${dir}"/nomad/nomad-job.json
  else
    # This avoids building some extra non-needed dependencies.
    jq -r ".nomadJob.cloud.nomadExec"            \
      "${dir}"/container-specs.json              \
    > "${dir}"/nomad/nomad-job.json
  fi

  local nomad_job_name=$(basename "${dir}") # `^[a-zA-Z0-9-]{1,128}$)`.
  local nomad_namespace
  nomad_namespace="$(jq -r .cluster.nomad.namespace "${dir}"/profile.json)"
  local nomad_class
  nomad_class="$(jq -r .cluster.nomad.class "${dir}"/profile.json)"

  # The job file is "slightly" modified (jq) to suit the running environment.
  ## Job Name
  ###########
  backend_nomad allocate-run-nomad-job-patch-name "${dir}" "${nomad_job_name}"
  ## - Namespace
  ##############
  if test -n "${nomad_namespace}" || ! test "${nomad_namespace}" = "null"
  then
    # This sets only the global namespace, the job level namespace. Not groups!
    backend_nomad allocate-run-nomad-job-patch-namespace "${dir}" "${nomad_namespace}"
  else
    # Empty the global namespace
    backend_nomad allocate-run-nomad-job-patch-namespace "${dir}"
  fi
  ## - Class
  ##########
  if test -n "${nomad_class}" || ! test "${nomad_class}" = "null"
  then
    ### Adds it as a group level contraint to all groups.
    local group_constraints_array
    group_constraints_array="                   \
      [                                         \
        {                                       \
          \"operator\":  \"=\"                  \
        , \"attribute\": \"\${node.class}\"     \
        , \"value\":     \"${nomad_class}\"     \
        }                                       \
      ]                                         \
    "
      jq \
        --argjson group_constraints_array "${group_constraints_array}"         \
        "                                                                      \
            .[\"job\"][\"${nomad_job_name}\"][\"group\"]                       \
          |=                                                                   \
            with_entries(.value.constraint |= (. + \$group_constraints_array)) \
        " \
        "${dir}"/nomad/nomad-job.json \
    | \
      sponge "${dir}"/nomad/nomad-job.json
  fi
  ## "nix_installables"
  #####################
  ### Will set the flake URIs from ".installable" in container-specs.json
  backend_nomad allocate-run-nomad-job-patch-nix "${dir}"

  # The Nomad job spec will contain links ("nix_installables" stanza) to
  # the Nix Flake outputs it needs inside the container, these are
  # refereced with a GitHub commit ID inside the "container-specs" file.
  local gitrev
  gitrev=$(jq -r .gitrev "${profile_container_specs_file}")
  msg $(blue "INFO: Found GitHub commit with ID \"$gitrev\"")
  # Check if the Nix package was created from a dirty git tree
  if test "$gitrev" = "0000000000000000000000000000000000000000"
  then
    fatal "Can't run a cluster in the Nomad cloud without a publicly accessible GitHub commit ID"
  else
    msg "Checking if GitHub commit \"$gitrev\" is publicly accessible ..."
    local curl_response
    # Makes `curl` return two objects, one with the body the other with
    # the headers, separated by a newline (`jq -s`).
    if curl_response=$(curl --silent --show-error --write-out '%{json}' https://api.github.com/repos/intersectmbo/cardano-node/commits/"${gitrev}")
    then
      # Check HTTP status code for existance
      # https://docs.github.com/en/rest/commits/commits?apiVersion=2022-11-28#get-a-commit
      local headers
      headers=$(echo "${curl_response}" | jq -s .[1])
      if test "$(echo "${headers}" | jq .http_code)" != 200
      then
        fatal "GitHub commit \"$gitrev\" is not available online!"
      fi
      # Show returned commit info in `git log` fashion
      local body author_name author_email author_date message
      body=$(echo "${curl_response}" | jq -s .[0])
      author_name=$(echo $body  | jq -r .commit.author.name)
      author_email=$(echo $body | jq -r .commit.author.email)
      author_date=$(echo $body  | jq -r .commit.author.date)
      message=$(echo $body      | jq -r .commit.message)
      msg $(green "commit ${gitrev}")
      msg $(green "Author: ${author_name} <${author_email}>")
      msg $(green "Date: ${author_date}")
      msg $(green "\n")
      msg $(green "\t${message}\n")
      msg $(green "\n")
    else
      fatal "Could not fetch commit info from GitHub (\`curl\` error)"
    fi
  fi

  ############################################################################
  # Memory/resources: ########################################################
  ############################################################################
  local producer_resources
  producer_resources="$(jq -r .cluster.nomad.resources.producer "${dir}"/profile.json)"
  # Set this for every non-explorer node
    jq \
      --argjson producer_resources "${producer_resources}" \
      "                                                                        \
          .[\"job\"][\"${nomad_job_name}\"][\"group\"]                         \
        |=                                                                     \
          with_entries(                                                        \
            if ( .key != \"explorer\" )                                        \
            then (                                                             \
                .value.task                                                    \
              |=                                                               \
                with_entries( .value.resources = \$producer_resources )        \
            ) else (                                                           \
              .                                                                \
            ) end                                                              \
          )                                                                    \
      " \
      "${dir}"/nomad/nomad-job.json \
  | \
    sponge "${dir}"/nomad/nomad-job.json
  if jqtest '.composition.with_explorer' "${dir}"/profile.json
  then
    local explorer_resources
    explorer_resources="$(jq -r .cluster.nomad.resources.explorer "${dir}"/profile.json)"
    # TODO/MAYBE: When not "value" profile, let the explorer run in any node?
    # resource wise. So more than one "ci-test", "ci-bench", "default" profile
    # can be run at the same time. This will need some changes to Nomad
    # services names (currently all "perf-node-#" and maybe "perf-tracer").
    # WARNING: By always using/placing the explorer node in the only machine
    # with more memory, we are sure runs do not overlap and no ports, etc are
    # clashing and interfering with benchmarks results!
      jq \
        --argjson resources "${explorer_resources}" \
        "                                                                        \
            .[\"job\"][\"${nomad_job_name}\"][\"group\"][\"explorer\"][\"task\"] \
          |=                                                                     \
            with_entries( .value.resources = \$resources )                       \
        " \
        "${dir}"/nomad/nomad-job.json \
    | \
      sponge "${dir}"/nomad/nomad-job.json
  fi

  ############################################################################
  # SSH Server: ##############################################################
  ############################################################################
  if jqtest '.cluster.nomad.fetch_logs_ssh' "${dir}"/profile.json
  then
    # Get or create the keys for the SSH servers and add them as templates.
    local template_json_srv template_json_usr
    template_json_srv="$( \
      ssh-key-template \
        "\"sshd.id_ed25519\""                                           \
        "\"$(cat "$(wb nomad ssh key server)" | sed -z 's/\n/\\n/g')\"" \
        "\"600\""                                                       \
    )"
    template_json_usr="$( \
      ssh-key-template \
        "\"nobody.id_ed25519.pub\""                 \
        "\"$(cat "$(wb nomad ssh key user)".pub)\"" \
        "\"644\""                                   \
    )"
    # For each Nomad Job Group
    local groups_array
    groups_array=$(jq -S -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"] | keys | join (\" \")" "${dir}"/nomad/nomad-job.json)
    for group_name in ${groups_array[*]}
    do
      # For each Nomad Job Group Task
      local tasks_array
      tasks_array=$(jq -S -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"task\"] | keys | join (\" \")" "${dir}"/nomad/nomad-job.json)
      for task_name in ${tasks_array[*]}
      do
        # Append the new templates.
          jq \
            --argjson template_json_srv "${template_json_srv}" \
            --argjson template_json_usr "${template_json_usr}" \
            " \
                .[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"task\"][\"${task_name}\"][\"template\"] \
              |= \
                ( . + [\$template_json_srv, \$template_json_usr]) \
            " \
            "${dir}"/nomad/nomad-job.json \
        | \
          sponge "${dir}"/nomad/nomad-job.json
      done
    done
  fi

  ##############################################################################
  # Reproducibility: ###########################################################
  ##############################################################################
  # We are using always the same placement!
  # This means node-N always runs on the same Nomad Client/AWS EC2 machine
  # For this a file with all the available Nomad Clients is needed!
  # This files is a list of Nomad Clients with a minimun of ".id", ".name"
  # ".class", ".datacenter", ".attributes.platform.aws["instance-type"]",
  # ".attributes.platform.aws.placement["availability-zone"]",
  # ".attributes.unique.platform.aws["instance-id"]",
  # ".attributes.unique.platform.aws.["public-ipv4"]"
  # ".attributes.unique.platform.aws.mac", ".attributes.cpu.modelname" and
  # ".attributes.kernel.version".
  # File is created with `wb nomad clients machines` and returns the machines
  # for the Nomad class of the actual profile (cluster.nomad.class).
  if test -z "${NOMAD_CLIENTS_FILE:-}" || ! test -f "${NOMAD_CLIENTS_FILE}"
  then
    fatal "No \"\$NOMAD_CLIENTS_FILE\". For reproducible runs provide this file that ensures cluster nodes are always placed on the same machines, or create a new one with 'wb nomad clients machines' if Nomad Client Nodes have suffered changes and runs fail with \"placement errors\""
  fi
  # Keep a copy of this run's file (Existance checked in `setenv-defaults`)!
  cp "${NOMAD_CLIENTS_FILE}" "${dir}"/nomad/clients.json
  # Get the AWS instance types from the profile.
  local instance_type_explorer instance_type_producer
  instance_type_explorer="$(jq -r .cluster.aws.instance_type.explorer "${dir}"/profile.json)"
  instance_type_producer="$(jq -r .cluster.aws.instance_type.producer "${dir}"/profile.json)"
  # Iterate thorugh all the unique regions available in the node-specs.json
  # file, this are all the regions the actual profile defines.
  local regions_array
  regions_array=$(jq \
    -r \
    'map(.region) | sort | unique | join (" ")' \
    "${dir}"/node-specs.json \
  )
  for region in ${regions_array[*]}
  do
    local count=0
    # For each regions, get all the node names that are available.
    local nodes_names_array
    # MUST be sorted to always get the same order for the same profile!
    # The only realiable way to sort, used along the workbench, is the ".i".
    nodes_names_array=$(jq \
      -r \
      "map(select(.region == \"${region}\")) | sort_by(.i) | map(.name) | join (\" \")" \
      "${dir}"/node-specs.json \
    )
    for node_name in ${nodes_names_array[*]}
    do
      # Get the actual client for this datacenter/region and instance type.
      local actual_client
      # There may be only one "explorer" instance type!
      if \
            ! test "${node_name}" = "explorer" \
         || \
            test "${instance_type_explorer}" = "${instance_type_producer}"
      then
        # Sort first by name so if a Nomad client gets redeployed, replaced
        # by a new one with the same name, only that Task is placed in a
        # different EC2 machine instead of having random changes depending on
        # where the new UUID lands on the clients NOMAD_CLIENTS_FILE file.
        actual_client=$(jq \
          "   . \
            | \
              sort_by(.name, .id) \
            | \
              map(select(.class == \"${nomad_class}\")) \
            | \
              map(select(.datacenter == \"${region}\")) \
            | \
              map(select(.attributes.platform.aws[\"instance-type\"] == \"${instance_type_producer}\")) \
            | \
              .[${count}] \
          " \
          "${NOMAD_CLIENTS_FILE}" \
        )
        count=$(( count + 1 ))
      else
        actual_client=$(jq \
          "   . \
            | \
              sort_by(.name, .id) \
            | \
              map(select(.class == \"${nomad_class}\")) \
            | \
              map(select(.datacenter == \"${region}\")) \
            | \
              map(select(.attributes.platform.aws[\"instance-type\"] == \"${instance_type_explorer}\")) \
            | \
              .[0] \
          " \
          "${NOMAD_CLIENTS_FILE}" \
        )
      fi
      # Get all the properties we are properties we use to contraint the Tasks.
      local client_id client_name instance_id instance_type availability_zone public_ipv4 mac_address cpu_model kernel_version
      client_id="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.id' \
      )"
      client_name="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.name' \
      )"
      instance_type="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.attributes.platform.aws["instance-type"]' \
      )"
      availability_zone="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.attributes.platform.aws.placement["availability-zone"]' \
      )"
      instance_id="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.attributes.unique.platform.aws["instance-id"]' \
      )"
      public_ipv4="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.attributes.unique.platform.aws["public-ipv4"]' \
      )"
      mac_address="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.attributes.unique.platform.aws.mac' \
      )"
      cpu_model="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.attributes.cpu.modelname' \
      )"
      kernel_version="$( \
         echo "${actual_client}" \
        | \
          jq -r \
            '.attributes.kernel.version' \
      )"
      # Pin the actual node to an specific Nomad Client / AWS instance by
      # appending below constraints to the already there group constraints.
      # We pin it to a couple of AWS specifics attributes so if SRE changes
      # something related to Nomad Clients or AWS instances fail we may
      # hopefully notice it when the job fails to start (placement errors).
      msg "$(blue "INFO:") Nomad Task $(yellow "\"${node_name}\"") will be constrainted to $(yellow "AWS Instance ID \"${instance_id}\" with AZ \"${availability_zone}\"") running $(yellow "Nomad client node \"${client_name}\" (${client_id})")"
      local group_constraints_array_plus="
        [ \
            { \
              \"attribute\": \"\${node.unique.id}\" \
            , \"value\":     \"${client_id}\" \
            } \
          ,
            { \
              \"attribute\": \"\${node.unique.name}\" \
            , \"value\":     \"${client_name}\" \
            } \
          ,
            { \
              \"attribute\": \"\${attr.platform.aws.instance-type}\" \
            , \"value\":     \"${instance_type}\" \
            } \
          ,
            { \
              \"attribute\": \"\${attr.platform.aws.placement.availability-zone}\" \
            , \"value\":     \"${availability_zone}\" \
            } \
          ,
            { \
              \"attribute\": \"\${attr.unique.platform.aws.instance-id}\" \
            , \"value\":     \"${instance_id}\" \
            } \
          ,
            { \
              \"attribute\": \"\${attr.unique.platform.aws.public-ipv4}\" \
            , \"value\":     \"${public_ipv4}\" \
            } \
          ,
            { \
              \"attribute\": \"\${attr.unique.platform.aws.mac}\" \
            , \"value\":     \"${mac_address}\" \
            } \
          ,
            { \
              \"attribute\": \"\${attr.cpu.modelname}\" \
            , \"value\":     \"${cpu_model}\" \
            } \
          ,
            { \
              \"attribute\": \"\${attr.kernel.version}\" \
            , \"value\":     \"${kernel_version}\" \
            } \
        ] \
      "
        jq \
          --argjson group_constraints_array_plus "${group_constraints_array_plus}" \
          " \
              .[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${node_name}\"][\"constraint\"] \
            |= \
              ( . + \$group_constraints_array_plus) \
          " \
          "${dir}"/nomad/nomad-job.json \
      | \
        sponge "${dir}"/nomad/nomad-job.json
      # Check if there is enough free storage space available!
      if jqtest '.cluster.minimun_storage != null' "${dir}"/profile.json
      then
        local kb_free kb_needed
        kb_free="$(wb nomad clients storage-kb-available "${client_name}")"
        if test "${node_name}" = "explorer"
        then
          kb_needed="$(jq -r .cluster.minimun_storage.explorer "${dir}"/profile.json)"
        else
          kb_needed="$(jq -r .cluster.minimun_storage.producer "${dir}"/profile.json)"
        fi
        # This is just a warning message!
        if test "${kb_free}" -lt "${kb_needed}"
        then
          msg "$(yellow "WARNING: Nomad client \"${client_name}\" (${client_id}) has less than ${kb_needed} bytes of storage available")"
          read -p "Hit enter to continue ..."
        fi
      fi
      # Store this group's reproducibility constraints for debugging purposes.
        jq \
          ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${node_name}\"][\"constraint\"]" \
          "${dir}"/nomad/nomad-job.json \
      > "${dir}"/nomad/"${node_name}"/constraints.json
    done
  done
  # Store the job's reproducibility constraints for debugging purposes.
    jq \
      ".[\"job\"][\"${nomad_job_name}\"][\"constraint\"]" \
      "${dir}"/nomad/nomad-job.json \
  > "${dir}"/nomad/constraints.json

  # Store a summary of the job.
  jq \
    --argjson nomad_job_name "\"${nomad_job_name}\"" \
    '{
        "namespace":   ( .job[ $nomad_job_name ].namespace   )
      , "datacenters": ( .job[ $nomad_job_name ].datacenters )
      , "constraint":  ( .job[ $nomad_job_name ].constraint  )
      , "groups":      (
            .job[ $nomad_job_name ].group
          | with_entries(
              .value |= {
                  "constraint": .constraint
                , "affinity":   .affinity
                , "tasks":      (
                    .task | with_entries(
                      .value |= {
                          "constraint":         .constraint
                        , "resources":          .resources
                        , "nix_installables":   .config.nix_installables
                        , "templates":        ( .template | map(.destination) )
                      }
                    )
                  )
              }
            )
        )
    }' \
    "${dir}"/nomad/nomad-job.json \
  > "${dir}"/nomad/nomad-job.summary.json
}

check-deployment() {
  local usage="USAGE: check-deployment RUN-DIR"
  local dir=${1:?$usage}; shift

  # Can only be created if `"${dir}"/nomad/clients.json` exists
  # (Requested by `allocate-run`).
  msg "Creating ex-post node-specs.json and topology.json files ..."
  local jobs_array=()
  # A node-specs.json like file with only "i", "name", "region", "port"
  # but adds a "nomad-client" object with "id", "name", "az" and "ip".
      wb_nomad job node-specs "${dir}"/nomad/nomad-job.json \
    > "${dir}"/nomad/node-specs.json                        \
  &
  jobs_array+=("$!")
  # A topology.json like file with only "nodeId", "name", "region" and
  # the list of "producers" that is re-constructed.
      wb_nomad job topology   "${dir}"/nomad/nomad-job.json \
    > "${dir}"/nomad/topology.json                          \
  &
  jobs_array+=("$!")
  if ! wait_kill_em_all "${jobs_array[@]}"
  then
    return 1
  fi

  # An easy to compare .csv version of the topology.
    jq -r \
      '
        .coreNodes as $nodes | $nodes | map(
            .name as $name
          | .region as $region
          | .producers | map( . as $prodName |
              $name
            + ","
            + $region[0:2]
            + ","
            + $prodName
            + ","
            + ($nodes | map(select(.name == $prodName))[0] | .region[0:2])
          )
        ) | .[] | .[]
      '                             \
      "${dir}"/nomad/topology.json  \
  | sort --version-sort           \
  > "${dir}"/nomad/topology.csv

  local node_specs_filter='
      map( {i: .i, name: .name, region: .region[0:2], port: .port} )
    | sort_by(.i)
  '
  local node_specs_ante node_specs_post
  node_specs_ante="$(jq "${node_specs_filter}" "${dir}"/node-specs.json)"
  node_specs_post="$(jq "${node_specs_filter}" "${dir}"/nomad/node-specs.json)"
  if ! test "${node_specs_ante}" = "${node_specs_post}"
  then
    echo "${node_specs_ante}" > "${dir}"/node-specs.ante.json
    echo "${node_specs_post}" > "${dir}"/node-specs.post.json
    dyff between "${dir}"/node-specs.ante.json "${dir}"/node-specs.post.json
    msg "$(red "----------")"
    msg "$(red "REQUESTED AND DEPLOYED node-specs.json DO NOT MATCH")"
    msg "$(red "----------")"
  fi
  local topology_filter='
      .coreNodes
    | map({
          name:      .name
        , nodeId:    .nodeId
        , region:    .region[0:2]
        , producers: (.producers | sort)
      })
  '
  local topology_ante topology_post
  topology_ante="$(jq "${topology_filter}" "${dir}"/topology.json)"
  topology_post="$(jq "${topology_filter}" "${dir}"/nomad/topology.json)"
  if ! test "${topology_ante}" = "${topology_post}"
  then
    echo "${topology_ante}" > "${dir}"/topology.ante.json
    echo "${topology_post}" > "${dir}"/topology.post.json
    dyff between "${dir}"/topology.ante.json "${dir}"/topology.post.json
    msg "$(red "----------")"
    msg "$(red "REQUESTED AND DEPLOYED topology.json DO NOT MATCH")"
    msg "$(red "----------")"
  fi
}

# Called by `run.sh` without exit trap (unlike `scenario_setup_exit_trap`)!
deploy-genesis-nomadcloud() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift
  local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)

  # Create genesis tar file
  local genesis_file_name="${nomad_job_name}.tar.zst"
  msg "$(blue Creating) $(yellow "\"${genesis_file_name}\"") ..."
  # TODO: These files are link to file that don't exist!
  rm "${dir}"/genesis/profile.json
  rm "${dir}"/genesis/stake-delegator-keys
  find -L "${dir}"/genesis -printf "%P\n"         \
    | tar --create --zstd                         \
      --dereference --hard-dereference            \
      --file="${dir}"/"${genesis_file_name}"      \
      --owner=65534 --group=65534 --mode="u=rwx"  \
      --directory="${dir}"/genesis --files-from=-

  # Upload genesis tar file
  local s3_region="eu-central-1"
  local s3_host="s3.${s3_region}.amazonaws.com";
  local s3_bucket_name="iog-cardano-perf";
  local s3_access_key="${AWS_ACCESS_KEY_ID}";
  local s3_access_key_secret="${AWS_SECRET_ACCESS_KEY}"
  local s3_storage_class="STANDARD"
  local return_code=0
  msg "$(blue Uploading) $(yellow "\"${genesis_file_name}\"") to $(yellow "\"s3://${s3_bucket_name}/\"") ..."
  aws s3 cp                                                               \
    "${dir}"/"${genesis_file_name}"                                       \
    s3://"${s3_bucket_name}"                                              \
    --content-type "application/zstd"                                     \
    --region "${s3_region}"                                               \
    --expected-size "$(stat --printf=%s "${dir}"/"${genesis_file_name}")" \
  || return_code="$?"
  # https://docs.aws.amazon.com/cli/latest/userguide/cli-services-s3-commands.html#using-s3-commands-managing-objects-copy
  # https://awscli.amazonaws.com/v2/documentation/api/latest/reference/s3/cp.html
  if test "${return_code}" = "0"
  then
    # A server response was obtained.
    msg "$(green "File \"${genesis_file_name}\" uploaded successfully")"
  else
    msg "$(red "FATAL: Upload to Amazon S3 failed")"
    # Already "fatal" -> ignore errors!
    backend_nomad stop-nomad-job "${dir}" || msg "$(red "Failed to stop Nomad Job")"
    fatal "Failed to upload genesis"
  fi

  # Generic download from every node.
  local uri="https://${s3_bucket_name}.${s3_host}/${genesis_file_name}"
  if ! backend_nomad deploy-genesis-wget "${dir}" "${uri}"
  then
    # File kept for debugging!
    msg "$(red "FATAL: deploy-genesis-wget \"${dir}\" \"${uri}\"")"
    # Already "fatal" -> ignore errors!
    backend_nomad stop-nomad-job "${dir}" || msg "$(red "Failed to stop Nomad Job")"
    fatal "Deploy of genesis \"${uri}\" failed"
  else
    msg "$(green "Genesis \"${uri}\" deployed successfully")"
    # Don't keep the file once ready, it's not free!
    msg "$(blue Removing) genesis file from $(yellow "\"s3://${s3_bucket_name}\"") ..."
    aws s3 rm                                         \
      s3://"${s3_bucket_name}"/"${genesis_file_name}" \
      --region "${s3_region}"                         \
    || true # Ignore errors when doing clean ups!
    # Reminder to remove old files.
    msg "Still available files at $(yellow "\"s3://${s3_bucket_name}\""):"
    aws s3 ls                   \
      s3://"${s3_bucket_name}"/ \
      --region "${s3_region}"   \
    || true # Ignore errors when doing clean ups!
  fi
}

# Only if running on dedicated P&T Nomad cluster on AWS we use SSH, if not
# `nomad exec`, because we need to have an exclusive port open for us.
fetch-logs-ssh() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift

  msg "Fetch logs ..."
  msg "First start the sandboxed SSH servers ..."
  local jobs_array=()
  local nodes=($(jq_tolist keys "${dir}"/node-specs.json))
  for node in ${nodes[*]}
  do
    # TODO: Do it in parallel ?
    backend_nomad task-program-start "${dir}" "${node}" ssh &
    jobs_array+=("$!")
  done
  # Wait and check!
  if test -n "${jobs_array}"
  then
    if ! wait_kill_em_all "${jobs_array[@]}"
    then
      fatal "Failed to start ssh server(s)"
    else
      msg "Sandboxed ssh server(s) should be now ready"
      # Make sure the SSH config file used to connect is already created.
      # Ugly but if `ssh` is called inmediately after `wb nomad ssh config`
      # race conditions can happen because the file contents are still in the
      # cache.
      local ssh_config_path
      ssh_config_path="$(wb nomad ssh config)"
      msg "Used ssh config file: $(realpath ${ssh_config_path})"
    fi
  fi
  fetch-logs-ssh-retry "${dir}"
  msg "Sandboxed SSH servers will be kept running for debugging purposes"
}

fetch-logs-ssh-retry() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift

  local jobs_array=()
  for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
  do
    if ! test -f "${dir}"/nomad/"${node}"/download_ok
    then
      fetch-logs-ssh-node "${dir}" "${node}" &
      jobs_array+=("$!")
    else
      msg "Skipping \"${node}\": check file \"${dir}/nomad/${node}/download_ok\""
    fi
  done
  if test -n "${jobs_array:-}" # If = () "unbound variable" error
  then
    # Wait until all jobs finish, don't use `wait_kill_em_all` that kills.
    # Returns the exit code of the last failed job, we ignore it!
    if ! wait_all "${jobs_array[@]}"
    then
      msg "$(red "Failed to fetch some logs")"
      msg "Check files \"${dir}/nomad/NODE/download_ok\" and \"${dir}/nomad/NODE/download_failed\""
      read -p "Hit enter to retry ..."
      fetch-logs-ssh-retry "${dir}"
    else
      msg "$(green "Finished fetching logs")"
    fi
  else
    msg "Nothing to do: check files in \"${dir}/nomad/NODE/download_ok\""
  fi
}

fetch-logs-ssh-node() {
  local dir=${1}
  local node=${2}

  local node_ok="true"
  # Non SSH logs
  ##############
  # These files are not downloaded using SSH.
  # Job's entrypoints logs (supervisord stdout and stderr) as Nomad sees them.
  if ! backend_nomad download-logs-entrypoint "${dir}" "${node}"
  then
    # Generic `download-logs-entrypoint` already provides error messages.
    node_ok="false"
    touch "${dir}"/nomad/"${node}"/download_failed
  fi
  # SSH logs ###
  ##############
  local node_id public_ipv4
  node_id="$( \
    jq -r .NodeID "${dir}"/nomad/nomad-job.json.run/task."${node}".final.json \
  )"
  public_ipv4="$(                                            \
      nomad node status -json "${node_id}"                   \
    |                                                        \
      jq -r .Attributes[\"unique.platform.aws.public-ipv4\"] \
  )"
  local ssh_config_path ssh_command
  ssh_config_path="$(wb nomad ssh config)"
  ssh_command="ssh -F ${ssh_config_path} -p 32000 -l nobody"
  # Download latency(ies) logs. ################################################
  ##############################################################################
  msg "$(blue "Fetching") $(yellow "program \"latency\"") run files from $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
  if ! rsync -e "${ssh_command}" -au                      \
         -f'- start.sh'                                   \
         "${public_ipv4}":/local/run/current/latency/     \
         "${dir}"/latency/"${node}"/
  then
    node_ok="false"
    touch "${dir}"/nomad/"${node}"/download_failed
    msg "$(red Error fetching) $(yellow "program \"latency\"") $(red "run files from") $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
  fi
  # Download healthcheck(s) logs. ##############################################
  ##############################################################################
  msg "$(blue "Fetching") $(yellow "program \"healthcheck\"") run files from $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
  if ! rsync -e "${ssh_command}" -au                      \
         -f'- start.sh'                                   \
         "${public_ipv4}":/local/run/current/healthcheck/ \
         "${dir}"/healthcheck/"${node}"/
  then
    node_ok="false"
    touch "${dir}"/nomad/"${node}"/download_failed
    msg "$(red Error fetching) $(yellow "program \"healthcheck\"") $(red "run files from") $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
  fi
  # Download generator logs. ###################################################
  ##############################################################################
  if test "${node}" = "explorer"
  then
    msg "$(blue Fetching) $(yellow "program \"generator\"") run files from $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
    if ! rsync -e "${ssh_command}" -au                    \
           -f'- start.sh' -f'- run-script.json'           \
           "${public_ipv4}":/local/run/current/generator/ \
           "${dir}"/generator/
    then
      node_ok="false"
      touch "${dir}"/nomad/"${node}"/download_failed
      msg "$(red Error fetching) $(yellow "program \"generator\"") $(red "run files from") $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
    fi
  fi
  # Download node(s) logs. #####################################################
  ##############################################################################
  msg "$(blue Fetching) $(yellow "program \"node\"") run files from $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
  if ! rsync -e "${ssh_command}" -au                          \
         -f'- start.sh' -f'- config.json' -f'- topology.json' \
         -f'- node.socket' -f'- db/'                          \
         "${public_ipv4}":/local/run/current/"${node}"/       \
         "${dir}"/"${node}"/
  then
    node_ok="false"
    touch "${dir}"/nomad/"${node}"/download_failed
    msg "$(red Error fetching) $(yellow "program \"node\"") $(red "run files from") $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
  fi
  # Download tracer(s) logs. ###################################################
  ##############################################################################
  msg "$(blue Fetching) $(yellow "program \"tracer\"") run files from $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
  if ! rsync -e "${ssh_command}" -au                 \
         -f'- start.sh' -f'- config.json'            \
         -f'- tracer.socket' -f'- logRoot/'          \
         "${public_ipv4}":/local/run/current/tracer/ \
         "${dir}"/tracer/"${node}"/
  then
    node_ok="false"
    touch "${dir}"/nomad/"${node}"/download_failed
    msg "$(red Error fetching) $(yellow "program \"tracer\"") $(red "run files from") $(yellow "\"${node}\" (\"${public_ipv4}\")") ..."
  fi
  # Allow the user to do something if a download fails
  if ! test "${node_ok}" = "true"
  then
    msg "$(red "Failed fetching logs from") $(yellow "\"${node}\" (\"${public_ipv4}\")")"
    false
  else
    touch "${dir}"/nomad/"${node}"/download_ok
    msg "$(green "Finished fetching logs from") $(yellow "\"${node}\" (\"${public_ipv4}\")")"
    true
  fi
}

ssh-key-template() {
  local key_name=${1}
  local key_data=${2}
  local key_perm=${3}
  # TODO: Use {{ env \"NOMAD_META_TASK_STATEDIR\" }} instead of "/local/run/current"
  jq \
    --null-input                     \
    --argjson key_name "${key_name}" \
    --argjson key_data "${key_data}" \
    --argjson key_perm "${key_perm}" \
    '
      {
        "env":  false
      , "destination":          ("local/run/current/ssh/" + $key_name)
      , "data":                 $key_data
      , "change_mode":          "noop"
      , "error_on_missing_key": true
      , "perms":                $key_perm
      }
    '
}
