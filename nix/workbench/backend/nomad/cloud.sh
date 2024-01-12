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
      allocate-run-nomadcloud               "$@"
      # Does a pre allocation before calling the default/common allocation.
      backend_nomad allocate-run            "$@"
    ;;

    # Called by `run.sh` without exit trap (unlike `scenario_setup_exit_trap`)!
    start-cluster )
      backend_nomad start-cluster           "$@"
      # If value/plutus profile on the dedicated P&T Nomad cluster on AWS extra
      # checks to make sure the topology that was deployed is the correct one.
      if jqtest '.composition.topology == "torus-dense"' "${dir}"/profile.json
      then
        # Show a big warning but let the run continue!
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
      backend_nomad wait-pools-stopped   60 "$@"
    ;;

    fetch-logs )
      # Only if running on the dedicated P&T Nomad cluster on AWS we use SSH, if
      # not `nomad exec`, because we need to have a dedicated port open for us.
      if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
      then
        # It "overrides" completely `backend_nomad`'s `fetch-logs`.
        fetch-logs-nomadcloud               "$@"
      else
        # Generic backend sub-commands, shared code between Nomad sub-backends.
        backend_nomad fetch-logs            "$@"
      fi
    ;;

    # All or clean up everything!
    # Called after `scenario.sh` without an exit trap!
    stop-cluster )
      # Only when running on dedicated P&T Nomad cluster job is kept running!
      if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
      then
        # It "overrides" completely `backend_nomad`'s `stop-cluster`.
        local usage="USAGE: wb backend $op RUN-DIR"
        local dir=${1:?$usage}; shift
        local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
        msg "$(yellow "Cloud runs DO NOT automatically stop and purge Nomad jobs")"
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

# Sets jq envars ("profile_container_specs_file" ,"nomad_environment",
# "nomad_task_driver" and "one_tracer_per_node") and checks Nomad envars
# (NOMAD_ADDR, NOMAD_NAMESPACE, NOMAD_TOKEN).
setenv-defaults-nomadcloud() {
  local backend_dir="${1}"

  local profile_container_specs_file
  profile_container_specs_file="${backend_dir}"/container-specs.json

  # Nomad cloud profiles only available for Cardano World "qa" nodes
  # ("-nomadcwqa") or the P&T dedicated Nomad cluster ("-nomadperf")
  if                                                              \
        ! echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadcwqa" \
     &&                                                           \
        ! echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
  then
    fatal "Unknown profile for Nomad Cloud: \"${WB_SHELL_PROFILE}\""
  fi

  ##############
  # NOMAD_ADDR #
  ##############
  if test -z "${NOMAD_ADDR+set}"
  then
    # The variable is not set, it's not set to an empty value, just not set!
    ########################################################################
    msg $(blue "INFO: Nomad address \"NOMAD_ADDR\" envar is not set")
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadcwqa"
    then
      export NOMAD_ADDR="https://nomad.world.dev.cardano.org"
    fi
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
    then
      export NOMAD_ADDR="http://10.200.0.1:4646"
    fi
    msg $(yellow "WARNING: Setting \"NOMAD_ADDR\" to the SRE provided address for \"Performance and Tracing\" (\"${NOMAD_ADDR}\")")
  else
    # The variable is set and maybe empty!
    ######################################
    msg $(blue "INFO: Nomad address \"NOMAD_ADDR\" envar is \"${NOMAD_ADDR}\"")
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadcwqa"
    then
      if test "${NOMAD_ADDR}" != "https://nomad.world.dev.cardano.org"
      then
        fatal "Nomad address \"NOMAD_ADDR\" envar is not \"https://nomad.world.dev.cardano.org\""
      fi
    fi
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
    then
      if test "${NOMAD_ADDR}" != "http://10.200.0.1:4646"
      then
        fatal "Nomad address \"NOMAD_ADDR\" envar is not \"http://10.200.0.1:4646\""
      fi
    fi
  fi
  ###################
  # NOMAD_NAMESPACE #
  ###################
  if test -z ${NOMAD_NAMESPACE+set}
  then
    msg $(blue "INFO: Nomad namespace \"NOMAD_NAMESPACE\" envar is not set")
    # The variable is not set, it's not set to an empty value, just not set!
    ########################################################################
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadcwqa"
    then
      export NOMAD_NAMESPACE="perf"
      msg $(yellow "WARNING: Setting \"NOMAD_NAMESPACE\" to the SRE provided namespace for \"Performance and Tracing\" (\"${NOMAD_NAMESPACE}\")")
    fi
    # We don't use namespaces for the P&T cluster. Nothing else to do!
  else
    # The variable is set and maybe empty!
    ######################################
    msg $(blue "INFO: Nomad namespace \"NOMAD_NAMESPACE\" envar is \"${NOMAD_NAMESPACE}\"")
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadcwqa"
    then
      if test "${NOMAD_NAMESPACE}" != "perf"
      then
        fatal "Nomad namespace \"NOMAD_NAMESPACE\" envar is not \"perf\""
      fi
    fi
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
    then
      if test "${NOMAD_NAMESPACE}" != ""
      then
        fatal "Nomad namespace \"NOMAD_NAMESPACE\" envar is not empty"
      fi
    fi
  fi
  ###############
  # NOMAD_TOKEN #
  ###############
  if test -z "${NOMAD_TOKEN+set}"
  then
    msg $(blue "INFO: Nomad token \"NOMAD_TOKEN\" envar is not set")
    # The variable is not set, it's not set to an empty value, just not set!
    ########################################################################
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadcwqa"
    then
      export NOMAD_TOKEN="$(wb_nomad vault world nomad-token)"
      msg $(yellow "WARNING: Fetching a \"NOMAD_TOKEN\" from SRE provided Vault for \"Performance and Tracing\"")
    fi
    # We don't use tokens for the P&T cluster. Nothing else to do!
  else
    # The variable is set and maybe empty!
    ######################################
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadcwqa"
    then
      if test -z "${NOMAD_TOKEN}"
      then
        msg $(red "FATAL: Nomad token \"NOMAD_TOKEN\" envar is empty")
        fatal "If you need to fetch a NOMAD_TOKEN for world.dev.cardano.org don't set the envar"
      else
        msg $(blue "INFO: Using provided Nomad token \"NOMAD_TOKEN\" envar")
      fi
    fi
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
    then
      if test -n "${NOMAD_TOKEN}"
      then
        fatal "A non-empty Nomad token \"NOMAD_TOKEN\" envar was provided but none is needed"
      fi
    fi
  fi

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

  # Create nomad folder and copy the Nomad job spec file to run.
  mkdir -p "${dir}"/nomad
  # Select which version of the Nomad job spec file we are running and
  # create a nicely sorted and indented copy in "nomad/nomad-job.json".
  # Only if running on the dedicated P&T Nomad Cluster we use SSH, if not
  # `nomad exec`, because we need to have an exclusive port open for us
  if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
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
  # The job file is "slightly" modified (jq) to suit the running environment.
  if test -n "${NOMAD_NAMESPACE:-}"
  then
    # This sets only the global namespace, the job level namespace. Not groups!
    backend_nomad allocate-run-nomad-job-patch-namespace "${dir}" "${NOMAD_NAMESPACE}"
  else
    # Empty the global namespace
    backend_nomad allocate-run-nomad-job-patch-namespace "${dir}"
  fi
  # Will set the flake URIs from ".installable" in container-specs.json
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

  # Set the placement info and resources accordingly
  local nomad_job_name
  nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
  ##############################################################################
  # Profile name dependent changes #############################################
  ##############################################################################
  # "*-nomadperf" profiles only run on the dedicated P&T Nomad Cluster on AWS.
  # "*-nomadcwqa" (for example "ci-test-nomadcwqa" or "default-nomadcwqa") means
  # that they run on Cardano World Nomad cluster's nodes that belong to the "qa"
  # class, runs on these should be limited to short tests and must never use the
  # "infra" class where HA jobs runs.
  if test -z "${WB_SHELL_PROFILE:-}"
  then
    fatal "Envar \"WB_SHELL_PROFILE\" is empty!"
  else
    ############################################################################
    # Unique placement: ########################################################
    ############################################################################
    ## "distinct_hosts": Instructs the scheduler to not co-locate any groups
    ## on the same machine. When specified as a job constraint, it applies
    ## to all groups in the job. When specified as a group constraint, the
    ## effect is constrained to that group. This constraint can not be
    ## specified at the task level. Note that the attribute parameter should
    ## be omitted when using this constraint.
    ## https://developer.hashicorp.com/nomad/docs/job-specification/constraint#distinct_hosts
    local job_constraints_array
    job_constraints_array='
      [
        {
          "operator":  "distinct_hosts"
        , "value":     "true"
        }
      ]
    '
    # Adds it as an extra job level contraint.
      jq \
        --argjson job_constraints_array "${job_constraints_array}" \
        "                                                \
            .[\"job\"][\"${nomad_job_name}\"].constraint \
          |=                                             \
            (. + \$job_constraints_array)                \
        " \
        "${dir}"/nomad/nomad-job.json \
    | \
      sponge "${dir}"/nomad/nomad-job.json
    ############################################################################
    # Node class: ##############################################################
    ############################################################################
    local group_constraints_array
    # Nomad nodes that belong to the "perf" class are the default in the Job
    # definition and it stays like that unless the profile name contains
    # "-nomadcwqa", in this case we limit the usage of to "qa" class nodes (CI
    # dedicated) that are available for short runs.
    # We have also have to be careful that runs do not overlap. This is
    # automatically enforced because service names and resources definitions
    # currently won't allow that to happen.
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadcwqa"
    then
      # Using "qa" class distinct nodes. Only "short" test allowed here.
      group_constraints_array='
        [
          {
            "operator":  "="
          , "attribute": "${node.class}"
          , "value":     "qa"
          }
        ]
      '
    elif test -n "${NOMAD_NAMESPACE:-}"
    then
      # Use what was provided.
      # If no namespace all group level constraints will be emptied!
      group_constraints_array="                   \
        [                                         \
          {                                       \
            \"operator\":  \"=\"                  \
          , \"attribute\": \"\${node.class}\"     \
          , \"value\":     \"${NOMAD_NAMESPACE}\" \
          }                                       \
        ]                                         \
      "
    fi
    # Is there something to change related to group constraints ?
    # Sets or deletes all groups level constraints.
    if test -n "${group_constraints_array:-}"
    then
      # Adds it as a group level contraint to all groups replacing default ones.
        jq \
          --argjson group_constraints_array "${group_constraints_array}" \
          "                                                               \
              .[\"job\"][\"${nomad_job_name}\"][\"group\"]                \
            |=                                                            \
              with_entries(.value.constraint = \$group_constraints_array) \
          " \
          "${dir}"/nomad/nomad-job.json \
      | \
        sponge "${dir}"/nomad/nomad-job.json
    else
      # Else, empties all group level constraints, like previous namespaces.
        jq \
          "                                                \
              .[\"job\"][\"${nomad_job_name}\"][\"group\"] \
            |=                                             \
              with_entries(.value.constraint = null)       \
          " \
          "${dir}"/nomad/nomad-job.json \
      | \
        sponge "${dir}"/nomad/nomad-job.json
    fi
    ############################################################################
    # Memory/resources: ########################################################
    ############################################################################
    # Set the resources, only for perf exlusive cloud runs!
    # When not "-nomadperf", when "-nomadcwqa", only "short" tests are allowed
    # on whatever resources we are given.
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
    then
      # Producer nodes use this specs, make sure they are available!
      # AWS:
      ## c5.2xlarge: 8 vCPU and 16 Memory (GiB)
      ## https://aws.amazon.com/ec2/instance-types/c5/
      # Nomad:
      ## - cpu.arch:            = amd64
      ## - cpu.frequency:       = 3400
      ## - cpu.modelname:       = Intel(R) Xeon(R) Platinum 8275CL CPU @ 3.00GHz
      ## - cpu.numcores:        = 8
      ## - cpu.reservablecores: = 8
      ## - cpu.totalcompute:    = 27200
      ## - memory.totalbytes    = 16300142592
      ## Pesimistic: 1,798 MiB / 15,545 MiB Total
      ## Optimistic: 1,396 MiB / 15,545 MiB Total
      #
      # WARNING: Don't use more than roughly 15400, for example 15432, because
      # some clients show a couple bytes less available.
      local producer_resources='{
          "cores":      8
        , "memory":     15400
        , "memory_max": 32000
      }'
      # Set this for every non-explorer node
        jq \
          --argjson producer_resources "${producer_resources}" \
          "                                                                 \
              .[\"job\"][\"${nomad_job_name}\"][\"group\"]                  \
            |=                                                              \
              with_entries(                                                 \
                if ( .key != \"explorer\" )                                 \
                then (                                                      \
                    .value.task                                             \
                  |=                                                        \
                    with_entries( .value.resources = \$producer_resources ) \
                ) else (                                                    \
                  .                                                         \
                ) end                                                       \
              )                                                             \
          " \
          "${dir}"/nomad/nomad-job.json \
      | \
        sponge "${dir}"/nomad/nomad-job.json
      # The explorer node uses this specs, make sure they are available!
      # AWS
      ## m5.4xlarge: 8 vCPU and 16 Memory (GiB)
      ## https://aws.amazon.com/ec2/instance-types/m5/
      # Nomad:
      ## - cpu.arch             = amd64
      ## - cpu.frequency        = 3100
      ## - cpu.modelname        = Intel(R) Xeon(R) Platinum 8175M CPU @ 2.50GHz
      ## - cpu.numcores         = 16
      ## - cpu.reservablecores  = 16
      ## - cpu.totalcompute     = 54400
      ## - memory.totalbytes    = 65900154880
      # Node ID: 00db7a3a-a05b-7ae0-2b2a-b50b9db139a4
      # client named "ip-10-24-30-90.eu-central-1.compute.internal"
      local explorer_resources='{
          "cores":      16
        , "memory":     32000
        , "memory_max": 64000
      }'
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
    if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
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
    ########################################################################
    # Reproducibility: #####################################################
    ########################################################################
    # If value/plutus profile on "-nomadperf", using always the same placement!
    # This means node-N always runs on the same Nomad Client/AWS EC2 machine
    if jqtest '.composition.topology == "torus-dense"' "${dir}"/profile.json
    then
      # A file with all the available Nomad Clients is needed!
      # This files is a list of Nomad Clients with a minimun of ".id",
      # ".datacenter", ".attributes.platform.aws["instance-type"]",
      # ".attributes.platform.aws.placement["availability-zone"]",
      # ".attributes.unique.platform.aws["instance-id"]",
      # ".attributes.unique.platform.aws.["public-ipv4"]"
      # ".attributes.unique.platform.aws.mac", ".attributes.cpu.modelname" and
      # ".attributes.kernel.version".
      if test -z "${NOMAD_CLIENTS_FILE:-}" || ! test -f "${NOMAD_CLIENTS_FILE}"
      then
        fatal "No \"\$NOMAD_CLIENTS_FILE\". For reproducible builds provide this file that ensures cluster nodes are always placed on the same machines, or create a new one with 'wb nomad nodes' if Nomad Clients have suffered changes and runs fail with \"placement errors\""
      fi
      # Keep a copy of the file used for this run!
      cp "${NOMAD_CLIENTS_FILE}" "${dir}"/nomad/clients.json
      # For each (instance-type, datacener/region) we look incrementally for
      # the unique AWS EC2 "instance-id" only after ordering the Nomad
      # Clients by its unique Nomad provided "id".
      local count_ap=0 count_eu=0 count_us=0
      # For each Nomad Job Group
      local groups_array
      # Keys MUST be sorted to always get the same order for the same profile!
      # Bash's `sort --version-sort` to correctly sort "node-20" and "node-9".
      readarray -t groups_array < <(jq -S -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"] | keys | .[]" "${dir}"/nomad/nomad-job.json | sort --version-sort)
      for group_name in ${groups_array[*]}
      do
        # Obtain the datacenter as Nomad sees it, not as an AWS attribute.
        # For example "eu-central-1" instead of "eu-central-1a".
        # These values were corrected above.
        local datacenter
        datacenter=$(jq \
          -r \
          ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"].affinity.value" \
          "${dir}"/nomad/nomad-job.json \
        )
        # For each Nomad Job Group Task
        local tasks_array
        # Keys MUST be sorted to always get the same order for the same profile!
        # Bash's `sort --version-sort` to correctly sort "node-20" and "node-9".
        readarray -t tasks_array < <(jq -S -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"task\"] | keys | .[]" "${dir}"/nomad/nomad-job.json | sort --version-sort)
        for task_name in ${tasks_array[*]}
        do
          local count instance_type
          if test "${task_name}" = "explorer"
          then
            # There is only one of this instance!
            instance_type="m5.4xlarge"
            count=0
          else
            # There are many of these instances and we need to always fetch
            # them in the same order for reproducibility.
            instance_type="c5.2xlarge"
            if test "${datacenter}" = "ap-southeast-2"
            then
              count="${count_ap}"
              count_ap=$(( count_ap + 1 ))
            elif test "${datacenter}" = "eu-central-1"
            then
              count="${count_eu}"
              count_eu=$(( count_eu + 1 ))
            elif test "${datacenter}" = "us-east-1"
            then
              count="${count_us}"
              count_us=$(( count_us + 1 ))
            fi
          fi
          # Get the actual client for this datacenter and instance type.
          local actual_client
          # Sort first by name so if a Nomad client gets redeployed, replaced
          # by a new one with the same name, only that Task is placed in a
          # different EC2 machine instead of having random changes depending on
          # where the new UUID lands on the clients NOMAD_CLIENTS_FILE file.
          actual_client=$(jq \
            "   . \
              | \
                sort_by(.name, .id) \
              | \
                map(select(.datacenter == \"${datacenter}\")) \
              | \
                map(select(.attributes.platform.aws[\"instance-type\"] == \"${instance_type}\")) \
              | \
                .[${count}] \
            " \
            "${NOMAD_CLIENTS_FILE}" \
          )
          local instance_id availability_zone public_ipv4 mac_address cpu_model kernel_version
          instance_id="$( \
             echo "${actual_client}" \
            | \
              jq -r \
                '.attributes.unique.platform.aws["instance-id"]' \
          )"
          availability_zone="$( \
             echo "${actual_client}" \
            | \
              jq -r \
                '.attributes.platform.aws.placement["availability-zone"]' \
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
          # Pin the actual node to an specific Nomad Client / AWS instance
          # by appending below constraints to the already there group
          # constraints.
          # We pin it to a couple of AWS specifics attributes so if SRE
          # changes something related to Nomad Clients or AWS instances we
          # may hopefully notice it when the job fails to start (placement
          # errors).
          local group_constraints_array_plus="
            [ \
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
                  .[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"constraint\"] \
                |= \
                  ( . + \$group_constraints_array_plus) \
              " \
              "${dir}"/nomad/nomad-job.json \
          | \
            sponge "${dir}"/nomad/nomad-job.json
        done
      done
    # Else, if not value profile but still the P&T exclusive cluster, it's not
    # always the same exact placement, we just make sure regions are OK
    # When not "-nomadperf", when "-nomadcwqa", only "short" tests are allowed
    # on whatever resources we are given, regions are only an affinity.
    elif echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
    then
      local groups_array
      groups_array=$(jq -S -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"] | keys | sort | join (\" \")" "${dir}"/nomad/nomad-job.json)
      for group_name in ${groups_array[*]}
      do
        # Obtain the datacenter as Nomad sees it, not as an AWS attribute.
        # For example "eu-central-1" instead of "eu-central-1a".
        # These values were corrected above.
        local datacenter
        datacenter=$(jq \
          -r \
          ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"].affinity.value" \
          "${dir}"/nomad/nomad-job.json \
        )
        local group_constraints_array_plus="
          [                                            \
            {   \"attribute\": \"\${node.datacenter}\" \
              , \"value\":     \"${datacenter}\"       \
            }                                          \
          ]                                            \
        "
          jq \
            --argjson group_constraints_array_plus "${group_constraints_array_plus}" \
            "                                                                                   \
                .[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"constraint\"] \
              |=                                                                                \
                ( . + \$group_constraints_array_plus)                                           \
            " \
            "${dir}"/nomad/nomad-job.json \
        | \
          sponge "${dir}"/nomad/nomad-job.json
      done
    fi
    ############################################################################
  fi

  # Store a summary of the job.
  jq \
    '{
        "namespace":   ( .job["workbench-cluster-job"].namespace   )
      , "datacenters": ( .job["workbench-cluster-job"].datacenters )
      , "constraint":  ( .job["workbench-cluster-job"].constraint  )
      , "groups":      (
            .job["workbench-cluster-job"].group
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
    diff --side-by-side "${dir}"/node-specs.ante.json "${dir}"/node-specs.post.json
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
    diff --side-by-side "${dir}"/topology.ante.json "${dir}"/topology.post.json
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

fetch-logs-nomadcloud() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift

  msg "Fetch logs ..."

  msg "First start the sandboxed SSH servers ..."
  # Only if running on dedicated P&T Nomad cluster on AWS we use SSH, if not
  # `nomad exec`, because we need to have an exclusive port open for us.
  if echo "${WB_SHELL_PROFILE}" | grep --quiet "\-nomadperf"
  then
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
  fi

  fetch-logs-nomadcloud-retry "${dir}"

  msg "Sandboxed SSH servers will be kept running for debugging purposes"
}

fetch-logs-nomadcloud-retry() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift

  local jobs_array=()
  for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
  do
    if ! test -f "${dir}"/nomad/"${node}"/download_ok
    then
      fetch-logs-nomadcloud-node "${dir}" "${node}" &
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
      fetch-logs-nomadcloud-retry "${dir}"
    else
      msg "$(green "Finished fetching logs")"
    fi
  else
    msg "Nothing to do: check files in \"${dir}/nomad/NODE/download_ok\""
  fi
}

fetch-logs-nomadcloud-node() {
  local dir=${1}
  local node=${2}

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
  local node_ok="true"
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
