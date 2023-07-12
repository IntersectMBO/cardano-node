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
      # nomadcloud  (IOG Nomad Agents and Amazon S3 with credentials from Vault)
      echo 'nomadcloud'
    ;;

    # Generic backend sub-commands, shared code between Nomad sub-backends.

    describe-run )
      backend_nomad describe-run         "$@"
    ;;

    is-running )
      backend_nomad is-running           "$@"
    ;;

    start )
      backend_nomad start                "$@"
    ;;

    cleanup-cluster )
      backend_nomad cleanup-cluster      "$@"
    ;;

    start-nodes )
      backend_nomad start-nodes          "$@"
    ;;

    start-node )
      backend_nomad start-node           "$@"
    ;;

    stop-node )
      backend_nomad stop-node            "$@"
    ;;

    start-healthchecks )
      backend_nomad start-healthchecks   "$@"
    ;;

    start-generator )
      backend_nomad start-generator      "$@"
    ;;

    get-node-socket-path )
      backend_nomad get-node-socket-path "$@"
    ;;

    wait-node )
      backend_nomad wait-node            "$@"
    ;;

    wait-node-stopped )
      backend_nomad wait-node-stopped    "$@"
    ;;

    wait-pools-stopped )
      backend_nomad wait-pools-stopped   "$@"
    ;;

    stop-cluster )
      backend_nomad stop-cluster         "$@"
    ;;

    cleanup-cluster )
      backend_nomad cleanup-cluster      "$@"
    ;;

    # Sets jq envars "profile_container_specs_file" ,"nomad_environment",
    # "nomad_task_driver" and "one_tracer_per_node".
    # It "overrides" completely `backend_nomad`'s `setenv-defaults`.
    setenv-defaults )
      local usage="USAGE: wb backend $op BACKEND-DIR"
      local backend_dir=${1:?$usage}; shift

      setenvjqstr 'nomad_task_driver'   "exec"
      setenvjqstr 'nomad_server_name'   "srv1"
      # As one task driver runs as a normal user and the other as a root, use
      # different names to allow restarting/reusing without cleaup, this way
      # data folders already there can be accessed without "permission denied"
      # errors.
      setenvjqstr 'nomad_client_name'   "cli1-exe"

      # Store the location of the Nix-built "container-specs" file.
      # TODO/FIXME: This is the only way to be able to later copy it to "$dir" ?
      local profile_container_specs_file
      profile_container_specs_file="${backend_dir}"/container-specs.json
      setenvjqstr 'profile_container_specs_file' "${profile_container_specs_file}"
      setenvjqstr 'nomad_environment'   "cloud"
      setenvjqstr 'one_tracer_per_node' "true" # TODO: Not implemented yet!

      # Cloud runs always run the generator inside Nomad Task "explorer"
      setenvjqstr 'generator_task_name' "$(jq -r .nomadJob.generatorTaskName "${profile_container_specs_file}")"

      backend_nomadcloud setenv-nomadcloud "${profile_container_specs_file}"
    ;;

    # Checks for set the values for "NOMA..." and
    setenv-nomadcloud )
      local profile_container_specs_file=${1:?$usage}; shift
      local nomad_environment
      # If the most important `nomad` cli envars is present this is not a local
      # test, I repeat, this is not a drill =)
      if test -z "${NOMAD_ADDR:-}"
      then
        msg $(yellow "WARNING: Nomad address \"NOMAD_ADDR\" envar is not set")
        export NOMAD_ADDR="https://nomad.world.dev.cardano.org"
        msg $(blue "INFO: Setting \"NOMAD_ADDR\" to the SRE provided address for \"Performance and Tracing\" (\"${NOMAD_ADDR}\")")
      else
        if test "${NOMAD_ADDR}" != "https://nomad.world.dev.cardano.org"
        then
          msg $(yellow "WARNING: Nomad address \"NOMAD_ADDR\" envar is not \"https://nomad.world.dev.cardano.org\"")
        fi
      fi
      # The abscence of `NOMAD_NAMESPACE` or `NOMAD_TOKEN` needs confirmation
      if test -z "${NOMAD_NAMESPACE:-}"
      then
        msg $(yellow "WARNING: Nomad namespace \"NOMAD_NAMESPACE\" envar is not set")
        export NOMAD_NAMESPACE="perf"
        msg $(blue "INFO: Setting \"NOMAD_NAMESPACE\" to the SRE provided namespace for \"Performance and Tracing\" (\"${NOMAD_NAMESPACE}\")")
      else
        if test "${NOMAD_NAMESPACE}" != "perf"
        then
          msg $(yellow "WARNING: Nomad namespace \"NOMAD_NAMESPACE\" envar is not \"perf\"")
        fi
      fi
      if test -z "${NOMAD_TOKEN:-}"
      then
        msg $(yellow "WARNING: Nomad token \"NOMAD_TOKEN\" envar is not set")
        msg $(blue "INFO: Fetching a \"NOMAD_TOKEN\" from SRE provided Vault for \"Performance and Tracing\"")
        export NOMAD_TOKEN="$(wb_nomad vault world nomad-token)"
      fi
      # Check all the AWS S3 envars needed for the HTTP PUT request
      # Using same names as the AWS CLI
      # https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-envvars.html
      if test -z "${AWS_ACCESS_KEY_ID:-}" || test -z "${AWS_SECRET_ACCESS_KEY:-}"
      then
        msg $(yellow "WARNING: Amazon S3 \"AWS_ACCESS_KEY_ID\" or \"AWS_SECRET_ACCESS_KEY\" envar is not set")
        msg $(blue "INFO: Fetching \"AWS_ACCESS_KEY_ID\" and \"AWS_SECRET_ACCESS_KEY\" from SRE provided Vault for \"Performance and Tracing\"")
        local aws_credentials
        aws_credentials="$(wb_nomad vault world aws-s3-credentials)"
        export AWS_ACCESS_KEY_ID=$(echo "${aws_credentials}" | jq -r .data.access_key)
        export AWS_SECRET_ACCESS_KEY=$(echo "${aws_credentials}" | jq -r .data.secret_key)
      fi
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
        if curl_response=$(curl --silent --show-error --write-out '%{json}' https://api.github.com/repos/input-output-hk/cardano-node/commits/"${gitrev}")
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
      # There are so many assumptions that I like having the user confirm them!
      read -p "Hit enter to continue ..."
    ;;

    # Sub-backend specific allocs and calls `backend_nomad`'s `allocate-run`.
    allocate-run )
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
      # create a nicely sorted and indented copy it "nomad/nomad-job.json".
      jq -r ".nomadJob.exec.oneTracerPerNode"      \
        "${dir}"/container-specs.json              \
      > "${dir}"/nomad/nomad-job.json
      # The job file is "slightly" modified (jq) to suit the running environment.
      backend_nomad allocate-run-nomad-job-patch-namespace         "${dir}" "${NOMAD_NAMESPACE}"
      backend_nomad allocate-run-nomad-job-patch-nix               "${dir}"

      # Set the placement info and resources accordingly
      local nomad_job_name
      nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
      ##########################################################################
      # Profile name dependent changes #########################################
      ##########################################################################
      # "cw-perf-*" profiles are profiles that only run on Cardano World's Nomad
      # Nodes of class "perf".
      # Other cloud profiles are for example "ci-test-cw-qa", "ci-test-cw-perf",
      # "ci-test-cw-qa", "ci-test-cw-perf". "qa" means that they run on Nomad
      # nodes that belong to the "qa" class, runs on these should be limited to
      # short tests and must never use the "infra" class where HA jobs runs.
      if test -z "${WB_SHELL_PROFILE:-}"
      then
        fatal "Envar \"WB_SHELL_PROFILE\" is empty!"
      else
        ########################################################################
        # Fix for region mismatches ############################################
        ########################################################################
        # We use "us-east-2" and they use "us-east-1"
          jq \
            ".[\"job\"][\"${nomad_job_name}\"][\"datacenters\"] |= [\"eu-central-1\", \"us-east-1\", \"ap-southeast-2\"]" \
            "${dir}"/nomad/nomad-job.json \
        | \
            sponge "${dir}"/nomad/nomad-job.json
          jq \
            ".[\"job\"][\"${nomad_job_name}\"][\"group\"] |= with_entries( if (.value.affinity.value == \"us-east-2\") then (.value.affinity.value |= \"us-east-1\") else (.) end )" \
            "${dir}"/nomad/nomad-job.json \
        | \
            sponge "${dir}"/nomad/nomad-job.json
        ########################################################################
        # Unique placement: ####################################################
        ########################################################################
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
        # Adds it as a job level contraint.
          jq \
            --argjson job_constraints_array "${job_constraints_array}" \
            ".[\"job\"][\"${nomad_job_name}\"].constraint |= \$job_constraints_array" \
            "${dir}"/nomad/nomad-job.json \
        | \
          sponge "${dir}"/nomad/nomad-job.json
        ########################################################################
        # Node class: ##########################################################
        ########################################################################
        local group_constraints_array
        # "perf" class nodes are the default unless the profile name contains
        # "cw-qa", we try to limit the usage of Nomad nodes that are not
        # dedicated Perf team nodes.
        # But also, we have to be careful that "perf" runs do not overlap. We
        # are making "perf" class nodes runs can't clash because service names
        # and resources definitions currently won't allow that to happen but
        # still a new "perf" run may mess up a previously running cluster.
        if echo "${WB_SHELL_PROFILE}" | grep --quiet "cw-qa"
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
        else
          # Using Performance & Tracing exclusive "perf" class distinct nodes!
          group_constraints_array='
            [
              {
                "operator":  "="
              , "attribute": "${node.class}"
              , "value":     "perf"
              }
            ]
          '
        fi
        # Adds it as a group level contraint.
          jq \
            --argjson group_constraints_array "${group_constraints_array}" \
            ".[\"job\"][\"${nomad_job_name}\"][\"group\"] |= with_entries(.value.constraint = \$group_constraints_array)" \
            "${dir}"/nomad/nomad-job.json \
        | \
          sponge "${dir}"/nomad/nomad-job.json
        ########################################################################
        # Memory/resources: ####################################################
        ########################################################################
        # Set the resources, only for perf!
        # When not "perf", when "cw-qa", only "short" tests are allowed on
        # whatever resources we are given.
        if echo "${WB_SHELL_PROFILE}" | grep --quiet "cw-perf"
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
          local producer_resources='{
              "cores":      8
            , "memory":     13000
            , "memory_max": 15000
          }'
          # Set this for every non-explorer node
            jq \
              --argjson producer_resources "${producer_resources}" \
              " \
                  .[\"job\"][\"${nomad_job_name}\"][\"group\"] \
                |= \
                  with_entries( \
                    if ( .key != \"explorer\" ) \
                    then ( \
                        .value.task \
                      |= \
                        with_entries( .value.resources = \$producer_resources ) \
                    ) else ( \
                      . \
                    ) end \
                  ) \
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
            , "memory":     29000
            , "memory_max": 31000
          }'
            jq \
              --argjson resources "${explorer_resources}" \
              ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"explorer\"][\"task\"] |= with_entries( .value.resources = \$resources )" \
              "${dir}"/nomad/nomad-job.json \
          | \
            sponge "${dir}"/nomad/nomad-job.json
        fi
        ########################################################################
        # Reproducibility: #####################################################
        ########################################################################
        # If value profile on "perf", using always the same placement!
        # This means node-N always runs on the same Nomad Client/AWS EC2 machine
        if test "${WB_SHELL_PROFILE:0:13}" = 'cw-perf-value'
        then
          # A file with all the available Nomad Clients is needed!
          # This files is a list of Nomad Clients with a minimun of ".id",
          # ".datacenter", ".attributes.platform.aws["instance-type"]",
          # ".attributes.platform.aws.placement["availability-zone"]",
          # ".attributes.unique.platform.aws["instance-id"]",
          # ".attributes.unique.platform.aws.["public-ipv4"]" and
          # ".attributes.unique.platform.aws.mac".
          if test -z "${NOMAD_CLIENTS_FILE:-}" || ! test -f "${NOMAD_CLIENTS_FILE}"
          then
            fatal "No \"\$NOMAD_CLIENTS_FILE\". For reproducible builds provide this file that ensures cluster nodes are always placed on the same machines, or create a new one with 'wb nomad nodes' if Nomad Clients have suffered changes and runs fail with \"placement errors\""
          fi
          # For each (instance-type, datacener/region) we look incrementally for
          # the unique AWS EC2 "instance-id" only after ordering the Nomad
          # Clients by its unique Nomad provided "id".
          local count_ap=0 count_eu=0 count_us=0
          # For each Nomad Job Group
          local groups_array
          # Keys MUST be sorted to always get the same order for the same profile!
          groups_array=$(jq -S -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"] | keys | sort | join (\" \")" "${dir}"/nomad/nomad-job.json)
          for group_name in ${groups_array[*]}
          do
            # Obtain the datacenter as Nomad sees it, not as an AWS attributes.
            # For example "eu-central-1" instead of "eu-central-1a".
            local datacenter
            datacenter=$(jq \
              -r \
              ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"].affinity.value" \
              "${dir}"/nomad/nomad-job.json \
            )
            # For each Nomad Job Group Task
            local tasks_array
            # Keys MUST be sorted to always get the same order for the same profile!
            tasks_array=$(jq -S -r ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"task\"] | keys | sort | join (\" \")" "${dir}"/nomad/nomad-job.json)
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
              actual_client=$(jq \
                "   . \
                  | \
                    sort_by(.id) \
                  | \
                    map(select(.datacenter == \"${datacenter}\")) \
                  | \
                    map(select(.attributes.platform.aws[\"instance-type\"] == \"${instance_type}\")) \
                  | \
                    .[${count}] \
                " \
                "${NOMAD_CLIENTS_FILE}" \
              )
              local instance_id availability_zone public_ipv4 mac_address
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
                ] \
              "
                jq \
                  --argjson group_constraints_array_plus "${group_constraints_array_plus}" \
                  ".[\"job\"][\"${nomad_job_name}\"][\"group\"][\"${group_name}\"][\"constraint\"] |= ( . + \$group_constraints_array_plus)" \
                  "${dir}"/nomad/nomad-job.json \
              | \
                sponge "${dir}"/nomad/nomad-job.json
            done
          done
        fi
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

      backend_nomad allocate-run "${dir}"
    ;;

    # It "overrides" completely `backend_nomad`'s `deploy-genesis`.
    deploy-genesis )
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
        local nomad_agents_were_already_running=$(envjqr 'nomad_agents_were_already_running')
        if test "${nomad_agents_were_already_running}" = "false"
        then
          wb_nomad agents stop "${server_name}" "${client_name}" "exec"
        fi
        backend_nomad stop-nomad-job "${dir}"
        fatal "Failed to upload genesis"
      fi

      # Generic download from every node.
      local uri="https://${s3_bucket_name}.${s3_host}/${genesis_file_name}"
      if ! backend_nomad deploy-genesis-wget "${dir}" "${uri}"
      then
        # File kept for debugging!
        fatal "Deploy of genesis \"${uri}\" failed"
      else
        msg "$(green "Genesis \"${uri}\" deployed successfully")"
        # Don't keep the file once ready, it's not free!
        msg "$(blue Removing) genesis file from $(yellow "\"s3://${s3_bucket_name}\"") ..."
        aws s3 rm                                         \
          s3://"${s3_bucket_name}"/"${genesis_file_name}" \
          --region "${s3_region}"                         \
        || true
        # Reminder to remove old files.
        msg "Still available files at $(yellow "\"s3://${s3_bucket_name}\""):"
        aws s3 ls                   \
          s3://"${s3_bucket_name}"/ \
          --region "${s3_region}"
      fi
    ;;

    * )
      backend_nomad "${op}" "$@"
    ;;

  esac

}
