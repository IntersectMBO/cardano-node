usage_nomadcloud() {
  # Using a unique help message for all Nomad "sub-backends"
  usage_nomad
}

backend_nomadcloud() {

  op=${1:?$(usage_nomadcloud)}; shift

  case "${op}" in

    name )
      # Can be:
      # nomadpodman       (Using podman task driver in the cloud is not planned)
      # nomadexec    (Starts Nomad agents supporting the nix_installable stanza)
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
    # "nomad_task_driver" and "one_tracer_per_node"
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
        read -p "Hit enter to continue ..."
      else
        if test "${NOMAD_ADDR}" != "https://nomad.world.dev.cardano.org"
        then
          msg $(yellow "WARNING: Nomad address \"NOMAD_ADDR\" envar is not \"https://nomad.world.dev.cardano.org\"")
          read -p "Hit enter to continue ..."
        fi
      fi
      # The abscence of `NOMAD_NAMESPACE` or `NOMAD_TOKEN` needs confirmation
      if test -z "${NOMAD_NAMESPACE:-}"
      then
        msg $(yellow "WARNING: Nomad namespace \"NOMAD_NAMESPACE\" envar is not set")
        export NOMAD_NAMESPACE="perf"
        msg $(blue "INFO: Setting \"NOMAD_NAMESPACE\" to the SRE provided namespace for \"Performance and Tracing\" (\"${NOMAD_NAMESPACE}\")")
        read -p "Hit enter to continue ..."
      else
        if test "${NOMAD_NAMESPACE}" != "perf"
        then
          msg $(yellow "WARNING: Nomad namespace \"NOMAD_NAMESPACE\" envar is not \"perf\"")
          read -p "Hit enter to continue ..."
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
        local aws_credentials="$(wb_nomad vault world aws-s3-credentials)"
        export AWS_ACCESS_KEY_ID=$(echo "${aws_credentials}" | jq -r .data.access_key)
        export AWS_SECRET_ACCESS_KEY=$(echo "${aws_credentials}" | jq -r .data.secret_key)
      fi
      # The Nomad job spec will contain links ("nix_installables" stanza) to
      # the Nix Flake outputs it needs inside the container, these are
      # refereced with a GitHub commit ID inside the "container-specs" file.
      local gitrev=$(jq -r .gitrev "${profile_container_specs_file}")
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
          local headers=$(echo "${curl_response}" | jq -s .[1])
          if test "$(echo "${headers}" | jq .http_code)" != 200
          then
            fatal "GitHub commit \"$gitrev\" is not available online!"
          fi
          # Show returned commit info in `git log` fashion
          local body=$(echo "${curl_response}" | jq -s .[0])
          msg $(green "commit ${gitrev}")
          local author_name=$(echo $body | jq -r .commit.author.name)
          local author_email=$(echo $body | jq -r .commit.author.email)
          msg $(green "Author: ${author_name} <${author_email}>")
          local author_date=$(echo $body | jq -r .commit.author.date)
          msg $(green "Date: ${author_date}")
          msg $(green "\n")
          local message=$(echo $body | jq -r .commit.message)
          msg $(green "\t${message}\n")
          msg $(green "\n")
          read -p "Hit enter to continue ..."
        else
          fatal "Could not fetch commit info from GitHub (\`curl\` error)"
        fi
      fi
    ;;

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
      backend_nomad allocate-run-nomad-job-patch-namespace "${dir}" "${NOMAD_NAMESPACE}"
      backend_nomad allocate-run-nomad-job-patch-nix       "${dir}"

      backend_nomad allocate-run "${dir}"
    ;;

    deploy-genesis )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)

      local genesis_file_name="${nomad_job_name}.tar.zst"
      find "${dir}"/genesis -type f -printf "%P\n"    \
        | tar --create --zstd                         \
          --file="${dir}"/"${genesis_file_name}"      \
          --owner=65534 --group=65534 --mode="u=rwx"  \
          --directory="${dir}"/genesis --files-from=-

      local s3_region="eu-central-1"
      local s3_host="s3.${s3_region}.amazonaws.com";
      local s3_bucket_name="iog-cardano-perf";
      local s3_access_key="${AWS_ACCESS_KEY_ID}";
      local s3_access_key_secret="${AWS_SECRET_ACCESS_KEY}"
      local s3_storage_class="STANDARD"
      local return_code=0
      aws s3 cp                                                               \
        "${dir}"/"${genesis_file_name}"                                       \
        s3://"${s3_bucket_name}"                                              \
        --content-type "application/zstd"                                     \
        --region "${s3_region}"                                               \
        --expected-size "$(stat --printf=%s "${dir}"/"${genesis_file_name}")" \
      >/dev/null                                                              \
      || return_code="$?"
      # https://docs.aws.amazon.com/cli/latest/userguide/cli-services-s3-commands.html#using-s3-commands-managing-objects-copy
      # https://awscli.amazonaws.com/v2/documentation/api/latest/reference/s3/cp.html
      if test "${return_code}" = "0"
      then
        # A server response was obtained.
        msg "File \"${genesis_file_name}\" uploaded"
      else
        msg "Upload to Amazon S3 failed, cleaning up ..."
        local nomad_agents_were_already_running=$(envjqr 'nomad_agents_were_already_running')
        if test "${nomad_agents_were_already_running}" = "false"
        then
          backend_nomad nomad agents stop \
            "${server_name}" "${client_name}" "exec"
        fi
        backend_nomad stop-nomad-job "${dir}"
        fatal "Failed to upload ${genesis_file_name}"
      fi
      backend_nomad deploy-genesis-wget "${dir}" \
        "https://${s3_bucket_name}.${s3_host}/${genesis_file_name}"
    ;;

    * )
      # TODO: Replace with `usage_nomadcloud` and make the nomad helper commands
      # use a new top level sub-command `wb nomad`
      backend_nomad "${op}" "$@"
    ;;

  esac

}
