usage_nomadexec() {
  # Using a unique help message for all Nomad "sub-backends"
  usage_nomad
}

backend_nomadexec() {

  op=${1:?$(usage_nomadexec)}; shift

  case "${op}" in

    name )
      # Can be:
      # nomadpodman       (Using podman Task Driver in the cloud is not planned)
      # nomadexec  (Starts Nomad Agents supporting the "nix_installable" stanza)
      # nomadcloud  (IOG Nomad Agents and Amazon S3 with credentials from Vault)
      echo 'nomadexec'
    ;;

    # Generic backend sub-commands, shared code between Nomad sub-backends.

    describe-run )
      backend_nomad describe-run         "$@"
    ;;

    is-running )
      backend_nomad is-running           "$@"
    ;;

    start-cluster )
      backend_nomad start-cluster        "$@"
    ;;

    start-tracers )
      backend_nomad start-tracers        "$@"
    ;;

    start-nodes )
      backend_nomad start-nodes          "$@"
    ;;

    start-generator )
      backend_nomad start-generator      "$@"
    ;;

    start-healthchecks )
      backend_nomad start-healthchecks   "$@"
    ;;

    start-node )
      backend_nomad start-node           "$@"
    ;;

    stop-node )
      backend_nomad stop-node            "$@"
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

    stop-all )
      backend_nomad stop-all             "$@"
    ;;

    fetch-logs )
      backend_nomad fetch-logs           "$@"
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
      setenvjqstr 'nomad_environment'   "local"
      setenvjqstr 'one_tracer_per_node' "true"

      # Local runs always run the generator inside Nomad Task "node-0"
      setenvjqstr 'generator_task_name' "$(jq -r .nomadJob.generatorTaskName "${profile_container_specs_file}")"
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
      backend_nomad allocate-run-nomad-job-patch-namespace "${dir}" "default"
      backend_nomad allocate-run-nomad-job-patch-nix       "${dir}"

      backend_nomad allocate-run "${dir}"
    ;;

    # It "overrides" completely `backend_nomad`'s `deploy-genesis`.
    deploy-genesis )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
      local server_name=$(envjqr 'nomad_server_name')
      local client_name=$(envjqr 'nomad_client_name')

      # Add genesis to HTTP cache server
      local nomad_agents_were_already_running=$(envjqr 'nomad_agents_were_already_running')
      if ! wb_nomad webfs is-running
      then
        msg "$(blue Starting) a local $(yellow "HTTP server")"
        if ! wb_nomad webfs start
        then
          if test "${nomad_agents_were_already_running}" = "false"
          then
            msg "$(red "Startup of webfs failed, cleaning up ...")"
            wb_nomad agents stop "${server_name}" "${client_name}" "exec"
            backend_nomad stop-nomad-job "${dir}"
          fi
          fatal "Failed to start a local HTTP server"
        fi
      else
        msg "$(blue Reusing) already running local $(yellow "HTTP server")"
      fi
      msg "$(blue Creating) $(yellow "\"${nomad_job_name}.tar.zst\"") ..."
      if ! wb_nomad webfs add-genesis-dir "${dir}"/genesis "${nomad_job_name}"
      then
        if test "${nomad_agents_were_already_running}" = "false"
        then
          msg "$(red "Startup of webfs failed, cleaning up ...")"
          wb_nomad agents stop "${server_name}" "${client_name}" "exec"
          backend_nomad stop-nomad-job "${dir}"
        fi
        fatal "Failed to add genesis file to local HTTP server"
      else
        msg "$(green "File \"${nomad_job_name}.tar.zst\" created successfully and ready to deploy")"
      fi
      # Generic download from every node.
      local uri="http://127.0.0.1:12000/${nomad_job_name}.tar.zst"
      if ! backend_nomad deploy-genesis-wget "${dir}" "${uri}"
      then
        fatal "Deploy of genesis \"${uri}\" failed"
      else
        msg "$(green "Genesis \"${uri}\" deployed successfully")"
      fi
    ;;

    * )
      backend_nomad "${op}" "$@"
    ;;

  esac

}
