usage_nomadexec() {
  # Using a unique help message for all Nomad "sub-backends"
  usage_nomad
}

backend_nomadexec() {

  op=${1:?$(usage_nomadexec)}; shift

  case "${op}" in

    name )
      # Can be:
      # nomadexec  (Starts Nomad Agents supporting the "nix_installable" stanza)
      # nomadcloud    (SRE managed Nomad Agents on Amazon S3 (dedicated or not))
      echo 'nomadexec'
    ;;

    # Overrided backend "methods"

    # All sub-backends set these same jq envars.
    setenv-defaults )
      local usage="USAGE: wb backend $op BACKEND-DIR"
      local backend_dir=${1:?$usage}; shift
      # Repeated code / envars set by all sub-backends
      setenvjqstr 'nomad_environment'    "local"
      setenvjqstr 'one_tracer_per_node'  "true"
      # Local runs always run the generator inside Nomad Task "node-0"
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
      setenv-defaults-nomadexec          "${backend_dir}"
    ;;

    allocate-run )
      # Default allocation before calling the backend specific allocation.
      backend_nomad allocate-run            "$@"
      allocate-run-nomadexec                "$@"
    ;;

    # Called by `run.sh` without exit trap (unlike `scenario_setup_exit_trap`)!
    deploy-genesis )
      # It "overrides" completely `backend_nomad`'s `deploy-genesis`.
      deploy-genesis-nomadexec              "$@"
    ;;

    wait-pools-stopped )
      # It passes the sleep time (in seconds) required argument.
      # This time is different between local and cloud backends to avoid
      # unnecessary Nomad specific traffic (~99% happens waiting for node-0, the
      # first one it waits to stop inside a loop) and at the same time be less
      # sensitive to network failures.
      backend_nomad wait-pools-stopped      1 "$@"
    ;;

    wait-workloads-stopped )
      # It passes the sleep time (in seconds) required argument.
      # This time is different between local and cloud backends to avoid
      # unnecessary Nomad specific traffic (~99% happens waiting for node-0, the
      # first one it waits to stop inside a loop) and at the same time be less
      # sensitive to network failures.
      backend_nomad wait-workloads-stopped  1 "$@"
    ;;

    # All or clean up everything!
    # Called after `scenario.sh` without an exit trap!
    stop-cluster )
      # Shared code between Nomad sub-backends that internally only takes care
      # of the Nomad job.
      backend_nomad stop-cluster-internal   "$@"
      # Takes care of any Nomad agents (server and client(s)) that were setup
      # locally for only this run.
      backend_nomad stop-cluster-local      "$@"
    ;;

    # Generic backend sub-commands, shared code between Nomad sub-backends.

    describe-run )
      backend_nomad describe-run            "$@"
    ;;

    is-running )
      backend_nomad is-running              "$@"
    ;;

    start-cluster )
      backend_nomad start-cluster           "$@"
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

    start-workloads )
      backend_nomad start-workloads         "$@"
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

    fetch-logs )
      backend_nomad fetch-logs              "$@"
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
setenv-defaults-nomadexec() {
  local backend_dir="${1}"

  setenvjqstr 'nomad_server_name'   "srv1"
  # As other task driver can run as a normal user and exec needs root, use
  # different names to allow restarting/reusing without cleanup, this way
  # data folders already there can be accessed without "permission denied"
  # errors.
  setenvjqstr 'nomad_client_name'   "cli1-exe"
}

# Sub-backend specific allocs and calls `backend_nomad`'s `allocate-run`.
allocate-run-nomadexec() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift

  # Copy the container specs file (container-specs.json)
  # This is the output file of the Nix derivation
  local profile_container_specs_file=$(envjqr 'profile_container_specs_file')
  # Create a nicely sorted and indented copy
  jq . "${profile_container_specs_file}" > "${dir}"/container-specs.json

  # Select which version of the Nomad job spec file we are running and
  # create a nicely sorted and indented copy it "nomad/nomad-job.json".
  jq -r ".nomadJob.exec.oneTracerPerNode"      \
    "${dir}"/container-specs.json              \
  > "${dir}"/nomad/nomad-job.json
  # Update the Nomad Job specs file accordingly
  ## - Job Name
  ### Must match `^[a-zA-Z0-9-]{1,128}$)` or it won't be possible to use it
  ### as namespace.: "invalid name "2023-02-10-06.34.f178b.ci-test-bage.nom"".
  local nomad_job_name=$(basename "${dir}")
  backend_nomad allocate-run-nomad-job-patch-name "${dir}" "${nomad_job_name}"
  # The job file is "slightly" modified (jq) to suit the running environment.
  ## Empty the global namespace. Local runs ignore "${NOMAD_NAMESPACE:-}"
  backend_nomad allocate-run-nomad-job-patch-namespace "${dir}"
  # Will set the /nix/store paths from ".nix-store-path" in container-specs.json
  backend_nomad allocate-run-nomad-job-patch-nix "${dir}"
}

# Called by `run.sh` without exit trap (unlike `scenario_setup_exit_trap`)!
deploy-genesis-nomadexec() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift
  local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)

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
        # `stop-nomad-job` takes care of stopping the Nomad agents.
        backend_nomad stop-nomad-job "${dir}" || msg "$(red "Failed to stop Nomad Job")"
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
      # `stop-nomad-job` takes care of stopping the Nomad agents.
      backend_nomad stop-nomad-job "${dir}" || msg "$(red "Failed to stop Nomad Job")"
    fi
    fatal "Failed to add genesis file to local HTTP server"
  else
    msg "$(green "File \"${nomad_job_name}.tar.zst\" created successfully and ready to deploy")"
  fi
  # Generic download from every node.
  local uri="http://127.0.0.1:12000/${nomad_job_name}.tar.zst"
  if ! backend_nomad deploy-genesis-wget "${dir}" "${uri}"
  then
    msg "$(red "Deploy of genesis failed, cleaning up ...")"
    # `stop-nomad-job` takes care of stopping the Nomad agents.
    backend_nomad stop-nomad-job "${dir}" || msg "$(red "Failed to stop Nomad Job")"
    fatal "Deploy of genesis \"${uri}\" failed"
  else
    msg "$(green "Genesis \"${uri}\" deployed successfully")"
  fi
}
