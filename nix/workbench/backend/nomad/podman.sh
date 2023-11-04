usage_nomadpodman() {
  # Using a unique help message for all Nomad "sub-backends"
  usage_nomad
}

backend_nomadpodman() {

  op=${1:?$(usage_nomadpodman)}; shift

  case "${op}" in

    name )
      # Can be:
      # nomadpodman       (Using podman Task Driver in the cloud is not planned)
      # nomadexec  (Starts Nomad Agents supporting the "nix_installable" stanza)
      # nomadcloud    (SRE managed Nomad Agents on Amazon S3 (dedicated or not))
      echo 'nomadpodman'
    ;;

    # Overrided backend "methods"

    setenv-defaults )
      local usage="USAGE: wb backend $op BACKEND-DIR"
      local backend_dir=${1:?$usage}; shift
      # Repeated code / envars set by all sub-backends
      setenvjqstr 'nomad_task_driver'    "podman"
      setenvjqstr 'nomad_environment'    "local"
      setenvjqstr 'one_tracer_per_node'  "false"
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
      setenv-defaults-nomadpodman        "${backend_dir}"
    ;;

    allocate-run )
      allocate-run-nomadpodman              "$@"
      # Does a pre allocation before calling the default/common allocation.
      backend_nomad allocate-run            "$@"
    ;;

    # Called by `run.sh` without exit trap (unlike `scenario_setup_exit_trap`)!
    deploy-genesis )
      # It "overrides" completely `backend_nomad`'s `deploy-genesis`.
      deploy-genesis-nomadpodman            "$@"
    ;;

    wait-pools-stopped )
      # It passes the sleep time (in seconds) required argument.
      # This time is different between local and cloud backends to avoid
      # unnecesary Nomad specific traffic (~99% happens waiting for node-0, the
      # first one it waits to stop inside a loop) and at the same time be less
      # sensitive to network failures.
      backend_nomad wait-pools-stopped    1 "$@"
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

# Sets jq envars "profile_container_specs_file" ,"nomad_environment",
# "nomad_task_driver" and "one_tracer_per_node".
# It "overrides" completely `backend_nomad`'s `setenv-defaults`.
setenv-defaults-nomadpodman() {
  local backend_dir="${1}"

  setenvjqstr 'nomad_server_name'   "srv1"
  # As one task driver runs as a normal user and the other as a root, use
  # different names to allow restarting/reusing without cleaup, this way
  # data folders already there can be accessed without "permission denied"
  # errors.
  setenvjqstr 'nomad_client_name'   "cli1-pod"
}

# Sub-backend specific allocs and calls `backend_nomad`'s `allocate-run`.
allocate-run-nomadpodman() {
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
  jq -r ".nomadJob.podman.oneTracerPerCluster" \
    "${dir}"/container-specs.json              \
  > "${dir}"/nomad/nomad-job.json
  # The job file is "slightly" modified (jq) to suit the running environment.
  ## Empty the global namespace. Local runs ignore "${NOMAD_NAMESPACE:-}"
  backend_nomad allocate-run-nomad-job-patch-namespace "${dir}"
  podman_create_image "${dir}"
  # Make sure the "genesis-volume" dir is present when the Nomad job is
  # started because with the podman task driver (always local, not used for
  # cloud) these directories are going to be mounted by adding them to the
  # "volume" stanza.
  mkdir "${dir}"/genesis-volume
  # It needs to mount the tracer directory if "one_tracer_per_node" is
  # false and mount the genesis and CARDANO_MAINNET_MIRROR (if needed).
  nomad_job_file_create_mounts "${dir}"
}

# Called by `run.sh` without exit trap (unlike `scenario_setup_exit_trap`)!
deploy-genesis-nomadpodman() {
  local usage="USAGE: wb backend $op RUN-DIR"
  local dir=${1:?$usage}; shift
  # The "$dir/genesis-volume" folder is mounted to "run/local/genesis"
  # inside the container that for security reasons does not follow symlinks,
  # so everything is copied from the generated/patched genesis in
  # "$dir/genesis" to "$dir/genesis-volume"
  mkdir "${dir}"/genesis-volume/byron
  mkdir "${dir}"/genesis-volume/utxo-keys
  mkdir "${dir}"/genesis-volume/node-keys
  cp -a "${dir}"/genesis/genesis.alonzo.json        \
        "${dir}"/genesis-volume/genesis.alonzo.json
  cp -a "${dir}"/genesis/genesis.conway.json        \
        "${dir}"/genesis-volume/genesis.conway.json
  cp -a "${dir}"/genesis/genesis-shelley.json       \
        "${dir}"/genesis-volume/genesis-shelley.json
  cp -a "${dir}"/genesis/byron/genesis.json         \
        "${dir}"/genesis-volume/byron/genesis.json
  cp -a                                             \
        "${dir}"/genesis/utxo-keys/*.skey           \
        "${dir}"/genesis-volume/utxo-keys/
  cp -a                                             \
        "${dir}"/genesis/utxo-keys/*.vkey           \
        "${dir}"/genesis-volume/utxo-keys/
  cp -a                                             \
        "${dir}"/genesis/node-keys/*.skey           \
        "${dir}"/genesis-volume/node-keys/
  cp -a                                             \
        "${dir}"/genesis/node-keys/*.vkey           \
        "${dir}"/genesis-volume/node-keys/
  cp -a                                             \
        "${dir}"/genesis/node-keys/*.opcert         \
        "${dir}"/genesis-volume/node-keys/
}

podman_create_image() {
  local dir=${1:?$usage}; shift
  # Look up the OCI image's name and tag (Nix profile).
  local oci_image_name=$(jq -r .ociImage.imageName "${dir}"/container-specs.json)
  local oci_image_tag=$( jq -r .ociImage.imageTag  "${dir}"/container-specs.json)
  if podman image exists "${oci_image_name}:${oci_image_tag}"
  then
    setenvjqstr 'oci_image_was_already_available' "true"
    msg "OCI image ${oci_image_name}:${oci_image_tag} is already available"
  else
    setenvjqstr 'oci_image_was_already_available' "false"
    msg "Creating OCI image ..."
    # Script that creates the OCI image from nix2container layered output.
    local oci_image_skopeo_script=$(jq -r .ociImage.copyToPodman "${dir}"/container-specs.json)
    # TODO: for further research.
    # STORAGE_DRIVER=overlay "$oci_image_skopeo_script"
    # If podman 4.2.1 and nomad v1.3.5 this fix is not needed anymore
    # Forced the `overlay` storage driver or podman won't see the image.
    # https://docs.podman.io/en/latest/markdown/podman.1.html#note-unsupported-file-  systems-in-rootless-mode
    # Error was: workbench:  FATAL: OCI image registry.workbench.iog.io/  cluster:2l7wi7sh1zyp2mnl24m13ibnh2wsjvwg cannot be found by podman
    if ! "${oci_image_skopeo_script}"
    then
      fatal "Creation of OCI image ${oci_image_name}:${oci_image_tag} failed"
    else
      # Now check that `podman` can see the "cluster" OCI image.
      if ! podman image exists "${oci_image_name}:${oci_image_tag}"
      then
        fatal "OCI image ${oci_image_name}:${oci_image_tag} was created but cannot be found by podman"
      else
        msg "OCI image named \"${oci_image_name}:${oci_image_tag}\" created"
      fi
    fi
  fi
}

nomad_job_file_create_mounts() {
  local dir=$1
  local absolute_dir=$(realpath "${dir}")
  local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
  local one_tracer_per_node=$(envjqr 'one_tracer_per_node')
  # If CARDANO_MAINNET_MIRROR is present generate a list of needed volumes.
  if test -n "${CARDANO_MAINNET_MIRROR}"
  then
    # The nix-store path contains 3 levels of symlinks. This is a hack to
    # avoid creating a container image with all these files.
    local immutable_store=$(readlink -f "${CARDANO_MAINNET_MIRROR}"/immutable)
    local mainnet_mirror_volumes="[
        \"${CARDANO_MAINNET_MIRROR}:${CARDANO_MAINNET_MIRROR}:ro\"
      , \"${immutable_store}:${immutable_store}:ro\"
      $(find -L "${immutable_store}" -type f -exec realpath {} \; | xargs dirname | sort | uniq | xargs -I "{}" echo ", \"{}:{}:ro\"")
    ]"
  else
    local mainnet_mirror_volumes="[]"
  fi
  # Hint:
  # - Working dir is: /tmp/cluster/
  # - Mount point is: /tmp/cluster/run/current
  ## The workbench is expecting an specific hierarchy of folders and files.
  local container_mountpoint=$(jq -r ". [\"job\"][\"${nomad_job_name}\"][\"meta\"][\"TASK_STATEDIR\"]" "${dir}"/nomad/nomad-job.json)
  # Nodes
  for node in $(jq_tolist 'keys' "${dir}"/node-specs.json)
  do
    local task_stanza_name="${node}"
    # Every node needs access to "./genesis/" and tracer when only 1 is used.
    local jq_filter="
      [
        \"${absolute_dir}/genesis-volume:${container_mountpoint}/genesis:ro\"
      ]
      +
      (
        if \$one_tracer_per_node == true
        then
          [ ]
        else
          [ \"${absolute_dir}/tracer:${container_mountpoint}/tracer:rw\" ]
        end
      )
      +
      \$mainnet_mirror_volumes
    "
    local podman_volumes=$(jq "${jq_filter}" --argjson one_tracer_per_node "${one_tracer_per_node}" --argjson mainnet_mirror_volumes "${mainnet_mirror_volumes}" "${dir}"/profile/node-specs.json)
    jq ".job[\"${nomad_job_name}\"][\"group\"][\"${node}\"][\"task\"][\"${node}\"][\"config\"][\"volumes\"] = \$podman_volumes" --argjson podman_volumes "${podman_volumes}" "${dir}"/nomad/nomad-job.json | sponge "${dir}"/nomad/nomad-job.json
  done
  # Tracer
  if jqtest ".node.tracer" "${dir}"/profile.json && ! test "${one_tracer_per_node}" = "true"
  then
    local task_stanza_name_t="tracer"
    # Tracer only needs access to itself (its shared folder).
    local jq_filter_t="
      [
        \"${absolute_dir}/tracer:${container_mountpoint}/tracer:rw\"
      ]
    "
    local podman_volumes_t=$(jq "${jq_filter_t}" "${dir}"/profile/node-specs.json)
    jq ".job[\"${nomad_job_name}\"][\"group\"][\"tracer\"][\"task\"][\"tracer\"][\"config\"][\"volumes\"] = \$podman_volumes_t" --argjson podman_volumes_t "${podman_volumes_t}" "${dir}"/nomad/nomad-job.json | sponge "${dir}"/nomad/nomad-job.json
  fi
}
