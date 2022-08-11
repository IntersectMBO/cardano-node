usage_app() {
     usage "app" "Multi-container application" <<EOF
    compose               Multi-container description file

EOF
}

app() {
  local op=${1:-show}; test $# -gt 0 && shift

  case "$op" in

    # wb app compose $WORKBENCH_SHELL_PROFILE_DIR/{profile,node-specs}.json name tag
    compose )
      # jq 'keys|.[]' --raw-output $WORKBENCH_SHELL_PROFILE_DIR/node-specs.json
      local usage="USAGE: wb app $op PROFILE-NAME/JSON NODE-SPECS/JSON NODE_IMAGE_NAME NODE_IMAGE_TAG TRACER_IMAGE_NAME TRACER_IMAGE_TAG"
      local profile=${1:?$usage}
      local nodespecs=${2:?$usage}
      local nodeImageName=${3:?$usage}
      local nodeImageTag=${4:?$usage}
      local tracerImageName=${5:?$usage}
      local tracerImageTag=${6:?$usage}

      # Hack
      global_rundir_def=$PWD/run
      local profile_scenario=$(jq -r '.scenario' "$profile")
      yq --argjson profileScenario "\"$profile_scenario\"" --argjson nodeImageName "\"$nodeImageName\"" --argjson nodeImageTag "\"$nodeImageTag\"" --argjson tracerImageName "\"$tracerImageName\"" --argjson tracerImageTag "\"$tracerImageTag\"" --yaml-output -f "$global_basedir"/backend/docker-compose.jq $nodespecs;;

    * ) usage_app;; esac
}
