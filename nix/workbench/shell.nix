{ lib
, profileName
, workbenchDevMode ? false
, useCabalRun ? false
, profiled ? false
}:

with lib;

let
  shellHook = ''
    echo 'workbench shellHook:  workbenchDevMode=${toString workbenchDevMode} useCabalRun=${toString useCabalRun} profiled=${toString profiled} profileName=${profileName}'
    export WB_BACKEND=supervisor
    export WB_SHELL_PROFILE=${profileName}

    ${optionalString
      workbenchDevMode
    ''
    export WB_CARDANO_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
    export WB_EXTRA_FLAGS=

    function wb() {
      $WB_CARDANO_NODE_REPO_ROOT/nix/workbench/wb $WB_EXTRA_FLAGS "$@"
    }
    export -f wb
    ''}

    ${optionalString
      useCabalRun
      ''
      . nix/workbench/lib.sh
      . nix/workbench/lib-cabal.sh ${optionalString profiled "--profiled"}
      ''}

    export CARDANO_NODE_SOCKET_PATH=run/current/node-0/node.socket
    '';
in
{
  inherit shellHook;
}
