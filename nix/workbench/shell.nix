{ lib
, profileName
, workbenchDevMode ? false
, useCabalRun ? false
, checkoutWbMode ? "unknown"
}:

with lib;

let
  shellHook = ''
    echo 'workbench shellHook:  workbenchDevMode=${toString workbenchDevMode} useCabalRun=${toString useCabalRun} profileName=${profileName}'
    export WORKBENCH_BACKEND=supervisor
    export WORKBENCH_SHELL_PROFILE=${profileName}

    ${optionalString
      workbenchDevMode
    ''
    export WORKBENCH_CARDANO_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
    export WORKBENCH_EXTRA_FLAGS=

    function wb() {
      $WORKBENCH_CARDANO_NODE_REPO_ROOT/nix/workbench/wb --set-mode ${checkoutWbMode} $WORKBENCH_EXTRA_FLAGS "$@"
    }
    export -f wb
    ''}

    ${optionalString
      useCabalRun
      ''
      . nix/workbench/lib.sh
      . nix/workbench/lib-cabal.sh
      ''}

    export CARDANO_NODE_SOCKET_PATH=run/current/node-0/node.socket
    '';
in
{
  inherit shellHook;
}
