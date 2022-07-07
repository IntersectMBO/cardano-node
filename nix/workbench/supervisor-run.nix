let
  batchNameDefault   = "plain";
  profileNameDefault = "default-bage";
in

{ pkgs
, cardanoNodePackages
, supervisord-workbench
##
, profileName           ? profileNameDefault
, batchName             ? batchNameDefault
##
, workbenchDevMode      ? false
}:

let
  inherit (supervisord-workbench) workbench backend cacheDir stateDir basePort;

  with-supervisord-profile =
    { envArgsOverride ? {} }:
    workbench.with-profile
      { inherit backend profileName;
        envArgs = supervisord-workbench.env-args-base // envArgsOverride;
      };

  inherit
    (with-supervisord-profile {})
    profileNix profile topology genesis;
in

let

  inherit (profile.value) era composition monetary;

  path = pkgs.lib.makeBinPath path';
  path' =
    [ cardanoNodePackages.bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils
    ]
    ## In dev mode, call the script directly:
    ++ pkgs.lib.optionals (!workbenchDevMode)
    [ workbench.workbench ];

  interactive-start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail

    export PATH=$PATH:${path}

    wb start \
        --batch-name   ${batchName} \
        --profile-name ${profileName} \
        --profile      ${profile} \
        --cache-dir    ${cacheDir} \
        --base-port    ${toString basePort} \
        ''${WORKBENCH_CABAL_MODE:+--cabal} \
        "$@"
  '';

  interactive-stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail

    wb finish "$@"
  '';

  interactive-restart = pkgs.writeScriptBin "restart-cluster" ''
    set -euo pipefail

    wb run restart "$@" && \
        echo "workbench:  alternate command for this action:  wb run restart" >&2
  '';

  nodeBuildProducts =
    name:
    "report ${name}.log $out ${name}/stdout";

  profile-run =
    { trace ? false }:
    let
      inherit
        (with-supervisord-profile
          { envArgsOverride = { cacheDir = "./cache"; stateDir = "./"; }; })
        profileNix profile topology genesis;

      run = pkgs.runCommand "workbench-run-supervisord-${profileName}"
        { requiredSystemFeatures = [ "benchmark" ];
          nativeBuildInputs = with cardanoNodePackages; with pkgs; [
            bash
            bech32
            coreutils
            gnused
            jq
            moreutils
            nixWrapped
            pstree
            python3Packages.supervisor
            workbench.workbench
          ];
        }
          ''
          mkdir -p $out/{cache,nix-support}
          cd       $out

          export WORKBENCH_BACKEND=supervisor
          export CARDANO_NODE_SOCKET_PATH=$(wb backend get-node-socket-path ${stateDir})

          cmd=(
              wb
              ${pkgs.lib.optionalString trace "--trace"}
              start
              --profile-name        ${profileName}
              --profile             ${profile}
              --topology            ${topology}
              --genesis-cache-entry ${genesis}
              --batch-name          smoke-test
              --base-port           ${toString basePort}
              --cache-dir           ./cache
          )
          echo "''${cmd[*]}" > $out/wb-start.sh

          time "''${cmd[@]}" 2>&1 |
              tee $out/wb-start.log

          ## Convert structure from $out/run/RUN-ID/* to $out/*:
          rm -rf cache
          rm -f run/{current,-current}
          tag=$(cd run; ls)
          mv       run/$tag/*   .
          rmdir    run/$tag run
          rm -f    node-*/node.socket

          cat > $out/nix-support/hydra-build-products <<EOF
          report workbench.log $out wb-start.log
          report meta.json     $out meta.json
          ${pkgs.lib.concatStringsSep "\n"
            (map nodeBuildProducts (__attrNames profileNix.node-specs.value))}
          report node-0        $out meta.json
          EOF

          echo "workbench-test:  completed run $tag"
          '';
    in
      run // {
        analysis = workbench.run-analysis { inherit pkgs workbench profileNix run; };
      };
in
{
  inherit stateDir;
  inherit workbench supervisord-workbench;
  inherit profileNix profile topology genesis;
  inherit interactive-start interactive-stop interactive-restart;
  inherit profile-run;
}
