{ pkgs
, lib
, cardanoNodePackages
##
, batchName
, profileName
, backend
##
, cardano-node-rev
, workbench
, workbenchDevMode
, workbenchStartArgs
, profiling
##
, cacheDir              ? "${__getEnv "HOME"}/.cache/cardano-workbench"
}:
let
  backendName = backend.name;

  inherit (backend) stateDir basePort useCabalRun;

  profileData = workbench.materialise-profile
    { inherit profileName backend profiling; };
  backendData = backend.materialise-profile
    { inherit profileData; };
in
  let

    inherit (profileData.value) era composition monetary;

    path = pkgs.lib.makeBinPath path';
    path' =
      [ cardanoNodePackages.bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils
      ]
      ## In dev mode, call the script directly:
      ++ pkgs.lib.optionals (!workbenchDevMode)
      [ workbench.workbench ];

    workbench-interactive-start = pkgs.writeScriptBin "start-cluster" ''
      set -euo pipefail

      export PATH=$PATH:${path}

      wb start \
        --batch-name   ${batchName} \
        --profile-data ${profileData} \
        --backend-data ${backendData} \
        --cache-dir    ${cacheDir} \
        --base-port    ${toString basePort} \
        ${pkgs.lib.optionalString useCabalRun ''--cabal''} \
        ${__concatStringsSep " " workbenchStartArgs} \
        "$@"
    '';

    workbench-interactive-stop = pkgs.writeScriptBin "stop-cluster" ''
      set -euo pipefail

      wb finish "$@"
    '';

    workbench-interactive-restart = pkgs.writeScriptBin "restart-cluster" ''
      set -euo pipefail

      wb run restart "$@" && \
        echo "workbench:  alternate command for this action:  wb run restart" >&2
    '';

    nodeBuildProduct =
      name:
      "report ${name}-log $out ${name}/stdout";

    workbench-profile-run =
      let
        run = pkgs.runCommand "workbench-run-${backendName}-${profileName}"
          { requiredSystemFeatures = [ "benchmark" ];
            nativeBuildInputs = with cardanoNodePackages; with pkgs; [
              bash
              bech32
              coreutils
              gnused
              jq
              moreutils
              nix
              pstree
              workbench.workbench
              zstd
            ]
            ++
            backend.extraShellPkgs
            ;
          }
            ''
            mkdir -p    $out/{cache,nix-support}
            cd          $out
            export HOME=$out

            export WB_BACKEND=${backendName}
            export CARDANO_NODE_SOCKET_PATH=$(wb backend get-node-socket-path ${stateDir} node-0)

            cmd=(
              wb
              start
              --profile-data        ${profileData}
              --backend-data        ${backendData}
              --genesis-cache-entry ${profileData.genesis.files}
              --batch-name          smoke-test
              --base-port           ${toString basePort}
              --node-source         ${pkgs.cardanoNodeProject.args.src}
              --node-rev            ${cardano-node-rev}
              --cache-dir           ./cache
             ${__concatStringsSep " " workbenchStartArgs}
            )
            echo "''${cmd[*]}" > $out/wb-start.sh

            time "''${cmd[@]}" 2>&1 |
              tee $out/wb-start.log
            status=$?
            if test $status != 0
            then echo "wb start failed"
                 cd run/current
                 echo "==========  txgen  stdout:"; cat generator/stdout || true
                 echo "==========  txgen  stderr:"; cat generator/stderr || true
                 echo "==========  node-0 stdout:"; cat node-0/stdout || true
                 echo "==========  node-0 stderr:"; cat node-0/stderr || true
                 wb call fail "wb start failed"
           fi

            ## Convert structure from $out/run/RUN-ID/* to $out/*:
            rm -rf cache
            rm -f run/{current,-current}
            find $out -type s | xargs rm -f
            run=$(cd run; ls)
            (cd run; tar c $run --zstd) > archive.tar.zst
            mv       run/$run/*  .
            rm -rf   run/$run    run

            cat > $out/nix-support/hydra-build-products <<EOF
            report workbench-log   $out wb-start.log
            report meta            $out meta.json
            ${pkgs.lib.concatStringsSep "\n"
              (map nodeBuildProduct (__attrNames profileData.node-specs.value))}
            report archive-tar-zst $out archive.tar.zst
            EOF

            echo "workbench-test:  completed run $run"
            '';
  in
    run // {
      analysis = workbench.run-analysis { inherit pkgs workbench profileData run; };
    };

  overlay = self: super:
    (backend.overlay profileData self super
    //
    {
      inherit workbench-interactive-start;
      inherit workbench-interactive-stop;
      inherit workbench-interactive-restart;
    });
in
{
  inherit profileName profileData;
  inherit backend backendData;
  inherit profiling;
  inherit workbench-profile-run;

  inherit batchName stateDir overlay;

  inherit workbench-interactive-start;
  inherit workbench-interactive-stop;
  inherit workbench-interactive-restart;
}
