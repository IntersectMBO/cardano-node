{ pkgs
, lib
, cardanoNodePackages
##
, stateDir
, batchName
, profileName
, backend
##
, cardano-node-rev      ? "0000000000000000000000000000000000000000"
, workbench
, workbenchDevMode      ? false
##
, cacheDir              ? "${__getEnv "HOME"}/.cache/cardano-workbench"
, basePort              ? 30000
}:
let
  backendName = backend.name;

  inherit (backend) useCabalRun;

  with-backend-profile = workbench.with-profile
    { inherit stateDir profileName backend basePort workbench; };

  inherit (with-backend-profile) profileNix profile topology genesis;
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
        ${pkgs.lib.optionalString useCabalRun ''--cabal''} \
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

    nodeBuildProduct =
      name:
      "report ${name}-log $out ${name}/stdout";

    profile-run =
      { trace ? false }:
      let
        inherit
          (with-backend-profile)
          profileNix profile topology genesis;

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
              ${pkgs.lib.optionalString trace "--trace"}
              start
              --profile-name        ${profileName}
              --profile             ${profile}
              --topology            ${topology}
              --genesis-cache-entry ${genesis}
              --batch-name          smoke-test
              --base-port           ${toString basePort}
              --node-source         ${pkgs.cardanoNodeProject.args.src}
              --node-rev            ${cardano-node-rev}
              --cache-dir           ./cache
            )
            echo "''${cmd[*]}" > $out/wb-start.sh

            time "''${cmd[@]}" 2>&1 |
              tee $out/wb-start.log

            ## Convert structure from $out/run/RUN-ID/* to $out/*:
            rm -rf cache
            rm -f run/{current,-current}
            find $out -type s | xargs rm -f
            run=$(cd run; ls)
            (cd run; tar c $run --zstd) > archive.tar.zst
            mv       run/$run/*  .
            rmdir    run/$run    run

            cat > $out/nix-support/hydra-build-products <<EOF
            report workbench-log   $out wb-start.log
            report meta            $out meta.json
            ${pkgs.lib.concatStringsSep "\n"
              (map nodeBuildProduct (__attrNames profileNix.node-specs.value))}
            report archive-tar-zst $out archive.tar.zst
            EOF

            echo "workbench-test:  completed run $run"
            '';
  in
    run // {
      analysis = workbench.run-analysis { inherit pkgs workbench profileNix run; };
    };
in
{
  inherit stateDir batchName profileName backend;
  inherit workbench;
  inherit profileNix profile topology genesis;
  inherit interactive-start interactive-stop interactive-restart;
  inherit profile-run;
}
