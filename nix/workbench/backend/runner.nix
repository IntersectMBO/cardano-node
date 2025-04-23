{ pkgs, lib
## The binaries/scripts to use when calling the workbench.
, cardanoNodeProject
, cardanoNodePackages
, workbench # The derivation.
## Profile dependent parameters.
, profile
, backend
## This run parameters.
, batchName
, workbenchStartArgs
##
, cardano-node-rev
}:
let

  # Profile specification and directory data and "run" description.
  ##############################################################################

  profileName = profile.name;
  inherit (profile) profiling;

  backendName = backend.name;
  inherit (backend) stateDir basePort useCabalRun;

  profileBundle  = profile.profileBundle { inherit backend; };

  profileDataDir = profile.materialise-profile { inherit profileBundle; };
  backendDataDir = backend.materialise-profile { inherit profileBundle; };

  # Workbench shell setup.
  ##############################################################################

  cacheDir = "${__getEnv "HOME"}/.cache/cardano-workbench";

  # recover CHaP location from cardano's project
  chap = cardanoNodeProject.args.inputMap."https://chap.intersectmbo.org/";
  # build plan as computed by nix
  nixPlanJson = cardanoNodeProject.plan-nix.json;

  # Optimize cache hits setting gitrev using bash once inside the shell.
  workbench-envars =
    ''
    export WB_CHAP_PATH=${chap}
    export WB_NIX_PLAN=${nixPlanJson}
    export WB_SHELL_PROFILE=${profileName}
    export WB_SHELL_PROFILE_DATA=${profileDataDir}
    export WB_BACKEND=${backendName}
    export WB_BACKEND_DATA=${backendDataDir}
    export WB_CREATE_TESTNET_DATA=''${WB_CREATE_TESTNET_DATA:-1}
    export WB_DEPLOYMENT_NAME=''${WB_DEPLOYMENT_NAME:-$(basename $(pwd))}
    export WB_MODULAR_GENESIS=''${WB_MODULAR_GENESIS:-0}
    export WB_LOCLI_DB=''${WB_LOCLI_DB:-1}
    if test -z "$(git status --porcelain --untracked-files=no)"
    then export WB_GITREV="$(git rev-parse HEAD)"
    else export WB_GITREV="0000000000000000000000000000000000000000"
    fi
    export CARDANO_NODE_SOCKET_PATH=${stateDir}/node-0/node.socket
    ''
    + lib.optionalString (profileBundle.profile.value.scenario == "chainsync") (
      let cardano-mainnet-mirror =
            # "nix" branch last commit 819488be9eabbba6aaa7c931559bc584d8071e3d
            __getFlake "github:input-output-hk/cardano-mainnet-mirror/nix";
      in
        ''
        export CARDANO_MAINNET_MIRROR=${cardano-mainnet-mirror.outputs.defaultPackage.x86_64-linux.outPath}
        ''
    )
  ;

  workbench-interactive-start = pkgs.writeScriptBin "start-cluster"
    ''
    set -euo pipefail

    wb start \
      --batch-name   ${batchName} \
      --profile-data ${profileDataDir} \
      --backend-data ${backendDataDir} \
      --cache-dir    ${cacheDir} \
      --base-port    ${toString basePort} \
      ${pkgs.lib.optionalString useCabalRun ''--cabal''} \
      ${__concatStringsSep " " workbenchStartArgs} \
      "$@"
    ''
  ;

  workbench-interactive-stop = pkgs.writeScriptBin "stop-cluster"
    ''
    set -euo pipefail

    wb finish "$@"
    ''
  ;

  workbench-interactive-restart = pkgs.writeScriptBin "restart-cluster"
    ''
    set -euo pipefail

    wb run restart "$@" && \
      echo "workbench:  alternate command for this action:  wb run restart" >&2
    ''
  ;

  # Automatic cluster/profile run for CI smoke test.
  ##############################################################################

  # A workbench with the dependencies for this environment / run config.
  workbench-deps-hydra =
    [workbench]
    ++
    (with pkgs;
      [
        bash
        coreutils
        moreutils  # `sponge` (genesis)
        util-linux # flock    (acquire_lock)
        gnused
        procps
        nix
        git        # `workbench-envars` set the gitrev
        jq
      ]
    )
    ++
    (with cardanoNodePackages;
      [
        cardano-cli
        locli
      ]
    )
  ;

  workbench-profile-run =
    let
      profileJson = profileBundle.profile.JSON;
      nodeSpecsJson = profileBundle.node-specs.JSON;
      # Genesis files creation.
      genesisFiles = pkgs.runCommand "workbench-run-genesis-${profileName}"
        { requiredSystemFeatures = [ "benchmark" ];
          nativeBuildInputs = workbench-deps-hydra;
        }
        ''
        mkdir $out

        cache_key_input=$(wb genesis profile-cache-key-input ${profileJson})
        cache_key=$(      wb genesis profile-cache-key       ${profileJson})

        genesis_keepalive() {
          while test ! -e $out/profile
          do
            echo 'genesis_keepalive for Hydra'
            sleep 10s
          done
        }
        genesis_keepalive &
        __genesis_keepalive_pid=$!
        __genesis_keepalive_termination() {
          kill $__genesis_keepalive_pid 2>/dev/null || true
        }
        trap __genesis_keepalive_termination EXIT

        time \
          wb genesis actually-genesis \
            "${profileJson}"          \
            "${nodeSpecsJson}"        \
            "$out"                    \
            "$cache_key_input"        \
            "$cache_key"

        touch done
        ''
      ;
      # Run the profile.
      nodeBuildProduct =
        name:
        "report ${name}-log $out ${name}/stdout"
      ;
      run = pkgs.runCommand "workbench-run-${profileName}-${backendName}"
        { requiredSystemFeatures = [ "benchmark" ];
          nativeBuildInputs = with pkgs;
            workbench-deps-hydra
            ++
            [ zstd ]
            ++
            backend.extraShellPkgs
          ;
        }
        ''
        mkdir -p    $out/{cache,nix-support}
        cd          $out
        export HOME=$out

        ${workbench-envars}

        cmd=(
          wb
          start
          --profile-data        ${profileDataDir}
          --backend-data        ${backendDataDir}
          --genesis-cache-entry ${genesisFiles}
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
          (map nodeBuildProduct (__attrNames profileBundle.node-specs.value))}
        report archive-tar-zst $out archive.tar.zst
        EOF

        echo "workbench-test:  completed run $run"
        ''
      ;
      # Analyze the run.
      trace = false;
      analysis = pkgs.runCommand "workbench-run-analysis-${profileName}"
        { requiredSystemFeatures = [ "benchmark" ];
          nativeBuildInputs = workbench-deps-hydra;
        }
        ''
        echo "analysing run:  ${run}"

        ln -s ${run} $out/run

        cmd=(
            wb
            ${pkgs.lib.optionalString trace "--trace"}
            analyse
            # --filters size-full
            --outdir  $out
            standard
            ${run}
            )
        echo "''${cmd[*]}" > $out/wb-analyse.sh

        ''${cmd[@]} 2>&1 |
            tee $out/wb-analyse.log

        cd $out
        for x in $(ls *.json *.org *.txt | grep -v 'flt\.json$')
        do echo "report $x $out $x" >> $out/nix-support/hydra-build-products
        done
        EOF
        ''
      ;
    in
      run // {inherit analysis;}
  ;

in
{
  inherit backend;
  inherit profiling;

  inherit workbench-envars;
  inherit workbench-interactive-start;
  inherit workbench-interactive-stop;
  inherit workbench-interactive-restart;

  inherit workbench-profile-run;
}
