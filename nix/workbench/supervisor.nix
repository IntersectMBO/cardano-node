let
  basePortDefault    = 30000;
  cacheDirDefault    = "${__getEnv "HOME"}/.cache/cardano-workbench";
  stateDirDefault    = "run/current";
  profileNameDefault = "default-alzo";
in
{ pkgs
, cardanoNodePackages
, workbench
, lib
, bech32
, basePort              ? basePortDefault
, stateDir              ? stateDirDefault
, cacheDir              ? cacheDirDefault
, extraSupervisorConfig ? {}
, useCabalRun           ? false
, workbenchDevMode      ? false
, enableEKG             ? true
##
, profileName           ? profileNameDefault
, batchName             ? "plain"
, ...
}:
with lib;
let
  backend =
    rec
    { name = "supervisor";
      ## Generic Nix bits:
      topologyForNodeSpec =
        { profile, nodeSpec }:
        let inherit (nodeSpec) name i; in
        workbench.runWorkbench
          "topology-${name}.json"
          "topology projection-for local-${nodeSpec.kind} ${toString i} ${profile.name} ${profile.topology.files} ${toString basePort}";

      nodePublicIP =
        { i, name, ... }@nodeSpec:
        "127.0.0.1";

      finaliseNodeService =
        { name, i, isProducer, ... }: svc: recursiveUpdate svc
          ({
            stateDir       = stateDir + "/${name}";
            ## Everything is local in the supervisord setup:
            socketPath     = "node.socket";
            topology       = "topology.json";
            nodeConfigFile = "config.json";
          } // optionalAttrs useCabalRun {
            executable     = "cabal run exe:cardano-node --";
          } // optionalAttrs isProducer {
            operationalCertificate = "../genesis/node-keys/node${toString i}.opcert";
            kesKey         = "../genesis/node-keys/node-kes${toString i}.skey";
            vrfKey         = "../genesis/node-keys/node-vrf${toString i}.skey";
          });

      finaliseNodeConfig =
        { port, ... }: cfg: recursiveUpdate cfg
          ({
            AlonzoGenesisFile    = "../genesis.alonzo.json";
            ShelleyGenesisFile   = "../genesis-shelley.json";
            ByronGenesisFile     = "../genesis/byron/genesis.json";
          } // optionalAttrs enableEKG {
            hasEKG               = port + supervisord.portShiftEkg;
            hasPrometheus        = [ "127.0.0.1" (port + supervisord.portShiftPrometheus) ];
            setupBackends = [
              "EKGViewBK"
            ];
          });

      finaliseNodeArgs =
        { port, ... }: cfg: cfg;

      finaliseGeneratorService =
        svc: recursiveUpdate svc
          ({
            sigKey         = "../genesis/utxo-keys/utxo1.skey";
            nodeConfigFile = "config.json";
            runScriptFile  = "run-script.json";
          } // optionalAttrs useCabalRun {
            executable     = "cabal run exe:tx-generator --";
          });

      finaliseGeneratorConfig =
        cfg: recursiveUpdate cfg
          ({
            AlonzoGenesisFile    = "../genesis.alonzo.json";
            ShelleyGenesisFile   = "../genesis-shelley.json";
            ByronGenesisFile     = "../genesis/byron/genesis.json";
          });

      materialise-profile =
        { profileNix }:
        pkgs.runCommand "workbench-profile-outputs-${profileNix.name}-supervisord" {}
          ''
          mkdir $out
          cp ${supervisord.mkSupervisorConf profileNix} $out/supervisor.conf
          '';

      ## Backend-specific Nix bits:
      supervisord =
        {
          inherit
            extraSupervisorConfig;

          portShiftEkg        = 100;
          portShiftPrometheus = 200;

          ## mkSupervisorConf :: Profile -> SupervisorConf
          mkSupervisorConf =
            profile:
            pkgs.callPackage ./supervisor-conf.nix
            { inherit (profile) node-services generator-service;
              inherit
                pkgs lib stateDir
                basePort
                extraSupervisorConfig;
            };
        };
    };

  with-supervisord-profile =
    { envArgsOverride ? {} }:
    workbench.with-profile
      { inherit backend profileName;

        ## IMPORTANT:  keep in sync with envArgs in 'workbench/default.nix/generateProfiles/environment'.
        envArgs =
          {
            inherit (pkgs) cardanoLib;
            inherit stateDir cacheDir basePort;
            staggerPorts = true;
          }
          // envArgsOverride;
      };

  inherit
    (with-supervisord-profile {})
    profile profile-output topology-output genesis-output;
in

let

  inherit (profile.value) era composition monetary;

  path = makeBinPath path';
  path' =
    [ bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils
    ]
    ## In dev mode, call the script directly:
    ++ optionals (!workbenchDevMode)
    [ workbench.workbench ];

  interactive-start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail

    export PATH=$PATH:${path}

    set -x
    wb start \
        --batch-name   ${batchName} \
        --profile-name ${profileName} \
        --profile      ${profile} \
        --cache-dir    ${cacheDir} \
        --base-port    ${toString basePort} \
        ''${WORKBENCH_CABAL_MODE:+--cabal} \
        "$@" &&
        echo 'workbench:  cluster started. Run `stop-cluster` to stop' >&2
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
  inherit workbench;
  inherit profile stateDir;
  inherit interactive-start interactive-stop interactive-restart;
  inherit profile-run;
}
