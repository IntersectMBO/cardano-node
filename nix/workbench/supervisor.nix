let
  basePort              = 30000;
  cacheDirDefault       = "${__getEnv "HOME"}/.cache/cardano-workbench";
  stateDir              = "run/current";
in
{ pkgs
, lib
, workbench
##
, cacheDir              ? cacheDirDefault
, extraSupervisorConfig ? {}
, useCabalRun           ? false
, enableEKG             ? true
##
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
        let time_fmtstr =
              "{ " + escape [''"''] (concatStringsSep ''\n, '' time_entries) + " }";
            time_entries = [
              ''"wall_clock_s":       %e''
              ''"user_cpu_s":         %U''
              ''"sys_cpu_s":          %S''
              ''"avg_cpu_pct":       "%P"''
              ''"rss_peak_kb":        %M''
              ''"signals_received":   %k''
              ''"ctxsw_involuntary":  %c''
              ''"ctxsw_volunt_waits": %w''
              ''"pageflt_major":      %F''
              ''"pageflt_minor":      %R''
              ''"swaps":              %W''
              ''"io_fs_reads":        %I''
              ''"io_fs_writes":       %O''
              ''"cmdline":           "%C"''
              ''"exit_code":          %x''
            ];
        in
        profile: { name, i, isProducer, ... }: svc: recursiveUpdate svc
          ({
            stateDir       = stateDir + "/${name}";
            ## Everything is local in the supervisord setup:
            socketPath     = "node.socket";
            topology       = "topology.json";
            nodeConfigFile = "config.json";
          } // optionalAttrs useCabalRun {
            executable     = ''time -f "${time_fmtstr}" -o kernel-resource-summary.json cabal run exe:cardano-node -- +RTS -sghc-rts-report.txt -RTS'';
          } // optionalAttrs isProducer {
            operationalCertificate = "../genesis/node-keys/node${toString i}.opcert";
            kesKey         = "../genesis/node-keys/node-kes${toString i}.skey";
            vrfKey         = "../genesis/node-keys/node-vrf${toString i}.skey";
          } // optionalAttrs profile.node.tracer {
            tracerSocketPathConnect = "../tracer/tracer.socket";
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
        profile: nodeSpec: args: args;

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

      finaliseTracerService =
        svc: recursiveUpdate svc
          ({
            configFile     = "config.json";
            logRoot        = ".";
          } // optionalAttrs useCabalRun {
            executable     = "cabal run exe:cardano-tracer --";
          });

      materialise-profile =
        { profileNix }:
        pkgs.runCommand "workbench-profile-outputs-${profileNix.name}-supervisord" {}
          ''
          mkdir $out
          cp ${supervisord.mkSupervisorConf profileNix} $out/supervisor.conf
          '';

      ## IMPORTANT:  keep in sync with envArgs in 'workbench/default.nix/generateProfiles/environment'.
      env-args-base =
        {
          inherit (pkgs) cardanoLib;
          inherit stateDir cacheDir basePort;
          staggerPorts = true;
        };

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

  all-profiles =
    workbench.all-profiles
      { inherit backend;
        envArgs = backend.env-args-base;
      };
in
{
  inherit cacheDir stateDir basePort;
  inherit workbench;
  inherit backend;
  inherit all-profiles;
}
