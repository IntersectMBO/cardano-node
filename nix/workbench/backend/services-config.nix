{ lib
, workbench
##
, basePort              ? 30000
, stateDir              ? "run/current"
, useCabalRun           ? false
, enableEKG             ? true
}:
with lib;
{
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
            # Make the shell function take over.
            executable = "cardano-node";
            # executable     = ''time -f "${time_fmtstr}" -o kernel-resource-summary.json cabal run exe:cardano-node ''${WB_FLAGS_RTS} -- +RTS -sghc-rts-report.txt -RTS'';
          } // optionalAttrs isProducer {
            operationalCertificate = "./genesis/node-keys/node${toString i}.opcert";
            kesKey         = "./genesis/node-keys/node-kes${toString i}.skey";
            vrfKey         = "./genesis/node-keys/node-vrf${toString i}.skey";
          } // optionalAttrs profile.node.tracer {
            tracerSocketPathConnect = "../tracer/tracer.socket";
          });

      finaliseNodeConfig =
        { port, ... }: cfg: recursiveUpdate cfg
          (
            {
              AlonzoGenesisFile    = "./genesis/genesis.alonzo.json";
              ShelleyGenesisFile   = "./genesis/genesis-shelley.json";
              ByronGenesisFile     = "./genesis/byron/genesis.json";
            }
            // optionalAttrs enableEKG
            (let portShiftEkg        = 100;
                 portShiftPrometheus = 200;
            in {
              hasEKG = port + portShiftEkg;
              hasPrometheus = ["127.0.0.1" (port + portShiftPrometheus)];
              setupBackends = [
                "EKGViewBK"
              ];
            })
          );

      finaliseNodeArgs =
        profile: nodeSpec: args: args;

      finaliseGeneratorService =
        svc: recursiveUpdate svc
          ({
            sigKey         = "./genesis/utxo-keys/utxo1.skey";
            nodeConfigFile = "config.json";
            runScriptFile  = "run-script.json";
            ## path to the socket of the locally running node.
            localNodeSocketPath = "./node-0/node.socket";
          } // optionalAttrs useCabalRun {
            executable     = "cabal run exe:tx-generator --";
          });

      finaliseGeneratorConfig =
        cfg: recursiveUpdate cfg
          ({
            AlonzoGenesisFile    = "./genesis/genesis.alonzo.json";
            ShelleyGenesisFile   = "./genesis/genesis-shelley.json";
            ByronGenesisFile     = "./genesis/byron/genesis.json";
          } // optionalAttrs useCabalRun {
            executable           = "tx-generator";
          });

      finaliseTracerService =
        svc: recursiveUpdate svc
          ({
            configFile     = "config.json";
            logRoot        = ".";
          } // optionalAttrs useCabalRun {
            executable     = "cardano-tracer";
          });

}
