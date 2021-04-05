{ pkgs
, lib
, stateDir
, basePort
, nodeSpecs                ## :: Map NodeName NodeSpec
, profile
, useCabalRun
, defaultLogConfig
}:

with lib;

let

  ## The AWS node is started with:
  ## cardano-node run
  ## --config /nix/store/nywkyj205skkqy27ip3p0678977kxq0b-config-1-0.json
  ## --database-path /var/lib/cardano-node/db-bench-dense-k51-10ep-2000kU-500kD-6600esec
  ## --topology /nix/store/sb8gn8wb4s8m7a4pmkb6hvlr4fhy5vn2-topology.yaml
  ## --shelley-vrf-key /var/lib/keys/cardano-node-vrf-signing
  ## --shelley-kes-key /var/lib/keys/cardano-node-kes-signing
  ## --shelley-operational-certificate /var/lib/keys/cardano-node-operational-cert
  ## +RTS -l-agu -t --machine-readable -RTS +RTS -N2 -A16m -qg -qb -M14336.000000M -RTS
  ##
  ## nodeSpecServiceConfig :: NodeSpec -> ServiceConfig
  ##
  nodeSpecServiceConfig =
    { name, i, kind, port, isProducer }:
    {
      stateDir       = stateDir + "/${name}";
      socketPath     = "node.socket";
      topology       = "topology.json";
      nodeConfigFile = "config.json";
      dbPrefix       = "db";
      port           = nodeIndexToNodePort i;
      nodeConfig =
        lib.recursiveUpdate defaultLogConfig
          ({
            Protocol             = "Cardano";
            RequiresNetworkMagic = "RequiresMagic";
            ShelleyGenesisFile   = "../shelley/genesis.json";
            ByronGenesisFile     =   "../byron/genesis.json";

            hasEKG           = nodeIndexToEkgPort i;
            hasPrometheus    = [ "127.0.0.1" (nodeIndexToPrometheusPort i) ];

            TracingVerbosity = "NormalVerbosity";
            minSeverity      = "Debug";

            TraceMempool     = true;
            TraceTxInbound   = true;

            defaultScribes = [
              [ "StdoutSK" "stdout" ]
            ];
            setupScribes =
              [{
                scKind     = "StdoutSK";
                scName     = "stdout";
                scFormat   = "ScJson";
              }];
            options = {
              mapBackends = {
                "cardano.node.resources" = [ "KatipBK" ];
              };
            };

            LastKnownBlockVersion-Major = 0;
            LastKnownBlockVersion-Minor = 0;
            LastKnownBlockVersion-Alt   = 0;
          } //
          ({
            shelley =
              { TestShelleyHardForkAtEpoch = 0;
              };
            allegra =
              { TestShelleyHardForkAtEpoch = 0;
                TestAllegraHardForkAtEpoch = 0;
              };
            mary =
              { TestShelleyHardForkAtEpoch = 0;
                TestAllegraHardForkAtEpoch = 0;
                TestMaryHardForkAtEpoch    = 0;
              };
          }).${profile.era});
    } // optionalAttrs isProducer {
      operationalCertificate = "../shelley/node-keys/node${toString i}.opcert";
      kesKey                 = "../shelley/node-keys/node-kes${toString i}.skey";
      vrfKey                 = "../shelley/node-keys/node-vrf${toString i}.skey";
    } // optionalAttrs useCabalRun {
      executable = "cabal run exe:cardano-node --";
    };

  nodeIndexToNodePort       = i: basePort +   0 + i;
  nodeIndexToEkgPort        = i: basePort + 100 + i;
  nodeIndexToPrometheusPort = i: basePort + 200 + i;

  ## Given an env config, evaluate it and produce the node service.
  ## Call the given function on this service.
  ##
  ## nodeServiceConfigService :: NodeServiceConfig -> NodeService
  ##
  nodeServiceConfigService =
    serviceConfig:
    let
    systemdCompat.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      users = mkOption {};
      assertions = mkOption {};
    };
    eval = let
      extra = {
        services.cardano-node = {
          enable = true;
        } // serviceConfig;
      };
    in evalModules {
      prefix = [];
      modules = import ../nixos/module-list.nix ++ [ systemdCompat extra ];
      args = { inherit pkgs; };
    };
    in eval.config.services.cardano-node;

  ##
  ## nodeSetups :: Map NodeName (NodeSpec, NodeConfig, ServiceConfig, Script)
  ##
  nodeSetups = mapAttrs
    (_: nodeSpec:
      let
        nodeServiceConfig = nodeSpecServiceConfig nodeSpec;
        nodeService = nodeServiceConfigService nodeServiceConfig;
      in {
        inherit nodeSpec nodeServiceConfig nodeService;

        startupScript =
          pkgs.writeScript "cardano-node"
            ''
            #!${pkgs.stdenv.shell}
            ${nodeService.script}
            '';
      })
    nodeSpecs;
in
{
  inherit nodeSetups;
  inherit
    nodeIndexToNodePort
    nodeIndexToEkgPort
    nodeIndexToPrometheusPort;
}
