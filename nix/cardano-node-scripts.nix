{ commonLib
, cardanoNode
, customConfig
}:

let
  mkEdgeTopology = { hostAddr, port, edgeHost, edgePort, nodeId, valency ? 1 }:
  let
    topology = [
      {
        inherit nodeId;
        nodeAddress = {
          addr = hostAddr;
          inherit port;
        };
        producers = [
          {
            addr = edgeHost;
            port = edgePort;
            inherit valency;
          }
        ];
      }
    ];
  in builtins.toFile "topology.yaml" (builtins.toJSON topology);
  pkgs = commonLib.pkgs;
  inherit (pkgs) lib;
  mkNodeScript = environment: let
    envConfig = commonLib.environments.${environment};
    config = {
      consensusProtocol = "real-pbft";
      hostAddr = "127.0.0.1";
      port = 3001;
      signingKey = null;
      delegationCert = null;
      loggingConfig = ../configuration/log-configuration.yaml;
      pbftThreshold = null;
      nodeId = 0;
    } // envConfig
      // customConfig;
      topologyFile = if (config ? topologyFile) then topologyFile else mkEdgeTopology {
        inherit (config) hostAddr port nodeId;
        edgeHost = "127.0.0.1";
        edgePort = 7777;

      };
  in pkgs.writeScript "cardano-node-${environment}" ''
        ${cardanoNode}/bin/cardano-node \
          --genesis-file ${config.genesisFile} \
          --genesis-hash ${config.genesisHash} \
          --log-config ${config.loggingConfig} \
          node \
          --topology ${topologyFile} \
          --${config.consensusProtocol} \
          --node-id ${builtins.toString config.nodeId} \
          --host-addr ${config.hostAddr} \
          --port ${builtins.toString config.port} \
          ${lib.optionalString (config.pbftThreshold != null) "--pbft-signature-threshold ${config.pbftThreshold} \\"}
          ${lib.optionalString (config.signingKey != null) "--signing-key ${config.signingKey} \\"}
          ${lib.optionalString (config.delegationCert != null) "--delegation-certificate ${config.deleglationCert} \\"}
  '';

in with builtins; listToAttrs (map
  (e: {name = e; value = mkNodeScript e;})
  (attrNames commonLib.environments)
)
