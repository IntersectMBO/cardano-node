{
  defaultLogConfig
, stateDir
}:

rec {
  inherit stateDir;
  useByronWallet = true;
  relaysNew = "127.0.0.1";
  edgePort = 3001;
  confKey = "local";
  private = false;
  networkConfig = {
    ByronGenesisFile = "byron/genesis.json";
    ShelleyGenesisFile = "shelley/genesis.json";
    Protocol = "Cardano";
    RequiresNetworkMagic = "RequiresMagic";
    LastKnownBlockVersion-Major = 0;
    LastKnownBlockVersion-Minor = 0;
    LastKnownBlockVersion-Alt = 0;
    PBftSignatureThreshold = 0.9;
    MaxKnownMajorProtocolVersion = 2;

  };
  nodeConfig = networkConfig // defaultLogConfig;
  consensusProtocol = networkConfig.Protocol;
}
