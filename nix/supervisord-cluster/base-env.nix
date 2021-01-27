{
  defaultLogConfig
, stateDir
, era
}:

rec {
  inherit stateDir;
  useByronWallet = true;
  relaysNew = "127.0.0.1";
  edgePort = 3001;
  confKey = "local";
  private = false;
  networkConfig = {
    Protocol = "Cardano";
    ShelleyGenesisFile = "shelley/genesis.json";
    ByronGenesisFile   =   "byron/genesis.json";

    RequiresNetworkMagic = "RequiresMagic";
    LastKnownBlockVersion-Major = 0;
    LastKnownBlockVersion-Minor = 0;
    LastKnownBlockVersion-Alt = 0;
  } // ({
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
  }).${era};
  nodeConfig = networkConfig // defaultLogConfig;
  consensusProtocol = networkConfig.Protocol;
}
