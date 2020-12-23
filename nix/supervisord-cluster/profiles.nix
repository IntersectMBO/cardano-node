{ ...
}:

rec {
  default = {
    composition = {
      numBft = 1;
      numPools = 2;
    };
    monetary = {
      delegatePoolAmount = 1000000000000;
      initialFunds = import ./initial-funds.nix;
    };
    genesis = {
      protocolParams = {
        poolDeposit = 500000000;
        keyDeposit = 400000;
        nOpt = 10;
        rho = 0.0022;
        tau = 0.05;
        a0 = 0.3;
        minFeeA = 44;
        minFeeB = 155381;
        decentralisationParam = 0.8;
      };
      slotLength = 0.2;
      activeSlotsCoeff = 0.1;
      securityParam = 10;
      epochLength = 1000;
      maxLovelaceSupply = 45000000000000000;
      networkMagic = 42;
    };
  };
}
