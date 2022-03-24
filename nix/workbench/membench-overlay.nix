{ input
, cardano-mainnet-mirror
, node-snapshot
, node-measured
, node-process
, customConfig
, lib
}:
self: super: {
  ## Do not let this overlay escape -- exposure to other flakes will break down due to inputs being inherited.
  ## 0. Chain
  mainnet-chain = cardano-mainnet-mirror.defaultPackage.x86_64-linux;

  # TODO, fix this
  #db-analyser = ouroboros-network-snapshot.haskellPackages.ouroboros-consensus-cardano.components.exes.db-analyser;

  ## 1. Ledger snapshot
  inherit node-snapshot;
  db-analyser = node-snapshot.packages.x86_64-linux.db-analyser;
  snapshot = self.callPackage ./snapshot.nix
    { chain = self.mainnet-chain;
      inherit (customConfig) snapshotSlot finalChunkNo shelleyGenesisHash;
      inherit input;
    };

  ## 2. Node under measurement
  inherit node-measured;

  ## 3. Single run
  membench-run = self.callPackage ./membench-run.nix { inherit (customConfig) rtsflags rtsMemSize; inherit input; };

  ## 4. Run batch:  profiles X iterations
  inherit node-process;
  membench-batch = self.callPackage ./membench-batch.nix { inherit (customConfig) variantTable; inherit input; };

  ## 5. Data aggregation and statistics
  membench-batch-results = self.callPackage ./membench-batch-process.nix { inherit input; };

  ## 6. Report generation
  membench-batch-report = self.callPackage ./membench-batch-report.nix { inherit input; };
  membench-test-report = self.callPackage ./membench-batch-report.nix
    rec
    { inherit input;
      membench-batch = self.callPackage ./membench-batch.nix { inherit (customConfig) variantTable; inherit input; nIterations = 1; };
      membench-batch-results = self.callPackage ./membench-batch-process.nix { inherit input membench-batch; };
    };
}
