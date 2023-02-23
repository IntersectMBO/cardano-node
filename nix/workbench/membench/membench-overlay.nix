{ input
, cardano-mainnet-mirror
, node-snapshot
, node-measured
, node-process
, customConfig
, lib
}:

self: super:

let
  mkMembench =
    nIterations: { node, rev }:
    rec {
      ## 4. Run batch:  profiles X iterations
      batch = self.callPackage ./membench-batch.nix
        { inherit (customConfig) variantTable;
          inherit nIterations;
          node-measured-rev = rev;

          ## 3. Single run
          membench-run = self.callPackage ./membench-run.nix
            { inherit input;
              inherit (customConfig) rtsflags rtsMemSize;
              node-measured = node;
              node-measured-rev = rev;
            };
        };
      ## 5. Data aggregation and statistics
      batch-results = self.callPackage ./membench-batch-process.nix
        { inherit input batch; };

      ## 6. Report generation
      batch-report = self.callPackage ./membench-batch-report.nix
        { inherit input batch batch-results; };
    };
  node-self-rev = input.self.rev or "0000000000000000000000000000000000000000";
in
{
  ## Do not let this overlay escape -- exposure to other flakes will break down due to inputs being inherited.
  ## 0. Chain
  mainnet-chain = cardano-mainnet-mirror.defaultPackage.${self.system};

  # TODO, fix this
  #db-analyser = ouroboros-network-snapshot.haskellPackages.ouroboros-consensus-cardano.components.exes.db-analyser;

  ## 1. Ledger snapshot
  inherit node-snapshot;
  db-analyser = node-snapshot.packages.${self.system}.db-analyser;
  snapshot = self.callPackage ./snapshot.nix
    { chain = self.mainnet-chain;
      inherit (customConfig) snapshotSlot finalChunkNo shelleyGenesisHash;
      inherit input;
    };

  inherit node-process;

  membench-node-this-1     = mkMembench 1 { node = input.self;    rev = node-self-rev; };
  membench-node-this-5     = mkMembench 5 { node = input.self;    rev = node-self-rev; };
  membench-node-measured-1 = mkMembench 1 { node = node-measured; rev = input.node-measured.rev; };
  membench-node-measured-5 = mkMembench 5 { node = node-measured; rev = input.node-measured.rev; };
}
