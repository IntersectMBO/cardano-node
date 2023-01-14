{ lib, runCommand, jq
, db-analyser
, chain, shelleyGenesisHash
, input, node-snapshot
, snapshotSlot, finalChunkNo
}:

let
  secondLastChunkNo = finalChunkNo - 1; # todo, second last chunk, not epoch
  chain' = chain.override { upToChunk = finalChunkNo; };
  partialChain = runCommand "partial-chain-${toString finalChunkNo}" {} ''
    mkdir -p $out/immutable
    cd $out
    ln -s ${chain'}/protocolMagicId protocolMagicId
    for epoch in {00000..${toString secondLastChunkNo}}; do
      ln -s ${chain'}/immutable/''${epoch}.{chunk,primary,secondary} immutable
    done
    cp ${chain'}/immutable/0${toString finalChunkNo}.{chunk,primary,secondary} immutable

    ## Existence of 'clean' is necessary to avoid ImmutableDB revalidation:
    touch clean
  '';
  filter = name: type: let
    baseName = baseNameOf (toString name);
    sansPrefix = lib.removePrefix (toString node-snapshot) name;
  in
  builtins.trace sansPrefix (
    sansPrefix == "/configuration" ||
    (lib.hasPrefix "/configuration/cardano" sansPrefix));
in runCommand "snapshot-generation" {
  buildInputs = [ jq db-analyser ];
  inherit finalChunkNo snapshotSlot;
  requiredSystemFeatures = [ "benchmark" ];
  meta.timeout = 16 * 3600; # 16 hours
  genesisFiles = lib.cleanSourceWith { inherit filter; src = builtins.unsafeDiscardStringContext node-snapshot; name = "genesis-files"; };
} ''
  cp -r ${partialChain} chain
  chmod +w -R chain

  cp $genesisFiles/configuration/cardano/*-genesis.json .

  ## Normally db-analyser is silent, and so that would make Hydra builds time out.
  ## This prevents the timeout.
  { while true; do sleep 3600; echo not silent; done } &

  ## Actually produce the snapshot:
  args=( --configByron     mainnet-byron-genesis.json
         --configShelley   mainnet-shelley-genesis.json
         --nonce           ${shelleyGenesisHash}
         --configAlonzo    mainnet-alonzo-genesis.json
         --store-ledger    ${toString snapshotSlot}
       )
  db-analyser --db chain/ cardano "''${args[@]}"

  ls -ltrh chain/ledger

  mv chain/ledger/${toString snapshotSlot}_db-analyser temp
  rm chain/ledger/*
  mv temp chain/ledger/${toString snapshotSlot}

  mkdir $out
  mv chain $out/

  args=( --argjson snapshotSlot                 ${toString snapshotSlot}
         --arg     snapshottingConsensusNodeRev ${input.node-snapshot.rev}
         --arg     shelleyGenesisHash           ${shelleyGenesisHash}
         --arg     finalChunkNo                 ${toString finalChunkNo}
       )
  jq '{ snapshotSlot:                 $snapshotSlot
      , finalChunkNo:                 $finalChunkNo
      , snapshottingConsensusNodeRev: $snapshottingConsensusNodeRev
      , shelleyGenesisHash:           $shelleyGenesisHash
      }
     ' "''${args[@]}" > $out/snapshot-info.json
''
