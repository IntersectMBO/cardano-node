self: final: prev: with self.legacyPackages.${final.system}; {
  inherit cardano-node cardano-cli cardano-ping bech32 db-converter commonLib
    cardano-node-profiled cardano-node-eventlogged cardano-node-asserted;
  inherit (iohkNix) cardanoLib;
}
