## Profile variants are defined as a cartesian product of
## variations of genesis/generator/node axes.

def genesis_profile_variants:

  ## Baseline:
  [ { genesis: { utxo: 4000000, delegators: 1000000 } }

  ## Baseline, tweaked for fast local repro:
  , { genesis: { utxo: 4000000, delegators: 1000000
               , slot_duration: 0.2 }
    , composition: { with_observer: false }}

  ## Size-varied derivatives of baseline:
  , { genesis: { utxo: 4000000, delegators: 1000000 } }
  , { genesis: { utxo: 5000000, delegators: 1250000 } }
  , { genesis: { utxo: 6000000, delegators: 1500000 } }
  , { genesis: { utxo: 4000000, delegators: 1000000, dense_pool_density:  2 } }
  , { genesis: { utxo: 4000000, delegators: 1000000, dense_pool_density:  3 } }
  , { genesis: { utxo: 4000000, delegators: 1000000, dense_pool_density: 10 } }

  ## TPS-varied derivatives of baseline:
  , { genesis: { utxo: 4000000, delegators: 1000000 }
    , generator: { tps: 5 } }
  , { genesis: { utxo: 4000000, delegators: 1000000 }
    , generator: { tps: 10 } }

  ## Calibration:
  , { genesis: { utxo: 2000000, delegators: 1000000, max_block_size:  128000 }
    , generator: { tps:  16 } }
  , { genesis: { utxo: 2000000, delegators: 1000000, max_block_size:  256000 }
    , generator: { tps:  32 } }
  , { genesis: { utxo: 2000000, delegators:  500000, max_block_size:  512000 }
    , generator: { tps:  64 } }
  , { genesis: { utxo: 2000000, delegators:  500000, max_block_size: 1024000 }
    , generator: { tps: 128 } }
  , { genesis: { utxo: 2000000, delegators:  500000, max_block_size: 2048000 }
    , generator: { tps: 256 } }

  ## Fixed
  , { name: "fixed"
    , scenario: "fixed"
    , genesis: { utxo: 4000000, delegators: 1000000 }
    , node:
      { shutdown_on_slot_synced: 150
      }
    }
  , { name: "smoke"
    , scenario: "fixed-loaded"
    , node:
      { shutdown_on_slot_synced: 50
      }
    , generator: { tps: 10 }
    }

  ## Chainsync:
  , { name: "chainsync"
    , scenario: "chainsync"
    , preset: "mainnet"
    , composition:
      { locations:                      ["LO"]
      , n_bft_hosts:                    0
      , n_singular_hosts:               0
      , n_dense_hosts:                  0
      , with_proxy:                     true
      , with_observer:                  true
      } }
  ];

def generator_profile_variants:
  [ { generator: {} }
  ];

def node_profile_variants:
  [ { node: {} }
  ];

def       all_profile_variants:
  [   genesis_profile_variants
  , generator_profile_variants
  ,      node_profile_variants
  ]
  | [combinations]
  | map (reduce .[] as $item ({}; . * $item));
