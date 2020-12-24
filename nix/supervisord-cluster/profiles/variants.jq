## Profile variants are defined as a cartesian product of
## variations of genesis/generator/node axes.

def genesis_profile_variants:
  [ { genesis:     { utxo: 1000000, delegators:  125000 } }
  , { genesis:     { utxo: 1000000, delegators:  125000 }
    , composition: { dense_pool_density: 2 } }
  , { genesis:     { utxo: 1000000, delegators:  125000 }
    , composition: { dense_pool_density: 4 } }
  , { genesis:     { utxo: 1000000, delegators:  125000 }
    , composition: { dense_pool_density: 8 } }
  , { genesis:     { utxo: 1000000, delegators:  125000 }
    , composition: { dense_pool_density: 20 } }
  , { genesis:     { utxo: 1000000, delegators:  125000 }
    , composition: { dense_pool_density: 40 } }
  , { genesis:     { utxo: 2000000, delegators:  125000 } }
  , { genesis:     { utxo: 4000000, delegators:  125000 } }
  , { genesis:     { utxo: 1000000, delegators:  250000 } }
  , { genesis:     { utxo: 1000000, delegators:  500000 } }
  , { genesis:     { utxo: 1000000, delegators: 1000000 } }
  , { genesis:     { utxo: 2000000, delegators: 2000000 } }
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
