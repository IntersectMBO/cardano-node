def adhoc_profiles:
[ { name: "default"
  , desc: "Default profile, as per nix/workbench/profiles/defaults.jq"
  }

, { name: "short"
  , generator: { tx_count: 10000, inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100 }
  , genesis: { genesis_future_offset: "3 minutes" }
  }
, { name: "small"
  , generator: { tx_count: 1000,  inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100
               , init_cooldown: 25 }
  , analysis: { finish_patience: 4 }
  , genesis: { genesis_future_offset: "3 minutes" }
  }
# , { name: "smoke"
#   , generator: { tx_count: 100,   add_tx_size: 0, inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100
#                , init_cooldown: 25 }
#   , analysis: { finish_patience: 4 }
#   , genesis: { genesis_future_offset: "3 minutes", delegators: 4 }
#   }
, { name: "smoke-plutus"
  , generator: { tx_count: 100,   add_tx_size: 0, inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100
               , init_cooldown: 25
	       , plutusMode: true
	       , plutusAutoMode: true
	       , debugMode: false }
  , analysis: { finish_patience: 4 }
  , genesis: { genesis_future_offset: "3 minutes" }
  }
, { name: "10"
  , composition:
    { n_singular_hosts:               10
    , n_dense_hosts:                  0
    }
  , genesis:
    { genesis_future_offset: "10 seconds"
    , utxo:                  0
    }
  }

, { name: "devops"
  , genesis:
    { slot_duration:         0.2
    , parameter_k:           10
    , epoch_length:          1000
    , active_slots_coeff:    0.1
    , genesis_future_offset: "10 seconds"
    , utxo:                  0

    , shelley:
      { updateQuorum: 1
      }
    }
  }
];
