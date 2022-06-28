def adhoc_profiles:
[ { name: "devops"
  , scenario:               "idle"
  , genesis:
    { slot_duration:         0.2
    , parameter_k:           10
    , epoch_length:          1000
    , active_slots_coeff:    0.1
    , genesis_future_offset: "10 seconds"
    , utxo:                  0

    , shelley:
      { updateQuorum:        1
      }
    }
  , analysis:
    { type:                  null
    }
  }
];
