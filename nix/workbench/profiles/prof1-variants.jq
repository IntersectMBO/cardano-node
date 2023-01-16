import "epoch-timeline" as timeline;

def all_profile_variants:
                                         1024    as $Ki
  |                                      1000000 as $M
  ####################################################################################################
  ##
  ### Definition vocabulary:  dataset size
  ##
  |
    { genesis:
      { utxo:                               (10 * $M)
      , delegators:                         (1.3 * $M)
      }
    } as $dataset_jan2023
  |
    $dataset_jan2023
      as $dataset_current
  |
    { genesis:
      { utxo:                               (8 * $M)
      , delegators:                         (1.2 * $M)
      }
    } as $dataset_jun2022
  |
    { genesis:
      { utxo:                               (6 * $M)
      , delegators:                         (1.3 * $M)
      }
    } as $dataset_mar2022
  |
    { genesis:
      { utxo:                               (4 * $M)
      , delegators:                         (1 * $M)
      }
    } as $dataset_oct2021
  |
    { genesis:
      { utxo:                              0
      , delegators:                        0
      }
    } as $dataset_empty
  |
    { genesis:
      { utxo:                              (0.5 * $M)
      , delegators:                        (0.1 * $M)
      }
    , generator:
      { tps:                               1
      , tx_count:                          10
      }
    } as $dataset_miniature
  |
    { genesis:
      { utxo:                               (1   * $M)
      , delegators:                         (0.2 * $M)
      }
    } as $dataset_small
  |
    { genesis:
      { utxo:                               (30 * $M)
      , delegators:                         0
      , shelley:
        { protocolParams:
          { maxBlockBodySize:               (1 * $M)
          }
        }
      }
    , generator:
      { tps:                                (1 * $M / (360 * 20))
      }
    } as $dataset_dish
  |
  ##
  ### Definition vocabulary:  chain
  ##
    { chaindb:
      { mainnet_chunks:
        { chaindb_server:               10
        , observer:                     0
        }
      , ledger_snapshot:
        { chaindb_server:               237599
        , observer:                     0
        }
      }
    , node:
      { shutdown_on_slot_synced:
        { observer:                     237599
        }
      }
    } as $chaindb_early_byron
  |
   ({ chaindb:
      { mainnet_chunks:
        { chaindb_server:               1800
        , observer:                     1799
        }
      , ledger_snapshot:
        { chaindb_server:               38901589
        , observer:                     37173650
        }
      }
    , node:
      { shutdown_on_slot_synced:
        { observer:                     38901589
        }
      }
    }) as $chaindb_early_alonzo
  |
  ##
  ### Definition vocabulary:  cluster size
  ##
    { composition:
      { n_singular_hosts:               1
      , n_dense_hosts:                  0
      }
    } as $singleton
  |
    { composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  1
      , dense_pool_density:             10
      }
    } as $singleton_dense10
  |
    { composition:
      { n_singular_hosts:               2
      , n_dense_hosts:                  0
      }
    } as $doublet
  |
    { composition:
      { n_singular_hosts:               3
      , n_dense_hosts:                  0
      }
    } as $triplet
  |
    { composition:
      { n_singular_hosts:               6
      , n_dense_hosts:                  0
      }
    } as $hexagon
  |
    { composition:
      { n_singular_hosts:               10
      , n_dense_hosts:                  0
      }
    } as $tenner
  |
    { composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  0
      , with_chaindb_server:            true
      , with_observer:                  true
      }
    } as $chainsync_cluster
  |
  ##
  ### Definition vocabulary:  filtering
  ##
    ({}
     | .analysis.filters                = []
     | .analysis.filter_exprs           = []
    ) as $no_filtering
  |
  ##
  ### Definition vocabulary:  timescale
  ##
    { genesis:
      { epoch_length:                   600
      , parameter_k:                    3
      }
    } as $compressed_timescale
  |
    { genesis:
      { epoch_length:                   (3600 * 24 * 5)
      , parameter_k:                    (18   * 24 * 5)
      }
    } as $mainnet_timescale
  |
  ##
  ### Definition vocabulary:  duration
  ##
    ({} |
     .generator.epochs                = 3
    ) as $for_3ep
  |
    ({} |
     .generator.epochs                = 4
    ) as $for_4ep
  |
    ({} |
     .generator.epochs                = 12
    ) as $for_12ep
  |
    ({}
     | .node.shutdown_on_block_synced   = 1
    ) as $for_1blk
  |
    ({}
     | .node.shutdown_on_block_synced   = 3
    ) as $for_3blk
  |
    ({}
     | .node.shutdown_on_block_synced   = 15
    ) as $for_15blk
  |
    ({}
     | .node.shutdown_on_block_synced   = 30
    ) as $for_30blk
  ##
  ### Definition vocabulary:  workload
  ##
  | ({}|
     .generator.tps                   = 15
    ) as $current_tps_saturation_value
  |
    ({}|
     .generator.tps                   = 0.2
    ) as $current_tps_saturation_plutus
  |
    ({}
     | .generator.tps                 = 0.4
    ) as $double_tps_saturation_plutus
  |
   ($current_tps_saturation_plutus *
    { extra_desc: "with Plutus workload"
    , generator:
      { inputs_per_tx:                  1
      , outputs_per_tx:                 1
      }
    , analysis:
      { filters:                        ["size-small"]
      }
    }) as $plutus_base
  |
   ({ generator:
      { plutus:
          { type:                       "LimitSaturationLoop"
          , script:                     "v1/loop.plutus"
          , redeemer:
            { "int": 1000000 }
          }
      }
    }
    | .generator.tx_fee        = 1360000
    ) as $plutus_loop_counter
  |
   ({ generator:
      { plutus:
          { type:                      "LimitSaturationLoop"
          , script:                    "v2/ecdsa-secp256k1-loop.plutus"
          , redeemer:
            { constructor: 0
            , fields:
              [ ## Termination (but, de-facto arbitrary).
                { int:   1000000 }
              , ## Signer's verification key.
                { bytes: "0392d7b94bc6a11c335a043ee1ff326b6eacee6230d3685861cd62bce350a172e0" }
              , ## Message to check signature of.
                { bytes: "16e0bf1f85594a11e75030981c0b670370b3ad83a43f49ae58a2fd6f6513cde9" }
              , ## Signature.
                { bytes: "5fb12954b28be6456feb080cfb8467b6f5677f62eb9ad231de7a575f4b6857512754fb5ef7e0e60e270832e7bb0e2f0dc271012fa9c46c02504aa0e798be6295" }
              ]
            }
          }
      }
    }
    | .generator.tx_fee        = 1025000
    | .genesis.pparamsEpoch    = timeline::lastKnownEpoch
    | .genesis.pparamsOverlays = ["v8-preview"]
    ) as $plutus_loop_secp_ecdsa
  |
   ({ generator:
      { plutus:
          { type:                       "LimitSaturationLoop"
          , script:                     "v2/schnorr-secp256k1-loop.plutus"
          , redeemer:
            { constructor: 0
            , fields:
              [ ## Termination (but, de-facto arbitrary).
                { int:   1000000 }
              , ## Signer's verification key.
                { bytes: "599de3e582e2a3779208a210dfeae8f330b9af00a47a7fb22e9bb8ef596f301b" }
              , ## Message to check signature of.
                { bytes: "30303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030" }
              , ## Signature.
                { bytes: "5a56da88e6fd8419181dec4d3dd6997bab953d2fc71ab65e23cfc9e7e3d1a310613454a60f6703819a39fdac2a410a094442afd1fc083354443e8d8bb4461a9b" }
              ]
            }
          }
      }
    }
    | .generator.tx_fee        = 1020000
    | .genesis.pparamsEpoch    = timeline::lastKnownEpoch
    | .genesis.pparamsOverlays = ["v8-preview"]
    ) as $plutus_loop_secp_schnorr
  |
  ##
  ### Definition vocabulary:  node config variants
  ##
    ({ extra_desc:                     "without cardano-tracer"
     , suffix:                         "notracer"
     }|
     .node.tracer                     = false
    ) as $without_tracer
  |
    ({ extra_desc:                     "with legacy iohk-monitoring"
     , suffix:                         "oldtracing"
     }|
     .node.tracing_backend           = "iohk-monitoring"
    ) as $old_tracing
  |
    ({ extra_desc:                     "with P2P networking"
     , suffix:                         "p2p"
     }|
     .node.verbatim.EnableP2P         = true
    ) as $p2p
  |
  ##
  ### Definition vocabulary:  scenario
  ##
   ($mainnet_timescale * $chainsync_cluster * $no_filtering * $without_tracer *
    { desc: "Mainnet chain syncing benchmark"
    , scenario:                        "chainsync"
    , preset:                          "mainnet"
    , analysis:
      { type:                          "performance"
      }
    }) as $scenario_chainsync
  |
   ($compressed_timescale * $current_tps_saturation_value *
    { scenario:                        "fixed-loaded"
    }) as $scenario_fixed_loaded
  |
   ({ scenario:                        "idle"
    }) as $scenario_idle
  |
   ({ scenario:                        "tracer-only"
    }) as $scenario_tracer_only
  |
  ##
  ### Definition vocabulary:  base variant
  ##
   ($scenario_fixed_loaded * $doublet * $dataset_empty * $for_1blk * $no_filtering *
    { desc: "Stop as soon as we've seen a single block"
    }) as $startstop_base
  |
   ($scenario_fixed_loaded * $doublet * $dataset_empty * $for_3blk * $no_filtering *
    { desc: "Miniature dataset, CI-friendly duration, test scale"
    }) as $citest_base
  |
   ($scenario_fixed_loaded * $doublet * $dataset_miniature * $for_15blk * $no_filtering *
    { desc: "Miniature dataset, CI-friendly duration, bench scale"
    }) as $cibench_base
  |
   ($scenario_fixed_loaded * $dataset_small * $for_12ep *
    { node:
      { shutdown_on_slot_synced:        7200
      }
      , desc: "Small dataset, honest 12 epochs duration"
    }
    | .genesis.pparamsEpoch    = timeline::lastKnownEpoch
    | .genesis.pparamsOverlays = ["v8-preview"]
    ) as $plutuscall_base
  |
   ($scenario_fixed_loaded * $triplet * $dataset_oct2021 *
    { node:
      { shutdown_on_slot_synced:        2400
      }
    , desc: "Oct 2021 dataset size, honest four epochs."
    }) as $forge_stress_pre_base
  |
   ($scenario_fixed_loaded * $triplet * $dataset_current *
    { node:
      { shutdown_on_slot_synced:        2400
      }
    , desc: "Status-quo dataset size, honest four epochs."
    }) as $forge_stress_base
  |
   ($scenario_fixed_loaded * $triplet * $dataset_dish *
    { node:
      { shutdown_on_slot_synced:        2400
      }
    , desc: "Dish dataset & setup"
    }) as $dish_base
  |
  ####################################################################################################
  ##
  ### Actual profiles
  ##

  ### First, auto-named profiles:
  ###
  ## Short slots:
  [ $dataset_current * $triplet *
    ({}|
     .genesis.slot_duration           = 0.2 )

  ## Dense pool:
  , $dataset_current * $triplet *
    ({}|
     .genesis.dense_pool_density      = 10 )

  ## Sub-saturation TPS:
  , $dataset_current * $triplet *
    ({}|
     .generator.tps = 5 )

  , $dataset_current * $triplet *
    ({}|
     .generator.tps = 10 )

  ### Next, semantically-named profiles:
  ###
  ## Base variants:
  , { name: "default"
    , desc: "Default, as per nix/workbench/profiles/defaults.jq"
    }
  , $plutus_base * $plutus_loop_counter *
    { name: "plutus"
    , desc: "Default with Plutus workload: CPU/memory limit saturation counter loop"
    }
  , $plutus_base * $plutus_loop_secp_ecdsa *
    { name: "plutus-secp-ecdsa"
    , desc: "Default with Plutus workload: CPU/memory limit saturation ECDSA SECP256k1 loop"
    }
  , $plutus_base * $plutus_loop_secp_schnorr *
    { name: "plutus-secp-schnorr"
    , desc: "Default with Plutus workload: CPU/memory limit saturation Schnorr SECP256k1 loop"
    }
  , $old_tracing *
    { name: "oldtracing"
    , desc: "Default in legacy tracing mode"
    }
  , $scenario_idle *
    { name: "idle"
    , desc: "Idle scenario:  start nodes & detach from tty;  no cluster termination"
    }
  , $scenario_tracer_only *
    { name: "tracer-only"
    , desc: "Idle scenario:  start only the tracer & detach from tty;  no termination"
    }

  ## Fastest -- start-stop
  , $startstop_base *
    { name: "startstop"
    }
  , $startstop_base * $p2p *
    { name: "startstop-p2p"
    }
  , $startstop_base * $plutus_base * $plutus_loop_counter *
    { name: "startstop-plutus"
    }
  , $startstop_base * $without_tracer *
    { name: "startstop-notracer"
    }
  , $startstop_base * $old_tracing *
    { name: "startstop-oldtracing"
    }

  ## CI variants: test duration, 3 blocks
  , $citest_base *
    { name: "ci-test"
    }
  , $citest_base * $p2p *
    { name: "ci-test-p2p"
    }
  , $citest_base * $plutus_base * $plutus_loop_counter *
    { name: "ci-test-plutus"
    }
  , $citest_base * $without_tracer *
    { name: "ci-test-notracer"
    }

  ## CI variants: bench duration, 15 blocks
  , $cibench_base *
    { name: "ci-bench"
    }
  , $cibench_base * $p2p *
    { name: "ci-bench-p2p"
    }
  , $cibench_base * $plutus_base * $plutus_loop_counter *
    { name: "ci-bench-plutus"
    }
  , $cibench_base * $plutus_base * $plutus_loop_secp_ecdsa *
    { name: "ci-bench-plutus-secp-ecdsa"
    }
  , $cibench_base * $plutus_base * $plutus_loop_secp_schnorr *
    { name: "ci-bench-plutus-secp-schnorr"
    }
  , $cibench_base * $without_tracer *
    { name: "ci-bench-notracer"
    }

  ## CI variants: test duration, 3 blocks, dense10
  , $citest_base * $singleton_dense10 *
    { name: "ci-test-dense10"
    }

  ## Plutus call variants: 12 epochs
  , $plutuscall_base * $plutus_base * $double_tps_saturation_plutus * $plutus_loop_counter *
    { name: "plutuscall-loop"
    }
  , $plutuscall_base * $plutus_base * $double_tps_saturation_plutus * $plutus_loop_secp_ecdsa *
    { name: "plutuscall-secp-ecdsa"
    }
  , $plutuscall_base * $plutus_base * $double_tps_saturation_plutus * $plutus_loop_secp_schnorr *
    { name: "plutuscall-secp-schnorr"
    }

## Dish variants
  , $dish_base *
    { name: "dish"
    }
  , $dish_base *
    { name: "dish-10M"
    , genesis:
      { utxo:                               (10 * $M)
      }
    }
  , $dish_base * $plutus_base * $plutus_loop_counter *
    { name: "dish-plutus"
    }
  , $dish_base * $plutus_base * $plutus_loop_counter *
    { name: "dish-10M-plutus"
    , genesis:
      { utxo:                               (10 * $M)
      }
    }

  ## Large local cluster -- 10 nodes
  , $cibench_base * $tenner *
    { name: "10"
    }
  , $cibench_base * $tenner * $p2p *
    { name: "10-p2p"
    }
  , $cibench_base * $tenner * $plutus_base * $plutus_loop_counter *
    { name: "10-plutus"
    }
  , $cibench_base * $tenner * $without_tracer *
    { name: "10-notracer"
    }

  ## Status-quo (huge) dataset, small cluster (2 nodes)
  , $forge_stress_base *
    { name: "forge-stress"
    }
  , $forge_stress_base * $plutus_base * $plutus_loop_counter *
    { name: "forge-stress-p2p"
    }
  , $forge_stress_base * $plutus_base * $plutus_loop_counter *
    { name: "forge-stress-plutus"
    }
  , $forge_stress_base * $plutus_base * $plutus_loop_counter * $singleton *
    { name: "forge-stress-plutus-singleton"
    }
  , $forge_stress_base * $without_tracer *
    { name: "forge-stress-notracer"
    }

  , $forge_stress_pre_base *
    { name: "forge-stress-pre"
    }
  , $forge_stress_pre_base * $plutus_base * $plutus_loop_counter *
    { name: "forge-stress-pre-plutus"
    }
  , $forge_stress_pre_base * $without_tracer *
    { name: "forge-stress-pre-notracer"
    }

  , $scenario_chainsync * $chaindb_early_byron *
    { name: "chainsync-early-byron"
    }
  , $scenario_chainsync * $chaindb_early_byron * $without_tracer *
    { name: "chainsync-early-byron-notracer"
    }
  , $scenario_chainsync * $chaindb_early_byron * $old_tracing *
    { name: "chainsync-early-byron-oldtracing"
    }

  , $scenario_chainsync * $chaindb_early_alonzo *
    { name: "chainsync-early-alonzo"
    }
  , $scenario_chainsync * $chaindb_early_alonzo * $without_tracer *
    { name: "chainsync-early-alonzo-notracer"
    }
  , $scenario_chainsync * $chaindb_early_alonzo * $old_tracing *
    { name: "chainsync-early-alonzo-oldtracing"
    }
  , $scenario_chainsync * $chaindb_early_alonzo * $p2p *
    { name: "chainsync-early-alonzo-p2p"
    }

  ## Last, but not least, the profile used by "nix-shell -A devops":
  , { name: "devops"
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
