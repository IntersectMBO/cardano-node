def all_profile_variants:
                                         1024    as $Ki
  |                                      1000000 as $M
  ####################################################################################################
  ##
  ### Historic record
  ##
  | { genesis:
      { utxo:                               (4 * $M)
      , delegators:                         (1 * $M)
      }
    } as $dataset_oct2021
  |
    ({} |
     .genesis.max_block_size              = (72 * $Ki)
    ) as $blocksize_dec2021
  |
    { genesis:
      ({} |
       .max_block_size                    = (72 * $Ki) |       ## ???
       .alonzo.maxTxExUnits.exUnitsMem    = (12.5 * $M) )      ## RMT-54  CR.059
    } as $params_jan2022
  |
    { genesis:
      ({}|
       .max_block_size                    = (80 * $Ki) |       ## RMT-56  CAD-3891  CR.061
       .alonzo.maxTxExUnits.exUnitsMem    = (14 * $M) |        ## RMT-56
       .alonzo.maxBlockExUnits.exUnitsMem = (56 * $M))         ## CAD-3945
    } as $params_feb2022
  |
    { genesis:
      { utxo:                               (6 * $M)
      , delegators:                         (1.3 * $M)
      }
    } as $dataset_mar2022
  |
    { genesis:
      { max_block_size:                     (88 * $Ki) }       ## CAD-4153  CR.068
    } as $blocksize_may2022
  |
    { genesis:
      { utxo:                               (8 * $M)
      , delegators:                         (1.3 * $M)
      }
    } as $dataset_jun2022
  |
    { genesis:
      ({}|
       .alonzo.maxBlockExUnits.exUnitsMem = (62 * $M))         ## CAD-3991  CR.064
    } as $plutus_next
  |
  ####################################################################################################
  ##
  ### Definition vocabulary:  dataset size
  ##
    $dataset_jun2022
      as $current_dataset
  | ({}|
     .generator.tps                   = 15
    ) as $current_tps_saturation_value
  |
    ({}|
     .generator.tps                   = 0.2
    ) as $current_tps_saturation_plutus
  |
    ({}|
     .genesis.max_block_size = $params_feb2022.genesis.max_block_size
    ) as $current_block_size
  |
    ({}|
     .genesis.alonzo = $params_feb2022.genesis.alonzo
    ) as $current_plutus
  |
    ($current_block_size *
     $current_plutus *
     { genesis:
       { utxo:                              0
       , delegators:                        0
       }
     }) as $dataset_empty
  |
    ($current_block_size *
     $current_plutus *
     { genesis:
       { utxo:                              (0.5 * $M)
       , delegators:                        (0.1 * $M)
       }
     , generator:
       { tps:                               1
       , tx_count:                          10
       }
     }) as $dataset_miniature
  |
    ($current_block_size *
     $current_plutus *
     $current_dataset
    ) as $dataset_status_quo
  |
    { genesis:
      { utxo:                               (30 * $M)
      , delegators:                         0
      , max_block_size:                     (1 * $M)
      }
    , generator:
      { tps:                                (1 * $M / (360 * 20))
      }
    } as $dataset_dish
  |
  ##
  ### Definition vocabulary:  protocol parameters
  ##
    ({} |
     .genesis.alonzo.costModels =
      { PlutusV1:
        {
        }
      , PlutusV2:
        {
        }
      }
    ) as $pparams_v7_to_v8
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
   ($dataset_oct2021 *
    { chaindb:
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
  |
  ##
  ### Definition vocabulary:  workload
  ##
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
    }) as $plutus_loop_counter
  |
   ($pparams_v7_to_v8 *
    { generator:
      { plutus:
          { type:                      "LimitSaturationLoop"
          , script:                    "v2/ecdsa-secp256k1-loop.plutus"
          , redeemer:
            { constructor: 0
            , fields:
              [ ## Termination (but, de-facto arbitrary).
                { int:   1000000 }
              , ## Signer's verification key.
                { bytes: "c990a0510345c7ac576d5e612afd41a7da65c228dde536db0d6c680350134bcdf23988c04e666cf5d478c3ba8dc81a4af9466dbf441c07906f77e16efd02828d" }
              , ## Message to check signature of.
                { bytes: "315f5bdb76d078c43b8ac0064e4a0164612b1fce77c869345bfc94c75894edd3"}
              , ## Signature.
                { bytes: "ed651a9436ae022a3ac25be76a0caf22c2cc5b346f65df7a7f76b396556320df875ebc95693a8d1ec53f95161d008299621af3fdd5ead19bf8e578f64753103d" }
              ]
            }
          }
      }
    }) as $plutus_loop_secp_ecdsa
  |
   ($pparams_v7_to_v8 *
    { generator:
      { plutus:
          { type:                       "LimitSaturationLoop"
          , script:                     "v2/schnorr-secp256k1-loop.plutus"
          , redeemer:
            { constructor: 0
            , fields:
              [ ## Termination (but, de-facto arbitrary).
                { int:   1000000 }
              , ## Signer's verification key.
                { bytes: "5e516dff32f8dd97e00a3b1eba5e494f6e762704a84c6dacf1d76c3fb85fb46b888794a3e9f98104fe93fb4d91f685de7a6fba97ca8053dce28f84cf550bce6e" }
              , ## Message to check signature of.
                { bytes: "315f5bdb76d078c43b8ac0064e4a0164612b1fce77c869345bfc94c75894edd3" }
              , ## Signature.
                { bytes: "070135e16157c952dd84da758639805051ba8a80d13487997023ea3bf6ba55f31947373a8519b1bb133270831170de7ec109de5c4ec71328323fbefdd6366f2c" }
              ]
            }
          }
      }
    }) as $plutus_loop_secp_schnorr
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
   ($scenario_fixed_loaded * $doublet * $dataset_oct2021 *
    { node:
      { shutdown_on_slot_synced:        2400
      }
    , desc: "Oct 2021 dataset size, honest four epochs."
    }) as $forge_stress_pre_base
  |
   ($scenario_fixed_loaded * $doublet * $dataset_status_quo *
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
  [ $dataset_status_quo *
    ({}|
     .genesis.slot_duration           = 0.2 )

  ## Dense pool:
  , $dataset_status_quo *
    ({}|
     .genesis.dense_pool_density      = 10 )

  ## Sub-saturation TPS:
  , ($dataset_status_quo | .generator.tps     = 5 )
  , ($dataset_status_quo | .generator.tps     = 10 )

  ## Block size:
  , ($dataset_status_quo | .genesis.max_block_size =  128000 | .generator.tps = 16 )
  , ($dataset_status_quo | .genesis.max_block_size =  256000 | .generator.tps = 32 )
  , ($dataset_status_quo | .genesis.max_block_size =  512000 | .generator.tps = 64 )
  , ($dataset_status_quo | .genesis.max_block_size = 1024000 | .generator.tps = 128 )
  , ($dataset_status_quo | .genesis.max_block_size = 2048000 | .generator.tps = 256 )

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
