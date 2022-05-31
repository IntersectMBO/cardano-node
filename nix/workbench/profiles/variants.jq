## Profile variants are defined as a cartesian product of
## variations of genesis/generator/node axes.

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
     $current_dataset
    ) as $dataset_status_quo
  |
    ($current_block_size *
     $current_plutus *
     { genesis:
       { utxo:                              (0.5 * $M)
       , delegators:                        (0.1 * $M)
       }
     }) as $dataset_miniature
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
     .node.shutdown_on_block_synced   = 1
    ) as $for_1blk
  |
    ({} |
     .node.shutdown_on_block_synced   = 3
    ) as $for_3blk
  |
    ({} |
     .node.shutdown_on_block_synced   = 15
    ) as $for_15blk
  |
    ({} |
     .node.shutdown_on_block_synced   = 30
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
      , plutusMode:                     true
      , plutusAutoMode:                 true
      }
    , analysis:
      { filters:                        ["base", "size-small"]
      }
    }) as $plutus
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
   ($mainnet_timescale * $chainsync_cluster *
    { desc: "Mainnet chain syncing benchmark"
    , scenario:                        "chainsync"
    , preset:                          "mainnet"
    , analysis:
      { type:                          "performance"
      , filters:                        []
      }
    }) as $scenario_chainsync
  |
   ($compressed_timescale * $current_tps_saturation_value *
    { scenario:                        "fixed-loaded"
    }) as $scenario_fixed_loaded
  |
  ##
  ### Definition vocabulary:  base variant
  ##
   ($scenario_fixed_loaded * $doublet * $dataset_miniature * $for_1blk *
    { desc: "Stop as soon as we've seen a single block"
    }) as $startstop_base
  |
   ($scenario_fixed_loaded * $doublet * $dataset_miniature * $for_3blk *
    { desc: "Miniature dataset, CI-friendly duration, test scale"
    }) as $citest_base
  |
   ($scenario_fixed_loaded * $doublet * $dataset_miniature * $for_15blk *
    { desc: "Miniature dataset, CI-friendly duration, bench scale"
    }) as $cibench_base
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
  , $plutus *
    { name: "plutus"
    , desc: "Default with Plutus workload"
    }

  ## Fastest -- start-stop
  , $startstop_base *
    { name: "startstop"
    }
  , $startstop_base * $p2p *
    { name: "startstop-p2p"
    }
  , $startstop_base * $plutus *
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
  , $citest_base * $plutus *
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
  , $cibench_base * $plutus *
    { name: "ci-bench-plutus"
    }
  , $cibench_base * $without_tracer *
    { name: "ci-bench-notracer"
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
  , $dish_base * $plutus *
    { name: "dish-plutus"
    }
  , $dish_base * $plutus *
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
  , $cibench_base * $tenner * $plutus *
    { name: "10-plutus"
    }
  , $cibench_base * $tenner * $without_tracer *
    { name: "10-notracer"
    }

  ## Status-quo (huge) dataset, small cluster (2 nodes)
  , $forge_stress_base *
    { name: "forge-stress"
    }
  , $forge_stress_base * $plutus *
    { name: "forge-stress-p2p"
    }
  , $forge_stress_base * $plutus *
    { name: "forge-stress-plutus"
    }
  , $forge_stress_base * $plutus * $singleton *
    { name: "forge-stress-plutus-singleton"
    }
  , $forge_stress_base * $without_tracer *
    { name: "forge-stress-notracer"
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
  ];
