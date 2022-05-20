## Profile variants are defined as a cartesian product of
## variations of genesis/generator/node axes.

def all_profile_variants:
                                         1024    as $Ki
  |                                      1000000 as $M
  ####################################################################################################
  ##
  ### Record history
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
  ### Status quo
  ##
    $dataset_jun2022
      as $current_dataset
  |
    ({}|
     .genesis.max_block_size = $params_feb2022.genesis.max_block_size
    ) as $current_block_size
  |
    ({}|
     .genesis.alonzo = $params_feb2022.genesis.alonzo
    ) as $current_plutus
  |
    ($current_dataset *
     $current_block_size *
     $current_plutus
    ) as $status_quo
  |
  ####################################################################################################
  ##
  ### Definition vocabulary
  ##
    ({}|
     .generator.tps                   = 15
    ) as $saturation_tps_value
  |
    ({}|
     .generator.tps                   = 0.2
    ) as $saturation_tps_plutus
  |
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
      { n_singular_hosts:               10
      , n_dense_hosts:                  0
      }
    } as $tenner
  |
    ({}|
     .node.tracer                     = true
    ) as $with_tracer
  |
    ({}|
     .node.tracing_backend           = "iohk-monitoring"
    ) as $old_tracing
  |
    { genesis:
      { epoch_length:                   600
      , parameter_k:                    3
      }
    } as $compressed
  |
   ($compressed * $saturation_tps_value *
    { scenario:                        "fixed-loaded"
    , analysis:
      { type:                          "standard" }
    }) as $fixed_loaded
  |
   ($fixed_loaded * $saturation_tps_plutus *
    { generator:
      { inputs_per_tx:                  1
      , outputs_per_tx:                 1
      , plutusMode:                     true
      , plutusAutoMode:                 true
      }
    , analysis:
      { type:                          "standard"
      , filters:                        ["base", "size-small"]
      }
    }) as $plutus
  |
   ($fixed_loaded * $doublet *
    { genesis:
      { utxo:                           6000
      , delegators:                     1300
      , max_block_size:                 80000
      }
    , node:
      { shutdown_on_slot_synced:        10
      }
    }) as $startstop_base
  |
   ($status_quo * $fixed_loaded * $doublet *
    { node:
      { shutdown_on_slot_synced:        2400
      }
    }) as $forge_stress_base
  |
    { scenario:                        "chainsync"
    , preset:                          "mainnet"
    , composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  0
      , with_chaindb_server:            true
      , with_observer:                  true
      }
    , analysis:
      { type:                          "performance"
      , filters:                        []
      }
    } as $chainsync_base
  |
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
  ####################################################################################################
  ##
  ### Actual profiles
  ##

  ## Baseline:
  [ { name: "default"
    , desc: "Default profile, as per nix/workbench/profiles/defaults.jq"
    }

  ## Short slots:
  , $status_quo *
    ({}|
     .genesis.slot_duration           = 0.2 )

  ## Dense pool:
  , $status_quo *
    ({}|
     .genesis.dense_pool_density      = 10 )

  ## Sub-saturation TPS:
  , ($status_quo | .generator.tps     = 5 )
  , ($status_quo | .generator.tps     = 10 )

  ## Block size:
  , ($status_quo | .genesis.max_block_size =  128000 | .generator.tps = 16 )
  , ($status_quo | .genesis.max_block_size =  256000 | .generator.tps = 32 )
  , ($status_quo | .genesis.max_block_size =  512000 | .generator.tps = 64 )
  , ($status_quo | .genesis.max_block_size = 1024000 | .generator.tps = 128 )
  , ($status_quo | .genesis.max_block_size = 2048000 | .generator.tps = 256 )

  ## Fixed
  , $startstop_base *
    { name: "startstop"
    }
  , $startstop_base * $with_tracer *
    { name: "startstop-tracer"
    }
  , $startstop_base * $old_tracing *
    { name: "startstop-oldtracing"
    }

  , $fixed_loaded * $saturation_tps_value *
    { name: "smoke"
    , node:
      { shutdown_on_slot_synced:        80
      }
    }

  , $fixed_loaded * $tenner *
    { name: "10"
    }
  , $fixed_loaded * $tenner * $with_tracer *
    { name: "10-tracer"
    }

  , $plutus *
    { name: "plutus"
    , generator:
      { tx_count:                       800
      }
    }

  , $forge_stress_base *
    { name: "forge-stress"
    }
  , $forge_stress_base * $plutus * $singleton *
    { name: "forge-stress-plutus"
    , generator:
      { tx_count:                       800
      }
    }
  , $forge_stress_base * $old_tracing * $with_tracer *
    { name: "forge-stress-tracer"
    }
  , $forge_stress_base * $old_tracing *
    { name: "forge-stress-oldtracing"
    }

  , $chainsync_base * $chaindb_early_byron *
    { name: "chainsync-early-byron"
    }
  , $chainsync_base * $chaindb_early_byron * $with_tracer *
    { name: "chainsync-early-byron-tracer"
    }
  , $chainsync_base * $chaindb_early_byron * $old_tracing *
    { name: "chainsync-early-byron-oldtracing"
    }

  , $chainsync_base * $chaindb_early_alonzo *
    { name: "chainsync-early-alonzo"
    }
  , $chainsync_base * $chaindb_early_alonzo * $old_tracing *
    { name: "chainsync-early-alonzo-oldtracing"
    }
  ];
