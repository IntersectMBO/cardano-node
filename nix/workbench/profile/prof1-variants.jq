import "epoch-timeline" as timeline;

## For the Nomad perf-ssd cluster, we might want to artificially
## cap the large RAM resources the instances provide.
def nomad_memory_limit($limit):
  { nomad:
    { resources:
      { producer:
        { memory:       $limit
        , memory_max:   $limit
        }
      }
    }
  };

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
    } as $dataset_miniature
  |
    { genesis:
      { utxo:                               (1   * $M)
      , delegators:                         (0.2 * $M)
      }
    } as $dataset_small
  |
    { genesis:
      { utxo:                               (24 * $M)
      , delegators:                         (1.2 * $M)
      }
    } as $dataset_24m
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
  ##
  ### Definition vocabulary:  dreps
  ##
  | { genesis:
      { dreps: 10
      }
    } as $dreps_tiny
  | { genesis:
      { dreps: 1000
      }
    } as $dreps_small
  | { genesis:
      { dreps: 2000
      }
    } as $dreps_medium
  | { genesis:
      { dreps: 10000
      }
    } as $dreps_large
  ##
  ### Definition vocabulary:  chain
  ##
  |
    { chaindb:
      { mainnet_chunks:
        { chaindb_server:               10
        , explorer:                     0
        }
      , ledger_snapshot:
        { chaindb_server:               237599
        , explorer:                     0
        }
      }
    , node:
      { shutdown_on_slot_synced:
        { explorer:                     237599
        }
      }
    } as $chaindb_early_byron
  |
   ({ chaindb:
      { mainnet_chunks:
        { chaindb_server:               1800
        , explorer:                     1799
        }
      , ledger_snapshot:
        { chaindb_server:               38901589
        , explorer:                     37173650
        }
      }
    , node:
      { shutdown_on_slot_synced:
        { explorer:                     38901589
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
    } as $solo
  |
    { composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  1
      , dense_pool_density:             10
      }
    } as $solo_dense10
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
      { n_singular_hosts:               4
      , n_dense_hosts:                  0
      }
    } as $quadruplet
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
      { n_singular_hosts:               52
      , n_dense_hosts:                  0
      }
    } as $compose_fiftytwo
  |
    { composition:
      { topology:                       "torus"
      }
    } as $torus
  |
    { composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  0
      , with_chaindb_server:            true
      , with_explorer:                  true
      }
    } as $chainsync_cluster
  |
  ##
  ### Definition vocabulary:  cluster
  ##
    # P&T exclusive Nomad cluster Nodes: Compute intensive (16 GB, no-SSD)
    { cluster:
      { nomad:
        { namespace: "perf"
        , class: "perf"
        , resources:
          {
            # Producer nodes use this specs, make sure they are available!
            # WARNING: Don't use more than roughly 15400, for example 15432,
            # because some clients show a couple bytes less available.
            producer: {cores:  8, memory: 15400, memory_max:  16000}
            # The explorer node uses this specs, make sure they are available!
          , explorer: {cores: 16, memory: 32000, memory_max:  64000}
          }
        , fetch_logs_ssh: true
        }
      , aws:
        { instance_type:
          { producer: "c5.2xlarge"
          , explorer: "m5.4xlarge"
          }
        , use_public_routing: true
        }
      # We are requiring 10.5GB on the explorer node and 9GB on the others.
      , minimun_storage:
        { 
          # 9 GB for the explorer node, that includes the tx-generator.
          # Plus giving 3 GB for the Nix Store and 1.5GB of margin.
          producer: 12582912 # 12×1024×1024
          # 7.5 GB for the nodes without the tx-generator.
          # Plus giving 3 GB for the Nix Store and 1.5GB of margin.
        , explorer: 14155776 # 13.5×1024×1024
        }
      , keep_running: true
      }
    } as $nomad_perf
  |
    # P&T exclusive Nomad cluster Nodes: Disk intensive (fast SSDs)
    { cluster:
      { nomad:
        { namespace: "perf-ssd"
        , class: "perf-ssd"
        , resources:
          { producer: {cores: 16, memory: 120000, memory_max: 124000}
          , explorer: {cores: 16, memory: 120000, memory_max: 124000}
          }
        , host_volumes: [
            {source: "ssd1", destination: "/ssd1", read_only: false}
          , {source: "ssd2", destination: "/ssd2", read_only: false}
          ]
        , fetch_logs_ssh: true
        }
      , aws:
        { instance_type:
          { producer: "r5d.4xlarge"
          , explorer: "r5d.4xlarge"
          }
        , use_public_routing: true
        }
      , minimun_storage: null
      , keep_running: true
      }
    } as $nomad_perfssd
  |
    ($nomad_perf *
      { composition:
        { locations:                      ["eu-central-1", "us-east-1", "ap-southeast-2"]
        , topology:                       "torus"
        , with_explorer:                  true
        }
      }
    ) as $nomad_perf_torus
  |
    ($nomad_perf *
      { composition:
        { locations:                      ["eu-central-1", "us-east-1", "ap-southeast-2"]
        , topology:                       "torus-dense"
        , with_explorer:                  true
        }
      }
    ) as $nomad_perf_dense
  |
    ($nomad_perfssd *
      { composition:
        { locations:                      ["eu-central-1"]
        , topology:                       "uni-circle"
        , with_explorer:                  false
        }
      }
    ) as $nomad_perfssd_unicircle
  |
    ($nomad_perfssd *
      { composition:
        { locations:                      ["eu-central-1", "us-east-1", "ap-southeast-2"]
        , topology:                       "torus-dense"
        , with_explorer:                  true
        }
      }
    ) as $nomad_perfssd_dense
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
      { epoch_length:                   1200
      , parameter_k:                    6
      }
    } as $small_timescale
  |
    { genesis:
      { epoch_length:                   8000
      , parameter_k:                    40
      }
    } as $model_timescale
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
     .generator.epochs                = 7
    ) as $for_7ep
  |
    ({} |
     .generator.epochs                = 8
    ) as $for_8ep
  |
    ({} |
     .generator.epochs                = 9
    ) as $for_9ep
  |
    ({} |
     .generator.epochs                = 15
    ) as $for_15ep
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
     | .node.shutdown_on_block_synced   = 36
    ) as $for_36blk
  |
    ({}
     | .node.shutdown_on_slot_synced    = 900
    ) as $for_900slot
  |
    ({}
     | .node.shutdown_on_slot_synced    = 1200
    ) as $for_1200slot
  ##
  ### Definition vocabulary:  workload
  ##
  | ({}|
     .generator.tps                   = 15
    ) as $current_tps_saturation_value
  | ({}|
     .generator.tps                   = 12
    ) as $nomad_perf_tps_saturation_value
  | ({}|
     .generator.tps                   = 9
    ) as $model_tps_saturation_value
  |
    ({}|
     .generator.tps                   = 0.2
    ) as $current_tps_saturation_plutus
  |
    ({}
     | .generator.tps                 = 0.4
    ) as $double_tps_saturation_plutus
  |
    ({}
     | .generator.tps                 = 0.48
    ) as $double_plus_tps_saturation_plutus
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
          , script:                     "Loop"
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
          { type:                      "LimitTxPerBlock_8"
          , script:                    "EcdsaSecp256k1Loop"
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
    ) as $plutus_loop_secp_ecdsa
  |
   ({ generator:
      { plutus:
          { type:                       "LimitTxPerBlock_8"
          , script:                     "SchnorrSecp256k1Loop"
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
    ) as $plutus_loop_secp_schnorr
  |
   ({ generator:
      { plutus:
          { type:                       "LimitTxPerBlock_8"
          , script:                     "HashOntoG2AndAdd"
          , redeemer:
            { "constructor": 0
            , "fields": [
                { "int": 1000000 }
              ## ByteString content is arbitrary
              , { "list": [
                    { "bytes": "714805c6" }
                  , { "bytes": "c413111e" }
                  , { "bytes": "2d7eb870" }
                  , { "bytes": "4ecbd6a1" }
                  ]
                }
              ]
            }
          }
      }
    }
    | .generator.tx_fee        = 539076
    ) as $plutus_loop_blst    
  ##
  ### Definition vocabulary:  genesis variants
  ##
  |
    ({}
      | .genesis.pparamsEpoch         = timeline::lastKnownEpoch
      | .genesis.pparamsOverlays      = ["v8-preview"]
    ) as $costmodel_v8_preview
  |
    ({}
      | .genesis.pparamsEpoch         = timeline::lastKnownEpoch
      | .genesis.pparamsOverlays      = ["v8-preview", "stepshalf"]
    ) as $costmodel_v8_preview_stepshalf
  |
    ({}
      | .genesis.pparamsEpoch         = timeline::lastKnownEpoch
      | .genesis.pparamsOverlays      = ["v8-preview", "doublebudget"]
    ) as $costmodel_v8_preview_doubleb
  |
    ({}
      | .genesis.pparamsEpoch         = timeline::lastKnownEpoch
      | .genesis.pparamsOverlays      = ["v8-preview", "v9-preview"]
    ) as $costmodel_v9_preview 
  ##
  ### Definition vocabulary:  node + tracer config variants
  ##
  |
    ({ extra_desc:                     "without cardano-tracer"
     , suffix:                         "notrc"
     }|
     .node.tracer                     = false
    ) as $without_tracer
  |
    ({ extra_desc:                     "with RTView"
     , suffix:                         "rtvw"
     }|
     .tracer.rtview                   = true
    ) as $with_rtview
  |
    ({ extra_desc:                     "with resource tracing in cardano-tracer"
     }|
     .tracer.withresources            = true
    ) as $with_resources
  |
    ({ extra_desc:                     "with legacy iohk-monitoring"
     , suffix:                         "iomf"
     }|
     .node.tracing_backend            = "iohk-monitoring"
    ) as $old_tracing
  |
    ({ extra_desc:                     "with P2P networking"
     , suffix:                         "p2p"
     }|
     .node.verbatim.EnableP2P         = true
    ) as $p2p
  |
  ##
  ### Definition vocabulary:  RTS config variants, overriding the default
  ##
    ({ extra_desc:                     "RTSflags A4m"
     , suffix:                         "rtsA4m"
     }|
     .node.rts_flags_override         = ["-A4m"]
    ) as $rts_A4m
  |
    ({ extra_desc:                     "RTSflags A64m"
     , suffix:                         "rtsA64m"
     }|
     .node.rts_flags_override         = ["-A64m"]
    ) as $rts_A64m
  |
    ({ extra_desc:                     "RTSflags N3"
     , suffix:                         "rtsN3"
     }|
     .node.rts_flags_override         = ["-N3"]
    ) as $rts_N3
  |
    ({ extra_desc:                     "RTSflags A4m N3"
     , suffix:                         "rtsA4mN3"
     }|
     .node.rts_flags_override         = ["-A4m", "-N3"]
    ) as $rts_A4mN3
  |
    ({ extra_desc:                     "RTSflags A64m N3"
     , suffix:                         "rtsA64mN3"
     }|
     .node.rts_flags_override         = ["-A64m", "-N3"]
    ) as $rts_A64mN3
  |
    ({ extra_desc:                     "RTSflags nonmoving GC"
     , suffix:                         "rtsxn"
     }|
     .node.rts_flags_override         = ["-xn"]
    ) as $rts_xn
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
   ($model_timescale * $nomad_perf_tps_saturation_value *
    { scenario:                        "fixed-loaded"
    }) as $scenario_nomad_perf
  |
   ($model_timescale * $nomad_perf_tps_saturation_value *
    { scenario:                        "fixed-loaded"
    }) as $scenario_nomad_perfssd
  |
   ($small_timescale * $nomad_perf_tps_saturation_value *
    { scenario:                        "fixed-loaded"
    }) as $scenario_nomad_perfssd_solo
  |
   ($model_timescale * $model_tps_saturation_value *
    { scenario:                        "fixed-loaded"
    }) as $scenario_model
  |
   ({ scenario:                        "idle"
    }) as $scenario_idle
  |
   ({ scenario:                        "tracer-only"
    }) as $scenario_tracer_only
  |
   ({ scenario:                        "latency"
    }) as $scenario_latency
  |
  ##
  ### Definition vocabulary:  base variant
  ##
   ($scenario_fixed_loaded * $doublet * $dataset_empty * $for_1blk * $no_filtering *
    { desc: "Stop as soon as we've seen a single block"
    }) as $fast_base
  |
   ($scenario_fixed_loaded * $doublet * $dataset_empty * $for_3blk * $no_filtering *
    { desc: "Miniature dataset, CI-friendly duration, test scale"
    }) as $citest_base
  |
   ($scenario_fixed_loaded * $doublet * $dataset_miniature * $for_15blk * $no_filtering *
    { desc: "Miniature dataset, CI-friendly duration, bench scale"
    }) as $cibench_base
  |
   ($scenario_fixed_loaded * $hexagon * $torus * $dataset_empty * $for_15blk * $no_filtering * $with_resources *
    { desc: "6 low-footprint nodes in a torus topology, 5 minutes runtime"
    }) as $tracebench_base
  |
   ($scenario_fixed_loaded * $hexagon * $torus * $dataset_empty * $for_1200slot * $no_filtering * $with_resources *
    { desc: "6 low-footprint nodes in a torus topology, 20 minutes runtime"
    }) as $tracefull_base
  |
   ($scenario_fixed_loaded * $doublet * $dataset_empty * $for_900slot * $no_filtering *
    { desc: "2 low-footprint nodes, 15 minutes runtime"
    }) as $ep_trans_base
  |
   ($scenario_fixed_loaded * $dataset_small * $for_15ep *
    { node:
        { shutdown_on_slot_synced:        9000
        }
      , analysis:
        { filters:                        ["epoch3+", "size-moderate"]
        }
      , desc: "Small dataset, honest 15 epochs duration"
    }) as $plutuscall_base
  |
   ($scenario_nomad_perf * $compose_fiftytwo * $dataset_oct2021 * $for_8ep *
    { node:
        { shutdown_on_slot_synced:        64000
        }
      , analysis:
        { filters:                        ["epoch3+", "size-full"]
        }
      , generator:
        { init_cooldown:                  45
        }
      , genesis:
        { funds_balance:                  20000000000000
        , max_block_size:                 88000
        }
      , desc: "AWS c5-2xlarge cluster dataset, 7 epochs"
    }) as $nomad_perf_base
  |
   ($scenario_nomad_perfssd * $compose_fiftytwo * $dataset_oct2021 * $for_8ep *
    { node:
        { shutdown_on_slot_synced:        64000
        }
      , analysis:
        { filters:                        ["epoch3+", "size-full"]
        }
      , generator:
        { init_cooldown:                  45
        }
      , genesis:
        { funds_balance:                  20000000000000
        , max_block_size:                 88000
        }
      , desc: "AWS c5-2xlarge cluster dataset, 7 epochs"
    }) as $nomad_perfssd_base
  |
   ($scenario_nomad_perf * $compose_fiftytwo * $dataset_oct2021 * $for_9ep * $plutus_base * $plutus_loop_counter *
    { node:
        { shutdown_on_slot_synced:        72000
        }
      , analysis:
        { filters:                        ["epoch3+", "size-small"]
        }
      , generator:
        { init_cooldown:                  45
        , tps:                            0.85
        }
      , genesis:
        { funds_balance:                  20000000000000
        , max_block_size:                 88000
        }
      , desc: "AWS c5-2xlarge cluster dataset, 9 epochs"
    }) as $nomad_perf_plutus_base
  |
   ($scenario_latency * $compose_fiftytwo * $dataset_empty * $no_filtering *
    { desc: "AWS perf class cluster, stop when all latency services stop"
    }) as $nomad_perf_latency_base
  |
   ($scenario_latency * $compose_fiftytwo * $dataset_empty * $no_filtering *
    { desc: "AWS perf-ssd class cluster, stop when all latency services stop"
    }) as $nomad_perfssd_latency_base
  |
   ($scenario_nomad_perfssd_solo * $solo * $dataset_24m *
    { node:
        { shutdown_on_slot_synced:        7200
        }
      , analysis:
        { filters:                        ["epoch3+", "size-full"]
        }
      , generator:
        { epochs:                         6
        }
      , genesis:
        { funds_balance:                  20000000000000
        , max_block_size:                 88000
        }
      , desc: "AWS c5[d]-9xlarge utxoscale dataset, 6 epochs"
    }) as $nomad_perfssd_solo_base
  |
   ($scenario_model * $quadruplet * $dataset_current * $for_7ep *
    { node:
        { shutdown_on_slot_synced:        56000
        }
      , analysis:
        { filters:                        ["epoch3+", "size-full"]
        }
      , generator:
        { init_cooldown:                  45
        }
      , genesis:
        { funds_balance:                  20000000000000
        }
      , desc: "Status-quo dataset, 7 epochs"
    }) as $model_base
  |
   ($model_base * $plutus_base *
    { analysis:
        { filters:                        ["epoch3+", "size-moderate"]
        }
    }) as $modelplutus_base
  |
   ($scenario_fixed_loaded * $triplet * $dataset_oct2021 *
    { node:
      { shutdown_on_slot_synced:        2400
      }
    , desc: "Oct 2021 dataset size, four epochs."
    }) as $forge_stress_pre_base
  |
   ($forge_stress_pre_base * $hexagon *
    { analysis:
      { filters:                        ["epoch3+"] }
    , node:
      { shutdown_on_slot_synced:        4800 }
    , desc: "Status-quo dataset size, eight epochs, six nodes."
    }) as $forge_stress_pre_large_base
  |
   ($scenario_fixed_loaded * $triplet * $dataset_current *
    { node:
      { shutdown_on_slot_synced:        2400
      }
    , desc: "Status-quo dataset size, four epochs."
    }) as $forge_stress_base
  |
   ($scenario_fixed_loaded * $triplet * $dataset_current *
    { node:
      { shutdown_on_slot_synced:        1200
      }
    , desc: "Status-quo dataset size, two epochs."
    }) as $forge_stress_short_base
  |
   ($scenario_fixed_loaded * $triplet * $dataset_oct2021 *
    { node:
      { shutdown_on_slot_synced:        1200
      }
    , desc: "Oct 2021 dataset size, two epochs."
    }) as $forge_stress_light_base
  |
   ($forge_stress_base * $hexagon *
    { analysis:
      { filters:                        ["epoch3+"] }
    , node:
      { shutdown_on_slot_synced:        4800 }
    , desc: "Status-quo dataset size, eight epochs, six nodes."
    }) as $forge_stress_large_base
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

  ### Profile templates
  ###
  # UTxO scaling on a single node, mainnet blocksize, ~2h runtime (6 epochs) - default: 24mio UTxO, 64GB RAM cap
    ($nomad_perfssd_solo_base * $nomad_perfssd_unicircle * $costmodel_v8_preview * $p2p
    ) as $utxoscale_solo_template
  |
  # P&T Nomad cluster: 52 nodes, P2P by default - value-only workload
    ($nomad_perf_base * $nomad_perf_dense * $p2p * $costmodel_v8_preview
    ) as $value_nomadperf_template
  |
  # P&T Nomad cluster: 52 nodes, P2P by default - Plutus workload
    ($nomad_perf_plutus_base * $nomad_perf_dense * $p2p * $costmodel_v8_preview
    ) as $plutus_nomadperf_template
  |

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
    , desc: "Default, as per nix/workbench/profile/prof0-defaults.jq"
    }
  , $p2p *
    { name: "default-p2p"
    , desc: "Default, as per nix/workbench/profile/prof0-defaults.jq with P2P enabled"
    }
  , $nomad_perf_torus * $p2p *
    { name: "default-nomadperf"
    , desc: "Default on P&T exclusive cluster"
    }
  , $nomad_perf_torus *
    { name: "default-nomadperf-nop2p"
    , desc: "Default on P&T exclusive cluster with P2P disabled"
    }
  , $plutus_base * $costmodel_v8_preview * $plutus_loop_counter *
    { name: "plutus"
    , desc: "Default with Plutus workload: CPU/memory limit saturation counter loop"
    }
  , $plutus_base * $costmodel_v8_preview * $plutus_loop_secp_ecdsa *
    { name: "plutus-secp-ecdsa"
    , desc: "Default with Plutus workload: CPU/memory limit saturation ECDSA SECP256k1 loop"
    }
  , $plutus_base * $costmodel_v8_preview * $plutus_loop_secp_schnorr *
    { name: "plutus-secp-schnorr"
    , desc: "Default with Plutus workload: CPU/memory limit saturation Schnorr SECP256k1 loop"
    }
  , $old_tracing *
    { name: "oldtracing"
    , desc: "Default in legacy tracing mode"
    }
  , $nomad_perf_torus * $p2p * $old_tracing *
    { name: "oldtracing-nomadperf"
    , desc: "Default in legacy tracing mode on P&T exclusive cluster"
    }
  , $nomad_perf_torus * $old_tracing *
    { name: "oldtracing-nomadperf-nop2p"
    , desc: "Default in legacy tracing mode on P&T exclusive cluster with P2P disabled"
    }
  , $scenario_idle *
    { name: "idle"
    , desc: "Idle scenario:  start nodes & detach from tty;  no cluster termination"
    }
  , $scenario_tracer_only *
    { name: "tracer-only"
    , desc: "Idle scenario:  start only the tracer & detach from tty;  no termination"
    }

  ## Fastest profile to pass analysis: just 1 block
  , $fast_base *
    { name: "fast"
    }
  , $fast_base * $p2p *
    { name: "fast-p2p"
    }
  , $fast_base * $plutus_base * $plutus_loop_counter *
    { name: "fast-plutus"
    }
  , $fast_base * $without_tracer *
    { name: "fast-notracer"
    }
  , $fast_base * $old_tracing *
    { name: "fast-oldtracing"
    }

  ## Fast variants: single node with large, varying dataset sizes
  , $fast_base * $solo * $dataset_24m *
    { name: "faststartup-24M"
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
  , $citest_base * $with_rtview *
    { name: "ci-test-rtview"
    }
  , $citest_base * $nomad_perf_torus * $p2p *
    { name: "ci-test-nomadperf"
    , desc: "ci-test on P&T exclusive cluster"
    }
  , $citest_base * $nomad_perf_torus * $old_tracing *
    { name: "ci-test-oldtracing-nomadperf"
    , desc: "ci-test in legacy tracing mode on P&T exclusive cluster"
    }
  , $citest_base * $nomad_perf_torus *
    { name: "ci-test-nomadperf-nop2p"
    , desc: "ci-test on P&T exclusive cluster with P2P disabled"
    }

  ## CI variants: bench duration, 15 blocks
  , $cibench_base *
    { name: "ci-bench"
    }
  , $cibench_base * $p2p *
    { name: "ci-bench-p2p"
    }
  , $cibench_base * $plutus_base * $costmodel_v8_preview * $plutus_loop_counter *
    { name: "ci-bench-plutus"
    }
  , $cibench_base * $plutus_base * $costmodel_v8_preview * $plutus_loop_secp_ecdsa *
    { name: "ci-bench-plutus-secp-ecdsa"
    }
  , $cibench_base * $plutus_base * $costmodel_v8_preview * $plutus_loop_secp_schnorr *
    { name: "ci-bench-plutus-secp-schnorr"
    }
  , $cibench_base * $plutus_base * $double_plus_tps_saturation_plutus * $costmodel_v9_preview * $plutus_loop_blst *
    { name: "ci-bench-plutusv3-blst"
    , genesis:
      { max_block_size: 88000 }
    , analysis:
      { filters:        ["size-moderate-2"] }
    }
  , $cibench_base * $without_tracer *
    { name: "ci-bench-notracer"
    }
  , $cibench_base * $with_rtview *
    { name: "ci-bench-rtview"
    }
  , $cibench_base * $dreps_tiny *
    { name: "ci-bench-drep"
    }
  , $cibench_base * $p2p *
    { name: "ci-bench-lmdb"
    , node:    { utxo_lmdb: true }
    , cluster: { ssd_directory: "/tmp" }
    }
  , $cibench_base * $nomad_perf_torus * $p2p *
    { name: "ci-bench-nomadperf"
    , desc: "ci-bench on P&T exclusive cluster"
    }
  , $cibench_base * $nomad_perf_torus * $old_tracing *
    { name: "ci-bench-oldtracing-nomadperf"
    , desc: "ci-bench in legacy tracing mode on P&T exclusive cluster"
    }
  , $cibench_base * $nomad_perf_torus *
    { name: "ci-bench-nomadperf-nop2p"
    , desc: "ci-bench on P&T exclusive cluster with P2P disabled"
    }

  ## CI variants: test duration, 3 blocks, dense10
  , $citest_base * $solo_dense10 *
    { name: "ci-test-dense10"
    }

  ## CI variants: bench duration, 15 blocks
  , $tracebench_base *
    { name: "trace-bench"
    }
  , $tracebench_base * $old_tracing *
    { name: "trace-bench-oldtracing"
    }
  , $tracebench_base * $without_tracer *
    { name: "trace-bench-notracer"
    }
  , $tracebench_base * $with_rtview *
    { name: "trace-bench-rtview"
    }

  ## Full variants: 120 blocks
  , $tracefull_base *
    { name: "trace-full"
    }
  , $tracefull_base * $with_rtview *
    { name: "trace-full-rtview"
    }

  ## Epoch transition test: 1.5 epochs, 15mins runtime
  , $ep_trans_base *
    { name: "epoch-transition"
    }

  ## Plutus call variants: 15 epochs, with differences in block budget execution step limit
  , $plutus_base * $costmodel_v8_preview * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_counter *
    { name: "plutuscall-loop-plain"
    }
  , $plutus_base * $costmodel_v8_preview * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_secp_ecdsa *
    { name: "plutuscall-secp-ecdsa-plain"
    }
  , $plutus_base * $costmodel_v8_preview * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_secp_schnorr *
    { name: "plutuscall-secp-schnorr-plain"
    }
  , $plutus_base * $costmodel_v8_preview_stepshalf * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_counter *
    { name: "plutuscall-loop-half"
    }
  , $plutus_base * $costmodel_v8_preview_stepshalf * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_secp_ecdsa *
    { name: "plutuscall-secp-ecdsa-half"
    }
  , $plutus_base * $costmodel_v8_preview_stepshalf * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_secp_schnorr *
    { name: "plutuscall-secp-schnorr-half"
    }
  , $plutus_base * $costmodel_v8_preview_doubleb * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_counter *
    { name: "plutuscall-loop-double"
    }
  , $plutus_base * $costmodel_v8_preview_doubleb * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_secp_ecdsa *
    { name: "plutuscall-secp-ecdsa-double"
    }
  , $plutus_base * $costmodel_v8_preview_doubleb * $plutuscall_base * $double_tps_saturation_plutus * $plutus_loop_secp_schnorr *
    { name: "plutuscall-secp-schnorr-double"
    }

## P&T Nomad cluster: 52 nodes, 3 regions, value-only (incl. old tracing variant) and Plutus, P2P enabled by default
  , $value_nomadperf_template *
    { name: "value-nomadperf"
    }
  , $nomad_perfssd_base * $nomad_perfssd_dense * $p2p * $costmodel_v8_preview *
    { name: "value-nomadperfssd"
    }
  , $value_nomadperf_template * $old_tracing *
    { name: "value-oldtracing-nomadperf"
    }
  , $plutus_nomadperf_template *
    { name: "plutus-nomadperf"
    }
  , $nomad_perf_latency_base * $nomad_perf_dense * $p2p * $costmodel_v8_preview *
    { name: "latency-nomadperf"
    }
  , $nomad_perfssd_latency_base * $nomad_perfssd_dense * $p2p * $costmodel_v8_preview *
    { name: "latency-nomadperfssd"
    }

## P&T Nomad cluster: 52 nodes, value-only and Plutus workloads - DRep injection variants
  , $value_nomadperf_template * $dreps_small *
    { name: "value-drep1k-nomadperf"
    }
  , $value_nomadperf_template * $dreps_medium *
    { name: "value-drep2k-nomadperf"
    }
  , $value_nomadperf_template * $dreps_large *
    { name: "value-drep10k-nomadperf"
    }
  , $plutus_nomadperf_template * $dreps_small *
    { name: "plutus-drep1k-nomadperf"
    }
  , $plutus_nomadperf_template * $dreps_medium *
    { name: "plutus-drep2k-nomadperf"
    }
  , $plutus_nomadperf_template * $dreps_large *
    { name: "plutus-drep10k-nomadperf"
    }

## P&T Nomad cluster: 52 nodes, 3 regions, value-only (with old tracing variant) and Plutus, no P2P flavour
  , $nomad_perf_base * $nomad_perf_dense * $costmodel_v8_preview *
    { name: "value-nomadperf-nop2p"
    }
  , $nomad_perf_base * $nomad_perf_dense * $costmodel_v8_preview * $old_tracing *
    { name: "value-oldtracing-nomadperf-nop2p"
    }
  , $nomad_perf_plutus_base * $nomad_perf_dense * $costmodel_v8_preview *
    { name: "plutus-nomadperf-nop2p"
    }

## P&T Nomad cluster: 52 nodes, 3 regions, fast, P2P flavour
  , $fast_base * $compose_fiftytwo * $nomad_perf_dense * $costmodel_v8_preview * $p2p *
    { name: "fast-nomadperf"
    }
  , $fast_base * $compose_fiftytwo * $nomad_perf_dense * $costmodel_v8_preview *
    { name: "fast-nomadperf-nop2p"
    }
  , $fast_base * $compose_fiftytwo * $nomad_perfssd_dense * $costmodel_v8_preview * $p2p *
    { name: "fast-nomadperfssd"
    }

## P&T NomadSSD cluster: UTxO scale benchmarks on a single node
  , $utxoscale_solo_template *
    { name: "utxoscale-solo-24M64G-nomadperfssd"
    }
  , $utxoscale_solo_template *
    { name: "utxoscale-solo-12M64G-nomadperfssd"
    , genesis:
      { utxo:                               (12 * $M)
      }
    }
  , $utxoscale_solo_template *
    { name: "utxoscale-solo-12M16G-nomadperfssd"
    , genesis:
      { utxo:                               (12 * $M)
      }
    , node:
      { heap_limit:                         16384
      }
    }

## Model value variant: 7 epochs (128GB RAM needed; 16GB for testing locally)
  , $model_base * $costmodel_v8_preview *
    { name: "model-value"
    }
  , $model_base * $costmodel_v8_preview * $dataset_small *
    { name: "model-value-test"
    }

## Model plutus variants: 7 epochs, with differences in block budget execution step limit (128GB RAM needed)
  , $modelplutus_base * $costmodel_v8_preview * $double_tps_saturation_plutus * $plutus_loop_secp_ecdsa *
    { name: "model-secp-ecdsa-plain"
    }
  , $modelplutus_base * $costmodel_v8_preview_stepshalf * $double_tps_saturation_plutus * $plutus_loop_secp_ecdsa *
    { name: "model-secp-ecdsa-half"
    }
  , $modelplutus_base * $costmodel_v8_preview_doubleb * $double_tps_saturation_plutus * $plutus_loop_secp_ecdsa *
    { name: "model-secp-ecdsa-double"
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

  ## Status-quo (huge) dataset, 6 nodes, 8 epochs.
  , $forge_stress_large_base *
    { name: "forge-stress-large"
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

  # single forger node, larger blocksize, various flavours
  , $forge_stress_base * $solo * $costmodel_v8_preview *
    { name: "forge-stress-solo"
    , extra_desc: "with blocksize bumped to 88k"
    }
  , $forge_stress_base * $plutus_base * $plutus_loop_counter * $solo * $costmodel_v8_preview *
    { name: "forge-stress-plutus-solo"
    , extra_desc: "with blocksize bumped to 88k"
    }
  , $forge_stress_short_base * $solo * $costmodel_v8_preview *
    { name: "forge-stress-solo-xs"
    , extra_desc: "with blocksize bumped to 88k"
    }
  , $forge_stress_pre_base * $solo * $costmodel_v8_preview *
    { name: "forge-stress-pre-solo"
    , extra_desc: "with blocksize bumped to 88k"
    }
  , $forge_stress_pre_large_base * $solo * $costmodel_v8_preview *
    { name: "forge-stress-pre-solo-xl"
    , extra_desc: "with blocksize bumped to 88k"
    }
  , $forge_stress_light_base * $solo * $costmodel_v8_preview *
    { name: "forge-stress-pre-solo-xs"
    , extra_desc: "with blocksize bumped to 88k"
    }

  ## Large dataset, small cluster (3 nodes), variants for RTS parametrization
  , $forge_stress_pre_base * $rts_A4m *
    { name: "forge-stress-pre-rtsA4m"
    }
  , $forge_stress_pre_base * $rts_A64m *
    { name: "forge-stress-pre-rtsA64m"
    }
  , $forge_stress_pre_base * $rts_N3 *
    { name: "forge-stress-pre-rtsN3"
    }
  , $forge_stress_pre_base * $rts_A4mN3 *
    { name: "forge-stress-pre-rtsA4mN3"
    }
  , $forge_stress_pre_base * $rts_A64mN3 *
    { name: "forge-stress-pre-rtsA64mN3"
    }
  , $forge_stress_pre_base * $rts_xn *
    { name: "forge-stress-pre-rtsxn"
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
      , extra_future_offset:   10
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
