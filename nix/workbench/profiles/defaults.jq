## Testable with:
##
##   jq -n 'include "defaults" { search: "nix/workbench/profiles" }; era_defaults("babbage")'
##
def era_defaults($era):
{ common:
  { era:                              $era

  ## Choice of a cluster run scenario (wb scenario --help):
  , scenario:                         "fixed-loaded"

  ## Cluster topology and composition:
  , composition:
    { locations:                      ["LO"]
    , n_bft_hosts:                    0
    , n_singular_hosts:               5
    , n_dense_hosts:                  1
    , dense_pool_density:             1
    , with_proxy:                     false
    , with_observer:                  false
    , topology:                       "uni-circle"
    }

  , genesis:
    ## Trivia
    { network_magic:                  42

    ## Incrementality
    , single_shot:                    true

    ## UTxO & delegation
    , per_pool_balance:               1000000000000000
    , funds_balance:                  10000000000000
    , utxo:                           0
    , decentralisation_param:         0

    ## Blockchain time & block density
    , active_slots_coeff:             0.05
    , epoch_length:                   600   # Ought to be at least (10 * k / f).
    , parameter_k:                    3
    , slot_duration:                  1

    ## Block size & contents
    , max_block_size:                 80000
    , max_tx_size:                    16384

    ## Verbatim overlay, for all era-specific genesis slices:
    , shelley:
      { protocolParams:
        { poolDeposit:                500000000
        , keyDeposit:                 400000
        , rho:                        0.0022
        , tau:                        0.05
        , a0:                         0.3
        , minFeeA:                    0
        , minFeeB:                    0
        , decentralisationParam:      0
        , nOpt:                       50
        }
      }
    }

  , generator:
    { add_tx_size:                    100
    , init_cooldown:                  5
    , inputs_per_tx:                  2
    , outputs_per_tx:                 2
    , tx_fee:                         1000000
    , epochs:                         3
    , tps:                            12
    }

  , node:
    { rts_flags_override:             []
    , shutdown_on_slot_synced:        300
    , shutdown_on_block_synced:       null
    , tracing_backend:                "trace-dispatcher"  ## or "iohk-monitoring"
    , tracer:                         true
    , verbatim:
      { EnableP2P:                       false

      , MempoolCapacityBytesOverride:    "NoOverride"
      , ProtocolIdleTimeout:             5
      , TimeWaitTimeout:                 60
      , AcceptedConnectionsLimit:
        { hardLimit:                     512
        , softLimit:                     384
        , delay:                         5
        }
      , TargetNumberOfRootPeers:         100
      , TargetNumberOfKnownPeers:        100
      , TargetNumberOfEstablishedPeers:  50
      , TargetNumberOfActivePeers:       20
      }
    }

  , analysis:
    { type:                           "standard"
    , cluster_startup_overhead_s:     10
    , start_log_spread_s:             120
    , last_log_spread_s:              120
    , silence_since_last_block_s:     120
    , tx_loss_ratio:                  0.02
    , finish_patience:                21
    , filters:                        ["base", "size-full"]
    }
  }

, shelley:
  { analysis:
    { maximum_missed_slots:           0
    }
  }

, allegra:
  {
  }

, mary:
  {
  }

, alonzo:
  ({} |
    .genesis.shelley.protocolParams.protocolVersion =
      { major: 5
      , minor: 0
      })

, babbage:
  ({} |
    .genesis.shelley.protocolParams.protocolVersion =
      { major: 5
      , minor: 0
      })
} | (.common * .[$era]);
