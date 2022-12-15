## Testable with:
##
##   jq -n 'include "prof0-defaults" { search: "nix/workbench/profiles" }; era_defaults("babbage")'
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

    ## Blockchain time & block density
    , active_slots_coeff:             0.05
    , epoch_length:                   600   # Ought to be at least (10 * k / f).
    , parameter_k:                    3
    , slot_duration:                  1

    ## Protocol parameters
    , pparamsEpoch:                   300   # See: pparams/epoch-timeline.jq
    , pparamsOverlays:                []
    }

  , generator:
    { add_tx_size:                    100
    , init_cooldown:                  5
    , inputs_per_tx:                  2
    , outputs_per_tx:                 2
    , tx_fee:                         1000000
    , epochs:                         3
    , tps:                            12
    , plutus:
      { type:   null
      , script: null
      }
    }

  , node:
    { rts_flags_override:             []
    , shutdown_on_slot_synced:        null
    , shutdown_on_block_synced:       null
    , tracing_backend:                "trace-dispatcher"  ## or "iohk-monitoring"
    , ekg:                            false
    , tracer:                         true
    , verbatim:
      {
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
    , filters:                        ["unitary"]
    }
  }
} | (.common * (.[$era] // {}));
