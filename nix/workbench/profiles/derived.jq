include "defaults";
include "cli-args";

def may_attr($attr; $dict; $defdict; $scale; $suf):
  if ($dict[$attr] //
      error("undefined attr: \($attr)"))
     != $defdict[$attr]
  then [($dict[$attr] | . / $scale | tostring) + $suf] else [] end;

def profile_name($p):
  era_defaults($p.era).genesis     as     $genesis_defaults
| era_defaults($p.era).generator   as   $generator_defaults
| era_defaults($p.era).composition as $composition_defaults
| era_defaults($p.era).node        as        $node_defaults
  ## Genesis
| [ "k\($p.composition.n_pools)" ]
  + if $p.composition.n_dense_hosts > 0
    then may_attr("dense_pool_density";
                  $p.composition; $composition_defaults; 1; "ppn")
    else [] end
  + [ ($p.generator.epochs                  | tostring) + "ep"
    , ($p.generator.tx_count     | . / 1000 | tostring) + "kTx"
    , ($p.genesis.utxo           | . / 1000 | tostring) + "kU"
    , ($p.genesis.delegators     | . / 1000 | tostring) + "kD"
    , ($p.genesis.max_block_size | . / 1000 | tostring) + "kbs"
    ]
  + may_attr("tps";
             $p.generator; $generator_defaults; 1; "tps")
  + may_attr("add_tx_size";
             $p.generator; $generator_defaults; 1; "b")
  + may_attr("inputs_per_tx";
             $p.generator; $generator_defaults; 1; "i")
  + may_attr("outputs_per_tx";
             $p.generator; $generator_defaults; 1; "o")
  + if $p.generator.scriptMode
    then if $p.generator.plutusMode
         then [ ($p.generator.plutusScript | rtrimstr(".plutus"))
              , ($p.generator.plutusData | tostring)
              ]
         else ["scr"] end
    else ["cli"] end
  + if $p.node.rts_flags_override == [] then []
    else ["RTS", ($p.node.rts_flags_override | join(""))] end
  + if $p.composition.with_proxy
    then ["prox"]
    else [] end
  + if $p.composition.with_observer | not
    then ["nobs"]
    else [] end
  | join("-");

def profile_name_era_suffix($era):
  "-\($era | (.[0:2] + .[-2:]))";

def add_derived_params:
  (.genesis.genesis_future_offset //
    if      .composition.n_hosts > 50 then "32 minutes"
    else if .composition.n_hosts == 3 then "3 minutes"
         else "10 minutes" end end)          as $future_offset
| .composition                               as $compo
| .genesis                                   as $gsis
| .generator                                 as $gtor
| .tolerances                                as $tolr
| .era                                       as $era

## Absolute durations:
| ($gsis.epoch_length * $gsis.slot_duration) as $epoch_duration
| ($epoch_duration * $gtor.epochs)           as $duration

## Tx count for inferred absolute duration.
##   Note that this the workload would take longer, if we saturate the cluster.
| ($gtor.tx_count // ($duration * $gtor.tps))
                                             as $tx_count

## Effective cluster composition:
| (if $compo.dense_pool_density > 1
   then { singular:  $compo.n_singular_hosts
        , dense:     $compo.n_dense_hosts }
   else { singular: ($compo.n_singular_hosts + $compo.n_dense_hosts)
        , dense:     0 }
   end)                                      as $hosts
| $hosts.singular                            as $n_singular_pools
| ($hosts.dense * $compo.dense_pool_density) as $n_dense_pools
| ($n_singular_pools + $n_dense_pools)       as $n_pools

## Note how derivations come in phases, too:
##
| (## First derivation:
   { common:
     { composition:
         { n_hosts:               ($compo.n_bft_hosts + $hosts.singular + $hosts.dense)
         , n_pools:               $n_pools
         , n_singular_hosts:      $hosts.singular
         , n_singular_pools:      $n_singular_pools
         , n_dense_hosts:         $hosts.dense
         , n_dense_pools:         $n_dense_pools
         , n_pool_hosts:          ($hosts.singular + $hosts.dense)
         }
     , genesis:
         { genesis_future_offset: $future_offset
         , delegators:            ($gsis.delegators // $n_pools)
         , pool_coin:             (if $n_pools == 0 then 0
                                   else $gsis.per_pool_balance end)
         , shelley:
           ({ protocolParams:
              { activeSlotsCoeff:           $gsis.active_slots_coeff
              , epochLength:                $gsis.epoch_length
              , securityParam:              $gsis.parameter_k
              , slotLength:                 $gsis.slot_duration
              , maxTxSize:                  $gsis.max_tx_size
              , protocolParams:
                { "decentralisationParam":  $gsis.decentralisation_param
                , "maxBlockBodySize":       $gsis.max_block_size
                , "nOpt":                   $compo.n_pools
                }
              }
            } * ($gsis.shelley // {}))
         # , alonzo: supposed to already be filled
         }
     , generator:
         { tx_count:              $tx_count
         }
     , node:
         {
         }
     , tolerances:
         { minimum_chain_density: ($gsis.active_slots_coeff * 0.5)
         , cluster_startup_overhead_s:
                (($gsis.utxo + $gsis.delegators) as $dataset_size
                | if $dataset_size < 10000 then 5
                  else $dataset_size / 25000
                  end)
         }
     }
   }
   | . *
   ## Second derivation:
   { common:
     { genesis:
       ## Depends on computed delegators:
       { delegator_coin:   (if .common.genesis.delegators == 0 then 0
                            else $gsis.per_pool_balance
                            end)
       }
     }
   })  as $derived
| . * $derived.common
    * ($derived[.era] // {})
| . *
    { name:     ( .era as $era
                | (.name // profile_name(.))
                | . + profile_name_era_suffix($era)
                )
    , cli_args: profile_cli_args(.)
    }
;

def profile_pretty_describe($p):
  [ "profile: \($p.name)"
  , "  - era:                \($p.era)"
  , "  - epoch slots:        \($p.genesis.epoch_length)"
  , "  - slot duration:      \($p.genesis.slot_duration)"
  , "  - k:                  \($p.genesis.parameter_k)"
  , "  - active slots coeff: \($p.genesis.active_slots_coeff)"
  , "  - hosts:              \($p.composition.n_hosts)"
  , "  - pools:              \($p.composition.n_pools)"
  , "    - normal:             \($p.composition.n_singular_pools)"
  , "    - dense:              \($p.composition.n_dense_pools)"
  , "  - UTxO:               \($p.genesis.utxo)"
  , "  - delegators:         \($p.genesis.delegators)"
  , ""
  ]
  | . + if $p.node.shutdown_on_slot_synced == null then []
        else [
    "  - terminate at slot:  \($p.node.shutdown_on_slot_synced)"
        ] end
  | . + [""]
  | join("\n");
