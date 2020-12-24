include "defaults" { search: "../profiles" };
include "cli-args" { search: "../profiles" };

def may_attr($attr; $dict; $defdict; $scale; $suf):
  if ($dict[$attr] //
      error("undefined attr: \($attr)"))
     != $defdict[$attr]
  then [($dict[$attr] | . / $scale | tostring) + $suf] else [] end;

def profile_name($p):
  era_defaults($p.era).genesis     as     $genesis_defaults
| era_defaults($p.era).generator   as   $generator_defaults
| era_defaults($p.era).composition as $composition_defaults
  ## Genesis
| [ "k\($p.genesis.n_pools)" ]
  + may_attr("dense_pool_density";
             $p.composition; $composition_defaults; 1; "ppn")
  + [ ($p.generator.epochs              | tostring) + "ep"
    , ($p.genesis.utxo       | . / 1000 | tostring) + "kU"
    , ($p.genesis.delegators | . / 1000 | tostring) + "kD"
    ]
  + may_attr("tps";
             $p.generator; $generator_defaults; 1; "tps")
  + may_attr("max_block_size";
             $p.genesis;     $genesis_defaults; 1000; "kb")
  + may_attr("add_tx_size";
             $p.generator; $generator_defaults; 1; "b")
  + may_attr("inputs_per_tx";
             $p.generator; $generator_defaults; 1; "i")
  + may_attr("outputs_per_tx";
             $p.generator; $generator_defaults; 1; "o")
  | join("-");

def add_derived_params:
  (.future_offset //
   if      .composition.n_hosts > 50 then 32
   else if .composition.n_hosts == 3 then 3
   else 10 end end)                          as $future_offset
| .composition                               as $compo
| .genesis                                   as $gsis
| .generator                                 as $gtor
| .tolerances                                as $tolr
| ($gsis.epoch_length * $gsis.slot_duration) as $epoch_duration
| ($epoch_duration * $gtor.epochs)           as $duration
| (if $compo.dense_pool_density > 1
   then $compo.n_singular_hosts
   else $compo.n_singular_hosts + $compo.n_dense_hosts
   end)                                      as $n_singular_pools
| ($compo.n_dense_hosts * $compo.dense_pool_density)
                                             as $n_dense_pools
| ($n_singular_pools + $n_dense_pools)       as $n_pools

## Note how derivations come in phases, too:
##
| (## First derivation:
   { common:
     { composition:
         { n_pools:               $n_pools
         , n_singular_pools:      $n_singular_pools
         , n_dense_pools:         $n_dense_pools
         }
     , genesis:
         { genesis_future_offset: "\($future_offset) minutes"
         , delegators:            ($gsis.delegators // $n_pools)
         , pool_coin:             ($gsis.pools_balance / $n_pools | floor)
         , verbatim:
           { protocolParams:
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
           }
         }
     , generator:
         { tx_count:              ($duration * ([$gtor.tps, 7] | min))
         }
     , node:
         {
         }
     , tolerances:
         { minimum_chain_density: ($gsis.active_slots_coeff * 0.5)
         }
     }
   } | . *
   ## Second derivation:
   { common:
     { genesis:
       { delegator_coin:          ($gsis.pools_balance /
                                   .common.genesis.delegators
                                   | floor)
       }
     }
   })  as $derived
| . * $derived.common
    * ($derived[.era] // {})
| . *
    { name:     (.name // profile_name(.))
    , cli_args: profile_cli_args(.)
    }
;
