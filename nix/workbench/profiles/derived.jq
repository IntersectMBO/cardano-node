include "defaults";
include "genesis";
include "lib";

def default_value_tx_size_estimate:
  381;

def may_mult($x):
  if type != "number" then null else . * $x end;

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
| $p.node.shutdown_on_block_synced as       $shutdown_block
| ($p.node.shutdown_on_slot_synced // (($shutdown_block * 1.5 / $p.genesis.active_slots_coeff)
                                       | ceil))
                                   as       $shutdown_slots
  ## Genesis
| [ "k\($p.composition.n_pools)" ]
  + if $p.composition.n_dense_hosts > 0
    then may_attr("dense_pool_density";
                  $p.composition; $composition_defaults; 1; "ppn")
    else [] end
  + if $shutdown_slots | type == "number"
    then [($shutdown_slots | tostring) + "slots"]
    else [ ($p.generator.epochs                  | tostring) + "ep"
         , ($p.generator.tx_count | . / 1000 | ceil | tostring) + "kTx" ]
    end
  + [ ($p.genesis.utxo           | . / 1000 | tostring) + "kU"
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
  + if $p.generator.plutusMode | not then []
    else [ ($p.generator.plutusScript | rtrimstr(".plutus"))
         , ($p.generator.plutusData | tostring)
         ] end
  + if $p.node.rts_flags_override == [] then []
    else ["RTS", ($p.node.rts_flags_override | join(""))] end
  + if $p.composition.with_proxy
    then ["prox"]
    else [] end
  + if $p.composition.with_chaindb_server
    then ["chdb"]
    else [] end
  + if $p.composition.with_observer
    then ["obsrv"]
    else [] end
  + if $p.scenario == "default" then [] else [$p.scenario] end
  | join("-");

def profile_name_era_suffix($era):
  "-\($era | (.[0:2] + .[-2:]))";

def add_derived_params:
  (.genesis.utxo + .genesis.delegators)      as $dataset_measure
| (if $dataset_measure < 10000 then 10
   else $dataset_measure / 50000
   end | ceil)                               as $dataset_induced_startup_delay_optimistic
| (if $dataset_measure < 10000 then 10
   else $dataset_measure / 10000
   end | ceil)                               as $dataset_induced_startup_delay_conservative
| (.derived.genesis_future_offset //
   "\($dataset_induced_startup_delay_optimistic) seconds")
                                             as $genesis_future_offset
| .composition                               as $compo
| .genesis                                   as $gsis
| .generator                                 as $gtor
| .analysis                                  as $ana
| .era                                       as $era
| .node                                      as $node

## Absolute durations:
| ($gsis.epoch_length * $gsis.slot_duration) as $epoch_duration
| $node.shutdown_on_block_synced             as $shutdown_block
| ($node.shutdown_on_slot_synced // (($shutdown_block * 1.5 / $gsis.active_slots_coeff)
                                     | ceil))
                                             as $shutdown_slots
| (if $shutdown_slots | type == "number"
   then $shutdown_slots / $gsis.epoch_length | ceil
   else $gtor.epochs
   end)                                      as $effective_epochs
| ($epoch_duration * $effective_epochs)      as $generator_requested_duration
| ($shutdown_slots | may_mult($gsis.slot_duration)) as $shutdown_time
| ([ $generator_requested_duration
   , $shutdown_time
   ] | drop_nulls | min)                     as $generator_duration

## Tx count for inferred absolute duration.
##   Note that this the workload would take longer, if we saturate the cluster.
| ($gtor.tx_count // ($generator_duration * $gtor.tps) | ceil)
                                             as $generator_tx_count
## Effective cluster composition:
| (if $compo.dense_pool_density > 1
   then { singular:  $compo.n_singular_hosts
        , dense:     $compo.n_dense_hosts }
   else { singular: ($compo.n_singular_hosts + $compo.n_dense_hosts)
        , dense:     0 }
   end)                                      as $hosts
| ($hosts.dense * $compo.dense_pool_density) as $n_dense_pools
| ($hosts.singular + $n_dense_pools)         as $n_pools

| ($gsis.delegators // $n_pools)             as $effective_delegators

| ($generator_tx_count * $gtor.inputs_per_tx)
                                             as $utxo_generated
| (($gsis.max_block_size / default_value_tx_size_estimate) | floor)
                                             as $default_value_tx_per_block_estimate
## Note how derivations come in phases, too:
##
| (## First derivation:
   { common:
     (($gsis.per_pool_balance * $n_pools) as $supply_delegated
     |
     { derived:
         { supply_delegated:              $supply_delegated
         , supply_total:                  ($supply_delegated + $gsis.funds_balance)

         , utxo_delegated:                $effective_delegators
         , utxo_generated:                $utxo_generated
           ## Stuffed UTxO is what we need over requested-UTxO + delegators' UTxO:
         , utxo_stuffed:                  ([ $gsis.utxo
                                           , 0
                                           ] | max)

         , delegators_effective:          ([ $n_pools
                                           , $gsis.delegators
                                           ] | max)

         , dataset_measure:               $dataset_measure
         , dataset_induced_startup_delay_optimistic:   $dataset_induced_startup_delay_optimistic
         , dataset_induced_startup_delay_conservative: $dataset_induced_startup_delay_conservative

         , genesis_future_offset:         $genesis_future_offset
         , epoch_duration:                $epoch_duration
         , generator_duration:            $generator_duration
         , shutdown_time:                 $shutdown_time

         , effective_epochs:              $effective_epochs

         , generator_tx_count:            $generator_tx_count

         , default_value_tx_size_estimate: default_value_tx_size_estimate
         , default_value_tx_per_block_estimate: $default_value_tx_per_block_estimate
         , generator_blocks_lower_bound:  ($generator_tx_count / $default_value_tx_per_block_estimate
                                           | . * 1.15 | ceil)
         }
     , composition:
         { n_hosts:               ($compo.n_bft_hosts + $hosts.singular + $hosts.dense)
         , n_pools:               $n_pools
         , n_singular_hosts:      $hosts.singular
         , n_singular_pools:      $hosts.singular
         , n_dense_hosts:         $hosts.dense
         , n_dense_pools:         $n_dense_pools
         , n_pool_hosts:          ($hosts.singular + $hosts.dense)
         }
     , genesis:
         { delegators:            $effective_delegators
         , pool_coin:             (if $n_pools == 0 then 0
                                   else $gsis.per_pool_balance end)
         , shelley:
           ({
             activeSlotsCoeff:           $gsis.active_slots_coeff
           , epochLength:                $gsis.epoch_length
           , securityParam:              $gsis.parameter_k
           , slotLength:                 $gsis.slot_duration
           , protocolParams:
              { maxTxSize:                  $gsis.max_tx_size
              , decentralisationParam:      $gsis.decentralisation_param
              , maxBlockBodySize:           $gsis.max_block_size
              , nOpt:                       $compo.n_pools
              }
            } * ($gsis.shelley // {}))
         # , alonzo: supposed to already be filled
         }
     , generator:
         { tx_count:              $generator_tx_count
         }
     , node:
         {
         }
     , analysis:
         { minimum_chain_density:      ($gsis.active_slots_coeff * 0.5)
         , cluster_startup_overhead_s: $dataset_induced_startup_delay_conservative
         }
     })
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
  [ "  - era:                \($p.era)"
  , "  - epoch slots:        \($p.genesis.epoch_length)"
  , "  - slot duration:      \($p.genesis.slot_duration)"
  , "  - k:                  \($p.genesis.parameter_k)"
  , "  - active slots coeff: \($p.genesis.active_slots_coeff)"
  , "  - hosts:              \($p.composition.n_hosts)"
  , "  - pools:              \($p.composition.n_pools)"
  , "    - normal:             \($p.composition.n_singular_pools)"
  , "    - dense:              \($p.composition.n_dense_pools)"
  , "  - UTxO:               \($p.genesis.utxo), of which:"
  , "    - delegated:          \($p.derived.utxo_delegated)"
  , "    - generated:          \($p.derived.utxo_generated)"
  , "    - stuffed:            \($p.derived.utxo_stuffed)"
  , "  - delegators:         \($p.genesis.delegators)"
  , "  - generator duration: \($p.derived.generator_duration              | tostring)s"
  , "    - requested epochs:   \($p.generator.epochs                      | tostring)ep"
  , "    - effective epochs:   \($p.derived.effective_epochs              | tostring)ep"
  , "    - transaction count:  \($p.derived.generator_tx_count | . / 1000 | ceil | tostring)kTx"
  , "    - full blocks:        \($p.derived.generator_blocks_lower_bound  | tostring)"
  , ""
  ]
  | . + if $p.node.shutdown_on_slot_synced == null then []
        else [
    "  - terminate at slot:  \($p.node.shutdown_on_slot_synced)"
        ] end
  | . + if $p.node.shutdown_on_block_synced == null then []
        else [
    "  - terminate at block: \($p.node.shutdown_on_block_synced)"
        ] end
  | . + [""]
  | join("\n");
