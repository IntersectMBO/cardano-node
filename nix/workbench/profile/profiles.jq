## Profiles are named bundles of parameters, where parameters are
## classified into sections:
##
##  - genesis
##  - generator
##  - node
##  - analysis (properties relevant to analysis)
##  - derived  (properties derived from all above)
##
## When combined with cluster composition (an extract from topology, see the
## definition below) and service parameters (non-crucial things like state directory,
## port ranges, etc), profiles ought to completely specify cluster parameters,
## allowing genesis and all necessary configuration files to be generated.
##
## Profiles themselves are layered as follows:
##
##  - era-dependent defaults for the aforementioned sections:
##    - profile/defaults.jq
##
##  - overlaid with generated profile variants (including adhoc profiles):
##    - profile/variants.jq
##
##  - each then further overlaid with derived parameters, computed from the above:
##    - profile/derived.jq
##
## Profiles variants are generated as a cartesian product of variations
## of the three axes: genesis, generator and node.
## These generated profiles are assigned computed names, as per the
## profile_name() function in 'profile/prof3-derived.jq'.
##
## Cluster composition must have the following structure:
##  { n_bft_hosts:      INT
##  , n_singular_hosts: INT
##  , n_dense_hosts:    INT
##  }
##
## Names of non-adhoc profiles are computed by the 'profile_name' function in
## profile/prof3-derived.jq, with era suffix appended, and have the following structure:
##
##   k${n_pools}
##    -[${dense_pool_density}ppn]
##    -${epochs}ep
##    -${utxo}kU
##    -${delegators}kD
##    -[${tps}tps]
##    -[${max_block_size}blk]
##    -[${add_tx_size}b]
##    -[${inputs_per_tx}i]
##    -[${outputs_per_tx}o]
##
## ..where [] denote optionals, that are only included if the profile
##   deviates from profile defaults.
##
## Testable by:
##
##   nix-build -A profiles       ## or simply:  make profiles
##
## ..which simply calls ./profiles.nix with {} params.
##

include "prof0-defaults";
include "prof1-variants";
include "prof2-pparams";
include "prof3-derived";

## Cluster composition is an extract from the topology,
## that classifies nodes into BFT, regular pools and dense pools,
## based on the 'pools' field.
##
## Testable with:
##
##   jq -n 'include "composition" { search: "nix/workbench/profile" }; topology_composition({ coreNodes: { bft1: { pools: 0 } } })'
##
def topology_composition($topo):
    $topo
  | (.Producers // .coreNodes // {})
  | to_entries
  | map (.value.pools // 0)
  | length                           as $n_hosts
  | map (select (. == 0))            as $bfts
  | map (select (. != 0))            as $pools
  | ($pools | map (select (. == 1))) as $singular_pools
  | ($pools | map (select (.  > 1))) as $dense_pools
  | ($singular_pools | length)       as $n_singular_hosts
  | { n_bft_hosts:      ($bfts           | length)
    , n_singular_hosts: ($singular_pools | length)
    , n_dense_hosts:    ($dense_pools    | length)
    };

##
## This is the workbench's entry point for everything profile.
##
##  generate_all_era_profiles :: Era -> Maybe Composition -> Topology -> Map Name Profile
##
def generate_all_era_profiles($era; $mcompo; $topo):
    ($mcompo // topology_composition($topo // {}) // {}) as $default_compo
  | era_defaults($era)                             as $defaults ## prof0-defaults.jq

  | all_profile_variants
  | map (. as $variant
         ## Each profile is defined as extension of defaults:   ## prof1-variants.jq
         | ($defaults * $variant)                  as $defandvar
         | pParamsWithOverlays($defandvar.genesis.pparamsEpoch; ## prof2-pparams.jq
                               $defandvar.genesis.pparamsOverlays
                               )                   as $pparams
         | $defandvar
         | .genesis.shelley.protocolParams = $pparams.shelley
         | .genesis.alonzo                 = $pparams.alonzo
         | .genesis.alonzo.costModels      = $pparams.costModels

         ## Profiles define their own cluster composition:
         | . * { composition: (.composition // $default_compo) }

         ## Finally, compute the derived ("computed") params.
         | add_derived_params                                  ## prof3-derived.jq
        )
  | map (## Assemble into a dictionary..
           { "\(.name)": .
           })
  | add;
