## Profiles are named bundles of parameters, classified into sections:
##
##  - genesis
##  - generator
##  - node
##  - tolerances (ranges of acceptable properties, given above parameters)
##
## When combined with cluster COMPOsition (an extract from topology) and
## service parameters (non-crucial things like state directory, port ranges, etc),
## profiles ought to completely specify cluster parameters,
## allowing genesis and all necessary configuration files to be generated.
##
## Profiles themselves are layered as follows:
##
##  - era-dependent defaults for the aforementioned sections:
##    - profiles/defaults.jq
##
##  - overlayed with generated profile variants + aux (ad-hoc) profiles:
##    - profiles/variants.jq and profiles/aux.jq
##
##  - each then further overlayed with derived parameters, computed from the above:
##    - profiles/derived.jq
##
## Profiles variants are generated as a cartesian product of variations
## of the three axes: genesis, generator and node.
## These generated profiles are assigned computed names, as per the
## profile_name() function in 'profiles/derived.jq'.
##
## Composition must have the following structure:
##  { n_hosts:          INT
##  , n_bft_hosts:      INT
##  , n_singular_hosts: INT
##  , n_dense_hosts:    INT
##  }
##  ..where n_hosts must be equal to a sum of the rest.
##
## Testable by:
##
##   jq -n 'include "profiles" { search: "nix/supervisord-cluster" }; profiles("shelley"; { n_bft_hosts: 1, n_dense_hosts: 1, n_singular_hosts: 1, n_hosts: 3 }; null)'
##

include "topology"    { search: "profiles" };
include "defaults"    { search: "profiles" };
include "aux"         { search: "profiles" };
include "variants"    { search: "profiles" };
include "derived"     { search: "profiles" };

def profiles($era; $mcompo; $topo):
    ($mcompo // topology_composition($topo // {}) // {}) as $compo

  ## Profiles are variants + custom (or aux) profiles:
  | all_profile_variants + aux_profiles

  | map (## Each profile extends defaults:
         era_defaults($era) * .

         ## Profiles can define their own cluster composition.
         | . * { composition: (.composition // $compo) }

         ## Compute the derived params.
         | add_derived_params

         ## Finally, assembly into a dictionary..
         | { "\(.name)":
               ## ..and cleanup:
               . | delpaths ([["generator", "epochs"]])}
        )
  | add;
