def loopback_node_topology_from_nixops_topology($topo; $i):
    # DON'T ASSUME NODES ARE ORDERED INSIDE THE GLOBAL TOPOLOGY FILE!!!!!!!!!!!!
    ($topo.coreNodes | map(select(.nodeId == $i)) | .[0] | .producers) as $producers
  | ($producers | map(ltrimstr("node-") | fromjson))                   as $prod_indices
  | { Producers:
      ( $prod_indices
        | map
          ({ addr:    "127.0.0.1"
          , port:    ($basePort + .)
          , valency: 1
          }
          ))
    };

def p2p_loopback_node_topology_from_nixops_topology($topo; $i):
    # DON'T ASSUME NODES ARE ORDERED INSIDE THE GLOBAL TOPOLOGY FILE!!!!!!!!!!!!
    ($topo.coreNodes | map(select(.nodeId == $i)) | .[0] | .producers) as $producers
  | ($producers | length)                                              as $valency
  | ($producers | map(ltrimstr("node-") | fromjson))                   as $prod_indices
  | { localRoots:
      ( $prod_indices
        | map
          ({ accessPoints: [
              { address: "127.0.0.1"
              , port:    ($basePort + .)
              }
            ]
          , advertise: false
          , valency: $valency
          })
      )
    , publicRoots: []
    , useLedgerAfterSlot: -1
    }
;

def composition_block_sources_nr($compo):
  $compo.n_bft_hosts
+ $compo.n_pool_hosts
+ if $compo.with_proxy          then 1 else 0 end
+ if $compo.with_chaindb_server then 1 else 0 end;

def composition_explorer_topology_loopback($compo; $basePort):
  [range(0; composition_block_sources_nr($compo))]
    as $src_indices
| { Producers:
    ( $src_indices
    | map
      ({ addr:    "127.0.0.1"
       , port:    ($basePort + .)
       , valency: 1
       }
      ))
  };
