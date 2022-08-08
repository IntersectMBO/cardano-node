def loopback_node_topology_from_nixops_topology($backend; $topo; $i):
    $topo.coreNodes[$i].producers                      as $producers
  | ($producers | map(ltrimstr("node-") | fromjson))   as $prod_indices
  | { Producers:
      ( $prod_indices
        | map
          ({ addr:   (if $backend == "supervisor"
                     then "127.0.0.1"
                     else ( "172.22." + (((. / 254) | floor) | tostring) + "." + ((. % 254 + 1) | tostring) )
                     end)
          , port:    ($basePort + .)
          , valency: 1
          }
          ))
    };

def composition_block_sources_nr($compo):
  $compo.n_bft_hosts
+ $compo.n_pool_hosts
+ if $compo.with_proxy          then 1 else 0 end
+ if $compo.with_chaindb_server then 1 else 0 end;

def composition_observer_topology_loopback($backend; $compo; $basePort):
  [range(0; composition_block_sources_nr($compo))]
    as $src_indices
| { Producers:
    ( $src_indices
    | map
      ({ addr:    (if $backend == "supervisor"
                   then "127.0.0.1"
                   else ( "172.22." + (((. / 254) | floor) | tostring) + "." + ((. % 254 + 1) | tostring) )
                   end
                  )
       , port:    ($basePort + .)
       , valency: 1
       }
      ))
  };
