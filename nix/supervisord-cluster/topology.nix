{ stateDir
, graphviz
, composition
, localPortBase
}:
with composition;
let
  # metadata = {
  #   inherit benchmarkingProfileName benchmarkingProfile benchmarkingTopology;
  # };
  producers =  cfg.producers;

  topology = builtins.toFile "topology.yaml" (builtins.toJSON {
    Producers =
      map (n: {
        addr = let a = n.addr or n; in if (nodes ? ${a}) then hostName a else a;
        port = n.port or nodePort;
        valency = n.valency or 1;
      }) cfg.producers;
  });
in

''
  mkdir -p "${stateDir}/topologies"

  ## 0. Generate the overall topology, in the 'cardano-ops'/nixops style:
  #
  args=( --topology-output "${stateDir}/topology-nixops.json"
         --dot-output      "${stateDir}/topology.dot"
         --size             ${toString n_hosts}
         ${toString
           (map (l: ''--loc ${l}'') locations)}
       )
  topology "''${args[@]}"

  ${graphviz}/bin/neato -s120 -Tpdf \
     "${stateDir}/topology.dot" > "${stateDir}/topology.pdf"

  ## 1. Patch the nixops topology with the density information:
  #
  jq 'def nixops_topology_set_pool_density($topo; $density):
        $topo *
        { coreNodes:
          ( .coreNodes
          | map
            ( . *
              { pools:
                (if .pools == null then 0 else
                 if .pools == 1    then 1 else
                    ([$density, 1] | max) end end)
              }
            )
          )
        };

      nixops_topology_set_pool_density(.; ${toString dense_pool_density})
     '   "${stateDir}/topology-nixops.json" |
  sponge "${stateDir}/topology-nixops.json"

  ## 2. Extract/generate the per-node topologies from the nixops topology:
  #
  #     NOTE: this relies on the topology using a particular nomenclature
  #           for node names:  'node-N', where N are contiguous, 0-based naturals.
  #
  for i in $(seq 0 $((${toString n_hosts} - 1)))
  do args=( --argjson   i         $i
            --slurpfile topology "${stateDir}/topology-nixops.json"
            --null-input
          )
     jq 'def loopback_node_topology_from_nixops_topology($topo; $i):
           $topo.coreNodes[$i].producers                      as $producers
         | ($producers | map(ltrimstr("node-") | fromjson))   as $prod_indices
         | { Producers:
             ( $prod_indices
             | map
               ({ addr:    "127.0.0.1"
                , port:    (${toString localPortBase} + .)
                , valency: 1
                }
               ))
           };

         loopback_node_topology_from_nixops_topology($topology[0]; $i)
        ' "''${args[@]}" > "${stateDir}/topologies/node-$i.json"
  done

  ## 3. Generate the observer topology, which is fully connected:
  #
  args=( --slurpfile topology "${stateDir}/topology-nixops.json"
         --null-input
       )
  jq 'def loopback_observer_topology_from_nixops_topology($topo):
        [range(0; $topo.coreNodes | length)] as $prod_indices
      | { Producers:
          ( $prod_indices
          | map
            ({ addr:    "127.0.0.1"
             , port:    (${toString localPortBase} + .)
             , valency: 1
             }
            ))
        };

      loopback_observer_topology_from_nixops_topology($topology[0])
     ' "''${args[@]}" > "${stateDir}/topologies/observer.json"
''
