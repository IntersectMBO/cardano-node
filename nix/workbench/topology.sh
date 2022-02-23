usage_topology() {
     usage "topology" "Topology generation" <<EOF
    The workbench generates topologies with the following properties:
      - all node names are of the form "node-N", where N is a 0-based natural
      - the BFT producer, is always present and is node-0;  inactived by d=0
      - a non-dense, regular pool producer is always present, and is always node-1
      - further nodes are dense pools (so potentially configurable to densities >1)
      - there is an optional observer as the last node

    For the producer section of the topology:
      - the topology is distributed across M regions,
          and is an M-circle toroidal mesh, one circle per distinct region
      - each node has two connections (upstreams) to immediate in-circle neighbors,
          and also two connections to nodes 1/3 circle length away,
          clockwise and counter-clockwise
      - each node has an additional connection to a neighbor in another circle
      - as the total topology node count decreases, the topology gracefully degrades
          to simpler fully-connected graphs, such as two mutually connected nodes,
          and then to a single lone node

    Whenever an observer node is present, it is connected to all producers.

    Subcommands:

    make PROFILE-JSON OUTDIR
                          Generate the full cluster topology, including:
                            - the Nixops/'cardano-ops' style topology
                            - the .dot and .pdf rendering

    density-map PROFILE-JSON TOPO-DIR
                          Generate the profile-induced map of node names
                            to pool density: 0, 1 or N (for dense pools)

    projection-for ROLE N PROFILE TOPO-DIR BASE-PORT
                          Given TOPO-DIR, containing the full cluster topology,
                            print topology for the N-th node, given its ROLE,
                            while assigning it a local port number BASE-PORT+N
EOF
}

topology() {
local op=${1:---help)}; shift

case "${op}" in
    make )
        local usage="USAGE:  wb topology make PROFILE-JSON OUTDIR"
        local profile_json=${1:?$usage}
        local outdir=${2:?$usage}

        local n_hosts=$(jq .composition.n_hosts "$profile_json")

        ## 0. Generate:
        #
        mkdir -p                 "$outdir"
        args=( --topology-output "$outdir"/topology-nixops.json
               --dot-output      "$outdir"/topology.dot
               $(jq '.composition.topology
                    ' --raw-output "$profile_json")
               --size             $n_hosts
               $(jq '.composition.locations
                    | map("--loc " + .)
                    | join(" ")
                    ' --raw-output "$profile_json")
             )
        cardano-topology "${args[@]}"

        ## 1. Render PDF:
        #
        neato -s120 -Tpdf \
              "$outdir"/topology.dot > "$outdir"/topology.pdf

        ## 2. Patch the nixops topology with the density information:
        #
        jq --slurpfile prof "$profile_json" '
           def nixops_topology_set_pool_density($topo; $density):
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

           nixops_topology_set_pool_density(.; $prof[0].dense_pool_density)
           '   "$outdir"/topology-nixops.json |
        sponge "$outdir"/topology-nixops.json
        ;;

    density-map )
        local usage="USAGE:  wb topology density-map PROFILE-JSON TOPO-DIR"
        local profile_json=${1:?$usage}
        local topo_dir=${2:?$usage}

        args=(--slurpfile profile  "$profile_json"
              --slurpfile topology "$topo_dir"/topology-nixops.json
              --null-input --compact-output
             )
        jq ' $topology[0] as $topo
           | $topo.coreNodes
           | map
             ({ key:   "\(.nodeId)"
              , value: ((.pools) // 0)
              })
           | from_entries
           ' "${args[@]}";;

    projection-for )
        local usage="USAGE:  wb topology $op ROLE N PROFILE TOPO-DIR BASE-PORT"
        local role=${1:?$usage}
        local i=${2:?$usage}
        local profile=${3:?$usage}
        local topo_dir=${4:?$usage}
        local basePort=${5:?$usage}

        local prof=$(profile get $profile)

        case "$role" in
        local-bft | local-pool )
            args=(--slurpfile topology "$topo_dir"/topology-nixops.json
                  --argjson   basePort $basePort
                  --argjson   i         $i
                  --null-input
                 )
            jq '
            def loopback_node_topology_from_nixops_topology($topo; $i):
                $topo.coreNodes[$i].producers                      as $producers
              | ($producers | map(ltrimstr("node-") | fromjson))   as $prod_indices
              | { Producers:
                  ( $prod_indices
                    | map
                      ({ addr:    "127.0.0.1"
                      , port:    ($basePort + .)
                      , valency: 1
                      }
                      ))
                };

            loopback_node_topology_from_nixops_topology($topology[0]; $i)
            ' "${args[@]}";;
        local-proxy )
            local   name=$(jq '.name'   <<<$prof --raw-output)
            local preset=$(jq '.preset' <<<$prof --raw-output)
            local topo_proxy=$(profile preset-get-file "$preset" 'proxy topology' 'topology-proxy.json')

            jq . "$topo_proxy";;
        local-observer )
            args=(--slurpfile topology "$topo_dir"/topology-nixops.json
                  --argjson   basePort $basePort
                  --null-input
                 )
            jq '
            def loopback_observer_topology_from_nixops_topology($topo):
              [range(0; $topo.coreNodes | length)] as $prod_indices
            | { Producers:
                ( $prod_indices
                | map
                  ({ addr:    "127.0.0.1"
                   , port:    ($basePort + .)
                   , valency: 1
                   }
                  ))
              };

            loopback_observer_topology_from_nixops_topology($topology[0])
            ' "${args[@]}";;
        * ) fail "unhandled role for topology node '$i': '$role'";;
        esac;;

    * ) usage_topology;; esac
}
