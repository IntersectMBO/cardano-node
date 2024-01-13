usage_topology() {
     usage "topology" "Topology generation" <<EOF
    The workbench generates topologies with the following properties:
      - all node names are of the form "node-N", where N is a 0-based natural
      - the BFT producer, is always present and is node-0;  inactived by d=0
      - a non-dense, regular pool producer is always present, and is always node-0
      - further nodes are dense pools (so potentially configurable to densities >1)
      - there is an optional explorer as the last node

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

    Whenever an explorer node is present, it is connected to all producers.

  $(red topology) $(blue subops):

    $(helpcmd make PROFILE-JSON OUTDIR)
                          Generate the full cluster topology, including:
                            - the Nixops/'cardano-ops' style topology
                            - the .dot and .pdf rendering

    $(helpcmd density-map NODE-SPECS-JSON)
                          Generate the profile-induced map of node names
                            to pool density: 0, 1 or N (for dense pools)

    $(helpcmd projection-for ROLE N PROFILE TOPO-DIR BASE-PORT)
                          Given TOPO-DIR, containing the full cluster topology,
                            print topology for the N-th node, given its ROLE,
                            while assigning it a local port number BASE-PORT+N
EOF
}

topology() {
local op=${1:---help)}; shift || true

case "${op}" in
    make )
        local usage="USAGE:  wb topology make PROFILE-JSON OUTDIR"
        local profile_json=${1:?$usage}
        local outdir=${2:?$usage}

        local topology_name=$(jq '.composition.topology' --raw-output "$profile_json")
        local n_hosts=$(jq .composition.n_hosts "$profile_json")

        ## 0. Generate:
        #
        mkdir -p                 "$outdir"
        args=( --topology-output "$outdir"/topology.json
               --dot-output      "$outdir"/topology.dot
               "$topology_name"
               --size             $n_hosts
               $(jq '.composition.locations
                    | map("--loc " + .)
                    | join(" ")
                    ' --raw-output "$profile_json")
             )
        if jqtest .composition.with_explorer $profile_json
        then args+=('--with-explorer')
        fi
        progress "topology" "cardano-topology ${args[*]}"
        cardano-topology "${args[@]}"
        # Patch the nixops topology with the density information:
        # This is only needed here, the dense topology was already imported
        # from nixops / cardano-ops.
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
           '   "$outdir"/topology.json |
          sponge "$outdir"/topology.json

        ## 1. Render GraphViz topology PDF:
        #
        neato -s120 -Tpdf \
              "$outdir"/topology.dot > "$outdir"/topology.pdf
        ;;

    dot )
        local usage="USAGE:  wb topology dot TOPOLOGY-JSON"
        local topology_file=${1:?$usage}

        # Top object start
        ##################
        echo "digraph dense {"
        echo "  splines=true;"
        echo "  overlap=false;"
        # Add each node to its corresponding location array.
        local lo_array=() eu_array=() us_array=() ap_array=()
        # Grab all the "i" properties from inside each "node-i" object
        # Why "i" and not name? `jq` sorts like: "node-49", "node-5", "node-50"
        local nodes_array
        nodes_array=$(jq --raw-output '.coreNodes | map(.nodeId) | sort | join (" ")' "${topology_file}")
        for node_i in ${nodes_array[*]}
        do
            # Fetch this node's JSON object description.
            local node
            node=$(jq -r ".coreNodes | map(select( .nodeId == ${node_i} )) | .[0]" "${topology_file}")
            local region
            region="$(echo "${node}" | jq -r .region)"
            local color
            if echo "${region}" | grep --quiet "eu-central-"
            then
                color="blue"
                eu_array+=("${node_i}")
            elif echo "${region}" | grep --quiet "us-east-"
            then
                color="red"
                us_array+=("${node_i}")
            elif echo "${region}" | grep --quiet "ap-southeast-"
            then
                color="green"
                ap_array+=("${node_i}")
            else
                color="black"
                lo_array+=("${node_i}")
            fi
        done
        # Output a GraphViz "subgraph" for each of the regions.
        echo "  subgraph eu {"
        echo "    label = \"EU\";"
        echo "    cluster=true;"
        for node_i in ${eu_array[*]}
        do
            local color="blue"
            echo "    \"node-${node_i}\" [fillcolor=${color}, style=filled];"
        done
        echo "  }"
        echo "  subgraph us {"
        echo "    label = \"US\";"
        echo "    cluster=true;"
        for node_i in ${us_array[*]}
        do
            local color="red"
            echo "    \"node-${node_i}\" [fillcolor=${color}, style=filled];"
        done
        echo "  }"
        echo "  subgraph ap {"
        echo "    label = \"AP\";"
        echo "    cluster=true;"
        for node_i in ${ap_array[*]}
        do
            local color="green"
            echo "    \"node-${node_i}\" [fillcolor=${color}, style=filled];"
        done
        echo "  }"
        # Output each node's connections with its corresponding color.
        for node_i in ${eu_array[*]}
        do
            local node
            node=$(jq -r ".coreNodes | map(select( .nodeId == ${node_i} )) | .[0]" "${topology_file}")
            local producers_array
            producers_array=$(echo "${node}" | jq --raw-output '.producers | sort | join (" ")')
            local color="blue"
            for producer in ${producers_array[*]}
            do
                echo "  \"node-${node_i}\" -> \"${producer}\" [color=${color}];"
            done
        done
        for node_i in ${us_array[*]}
        do
            local node
            node=$(jq -r ".coreNodes | map(select( .nodeId == ${node_i} )) | .[0]" "${topology_file}")
            local producers_array
            producers_array=$(echo "${node}" | jq --raw-output '.producers | sort | join (" ")')
            local color="red"
            for producer in ${producers_array[*]}
            do
                echo "  \"node-${node_i}\" -> \"${producer}\" [color=${color}];"
            done
        done
        for node_i in ${ap_array[*]}
        do
            local node
            node=$(jq -r ".coreNodes | map(select( .nodeId == ${node_i} )) | .[0]" "${topology_file}")
            local producers_array
            producers_array=$(echo "${node}" | jq --raw-output '.producers | sort | join (" ")')
            local color="green"
            for producer in ${producers_array[*]}
            do
                echo "  \"node-${node_i}\" -> \"${producer}\" [color=${color}];"
            done
        done
        # Top object end
        ################
        echo "}"
    ;;

    # For the value profile returns:
    # {
    # "0":1,
    # "1":1,
    # ...
    # "51":1
    # }
    density-map )
        local usage="USAGE:  wb topology density-map NODE-SPECS-JSON"
        local node_specs_json=${1:?$usage}

        args=(--slurpfile node_specs "$node_specs_json"
              --null-input --compact-output
             )
        # Filter the explorer node if not {...,"52":0}
        jq ' $node_specs[0]
          | to_entries
          | map( select(.value.kind == "pool") )
          | map
            ({ key:   "\(.value.i)"
             , value: ((.value.pools) // 0)
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

        local prof=$(profile json $profile)

        case "$role" in
        local-bft | local-pool )
            local jq_function
            if jqtest ".node.verbatim.EnableP2P" <<<$prof
            then
              jq_function="p2p_loopback_node_topology_from_nixops_topology"
            else
              jq_function="loopback_node_topology_from_nixops_topology"
            fi
            args=(-L$global_basedir
                  --slurpfile topology "$topo_dir"/topology.json
                  --argjson   basePort $basePort
                  --argjson   i        $i
                  --null-input
                 )
            jq \
                "include \"topology\"; $jq_function(\$topology[0]; \$i)" \
                "${args[@]}"
        ;;
        local-proxy )
            local   name=$(jq '.name'   <<<$prof --raw-output)
            local preset=$(jq '.preset' <<<$prof --raw-output)
            local topo_proxy=$(profile preset-get-file "$preset" 'proxy topology' 'topology-proxy.json')

            jq . "$topo_proxy";;
        local-chaindb-server )
            ## ChainDB servers are just that:
            jq --null-input "{ Producers: [] }";;
        local-explorer )
            args=(-L$global_basedir
                  --argjson   basePort     $basePort
                 )
            jq 'include "topology";

            composition_explorer_topology_loopback(.composition; $basePort)
            ' "${args[@]}" <<<$prof;;
        * ) fail "unhandled role for topology node '$i': '$role'";;
        esac;;

    * ) usage_topology;; esac
}
