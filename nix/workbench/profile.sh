usage_profile() {
     usage "profile" "Cluster profile operations" <<EOF
    list                  List profile names (json)
    all-profiles | all    All profile contents (json)
    compose NAME..        Create a profile composed from named profiles
    get NAME              Get contents of named profile
    describe NAME         Print a human description of a profile
    node-specs PROFILE-JSON PORT-BASE STAGGER-PORTS
                          Print node specs JSON for the given profile;
                            If STAGGER-PORTS is true, the assigned ports will
                            be incrementally distinc for each node spec
EOF
}

global_profile_eras=(
    shelley
    allegra
    mary
)

profile() {
local op=${1:-list}; test $# -gt 0 && shift

case "${op}" in
    list | names )
        profile generate-all | jq 'keys'
        ;;

    all-profiles | generate-all | all )
        (cd "$global_basedir/profiles";

         jq --argjson eras "$(to_jsonlist ${global_profile_eras[*]})" --null-input '
           include "profiles";

           $eras
           | map(profiles(.; null; null; []))
           | add
           ');;

    compose )
        local profile_names="$@"

        profile generate-all |
        jq --argjson profile_names "$(to_jsonlist ${profile_names[*]})" '
          . as $profiles
          | $profile_names | debug
          | map($profiles[.])
          | add
          ';;

    get )
        local usage="USAGE: wb profile get NAME"
        local name=${1:?$usage}

        profile generate-all |
        jq '.["'$name'"]'
        ;;

    describe )
        local usage="USAGE: wb profile describe NAME"
        local name=${1:?$usage}

        profile get $name |
        (cd "$global_basedir/profiles";

         echo -n "workbench:  "
         jq '
          include "derived";
          profile_pretty_describe(.)
          ' --raw-output);;

    node-specs )
        local usage="USAGE: wb profile node-specs PROFILE-JSON PORT-BASE STAGGER-PORTS"
        local profile_json=${1:?$usage}
        local port_base=${2:?$usage}
        local stagger_ports=${3:?$usage}

        args=(
            "$profile_json"
            --argjson port_base     $port_base
            --argjson stagger_ports $stagger_ports
        )
        jq '. as $prof
           | $prof.composition.n_bft_hosts  as $n_bfts
           | $prof.composition.n_pool_hosts as $n_pools
           | ([range(0;
                     $n_bfts)]
              | map({ i: .
                    , kind: "bft"
                    }))
              as $bfts
           | ([range($n_bfts;
                     $n_bfts + $n_pools)]
              | map({ i: .
                    , kind: "pool"
                    }))
              as $pools
           | ([range($n_bfts + $n_pools;
                     $n_bfts + $n_pools +
                     if $prof.composition.with_observer then 1 else 0 end)]
              | map({ i: .
                    , kind: "observer"
                    }))
              as $observers
           | ($bfts + $pools + $observers
              | map(. +
                    { name:       "node-\(.["i"])"
                    , isProducer: ([.kind == "bft", .kind == "pool"] | any)
                    , port:
                      (if $stagger_ports
                       then $port_base + .i
                       else $port_base
                       end)
                    }))
           | map({ key: .name, value: .})
           | from_entries
           ' "${args[@]}";;

    * ) usage_profile;; esac
}
