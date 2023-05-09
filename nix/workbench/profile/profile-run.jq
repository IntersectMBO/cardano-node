def profile_timing($prof;
                   $start;
                   $start_human;
                   $start_tag;
                   $systemStart):
  ($systemStart | fromdateiso8601 | . + $prof.derived.generator_duration)   as $workload_end
| ($systemStart | fromdateiso8601 | . + ($prof.derived.shutdown_time
                                      // $prof.derived.generator_duration)) as $shutdown_end
| ( [$shutdown_end, $workload_end]
  | map(select(. != null))
    | min)                                                                as $earliest_end
|
{ future_offset:         $prof.derived.genesis_future_offset
, extra_future_offset:   $prof.genesis.extra_future_offset
, start:                 $start
, shutdown_end:          $shutdown_end
, workload_end:          $workload_end
, earliest_end:          $earliest_end

, start_tag:       $start_tag
, start_human:     $start_human
, systemStart:     $systemStart
, shutdownTime:    ($shutdown_end | todateiso8601)
, workloadEndTime: ($workload_end | todateiso8601)
, earliestEndTime: ($earliest_end | todateiso8601)
};

def timing_pretty_describe($t):
  [ "  - future offset:         \($t.future_offset) (of which \($t.extra_future_offset) is extra)"
  , "  - start time:            \($t.systemStart)"
  , "  - shutdown time:         \($t.shutdownTime[:-1])"
  , "  - workload end time:     \($t.workloadEndTime[:-1])"
  , "  - earliest end:          \($t.earliestEndTime[:-1])"
  , ""
  ] | join("\n");

def profile_node_specs($env; $prof; $topology):
  $prof.composition.n_bft_hosts      as $n_bfts
| $prof.composition.n_pool_hosts     as $n_pools
| $prof.composition.n_singular_hosts as $n_singular_pools
| ([range(0;
          $n_bfts)]
   | map({ i: .
         , kind: "bft"
         , pools: 0
         , autostart: true
         }))
   as $bfts
| ([range($n_bfts;
          $n_bfts + $n_pools)]
   | map({ i: .
         , kind: "pool"
         , pools: (if . - $n_bfts < $n_singular_pools
                   then 1
                   else $prof.composition.dense_pool_density end)
         , autostart: true
         }))
   as $pools
| ([range($n_bfts + $n_pools;
          $n_bfts + $n_pools +
          if $prof.composition.with_proxy then 1 else 0 end)]
   | map({ i: .
         , kind: "proxy"
         , pools: 0
         , autostart: true
         }))
   as $proxies
| ([range($n_bfts + $n_pools
          + if $prof.composition.with_proxy then 1 else 0 end;
          $n_bfts + $n_pools
          + if $prof.composition.with_proxy then 1 else 0 end
          + if $prof.composition.with_chaindb_server then 1 else 0 end)]
   | map({ i: .
         , kind: "chaindb-server"
         , pools: 0
         , autostart: true
         }))
   as $chaindbs
| ([range($n_bfts + $n_pools
          + if $prof.composition.with_proxy then 1 else 0 end
          + if $prof.composition.with_chaindb_server then 1 else 0 end;
          $n_bfts + $n_pools
          + if $prof.composition.with_proxy then 1 else 0 end
          + if $prof.composition.with_chaindb_server then 1 else 0 end
          + if $prof.composition.with_explorer then 1 else 0 end)]
   | map({ i: .
         , kind: "explorer"
         , pools: 0
         , autostart: false
         }))
   as $explorers

## For each non-explorer node spec:
| ($bfts + $pools + $proxies + $chaindbs
   | map(. +
         { name:       "node-\(.["i"])"
         , isProducer: ([.kind == "bft", .kind == "pool"] | any)
         , port:
           (if $env.staggerPorts
            then $env.basePort + .i
            else $env.basePort
            end)
         , region:
           ( .i as $i
             | $topology[ 0 ].coreNodes
             | map( select( .nodeId == $i ) )[0]
             | .region
           )
         }))
  +
## For each explorer node spec:
  ($explorers
   | map(. +
         { name:       "explorer"
         , isProducer: false
         , port:
           (if $env.staggerPorts
            then $env.basePort + .i
            else $env.basePort
            end)
         , region:
           ( .i as $i
             | $topology[ 0 ].relayNodes
             | map( select( .nodeId == $i ) )[0]
             | .region
           )
         }))
## For each node spec:
| map(. +
      { shutdown_on_slot_synced:  $prof.node.shutdown_on_slot_synced
      , shutdown_on_block_synced: $prof.node.shutdown_on_block_synced
      }
      )
| map({ key: .name, value: .})
| from_entries;
