def profile_timing($prof;
                   $future_offset;
                   $start;
                   $start_human;
                   $start_tag;
                   $systemStart):
  ($systemStart | fromdateiso8601 | . + $prof.derived.shutdown_time)      as $shutdown_end
| ($systemStart | fromdateiso8601 | . + $prof.derived.generator_duration) as $workload_end
| ( [$shutdown_end, $workload_end]
  | map(select(. != null))
  | min)                                                                as $earliest_end
|
{ future_offset:   $future_offset
, start:           $start
, shutdown_end:    $shutdown_end
, workload_end:    $workload_end
, earliest_end:    $earliest_end

, start_tag:       $start_tag
, start_human:     $start_human
, systemStart:     $systemStart
, shutdownTime:    ($shutdown_end | todateiso8601)
, workloadEndTime: ($workload_end | todateiso8601)
, earliestEndTime: ($earliest_end | todateiso8601)
};

def timing_pretty_describe($t):
  [ "  - future offset:      \($t.future_offset)"
  , "  - start time:         \($t.systemStart)"
  , "  - shutdown time:      \($t.shutdownTime[:-1])"
  , "  - workload time:      \($t.workloadEndTime[:-1])"
  , "  - earliest end:       \($t.earliestEndTime[:-1])"
  , ""
  ] | join("\n");

def profile_node_specs($env; $prof):
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
          + if $prof.composition.with_observer then 1 else 0 end)]
   | map({ i: .
         , kind: "observer"
         , pools: 0
         , autostart: false
         }))
   as $observers
| ($bfts + $pools + $proxies + $chaindbs + $observers
   | map(. +
         { name:       "node-\(.["i"])"
         , isProducer: ([.kind == "bft", .kind == "pool"] | any)
         , port:
           (if $env.staggerPorts
            then $env.basePort + .i
            else $env.basePort
            end)
         }))
| map({ key: .name, value: .})
| from_entries;
