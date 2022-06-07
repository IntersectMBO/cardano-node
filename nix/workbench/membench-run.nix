{ runCommand, system, lib
, jq, yq, strace, util-linux, e2fsprogs, gnugrep, procps, time, hexdump
## Code
, input, node-snapshot, node-measured, node-measured-rev
## Parameters
, snapshot
, rtsflags, rtsMemSize
, legacyTracing ? true
## Iteration & nomenclature
, currentIteration ? null
, suffix ? ""
}:

let
  flags = "${rtsflags} ${lib.optionalString (rtsMemSize != null) "-M${rtsMemSize}"}";
  topology = { Producers = []; };
  topologyPath = builtins.toFile "topology.json" (builtins.toJSON topology);
  inVM = false;
  ident = "node-${node-measured-rev}${suffix}-iter${toString currentIteration}";
in
  runCommand "membench-run-${ident}" {
    buildInputs = [ node-measured hexdump jq strace util-linux procps time yq ];
    inherit currentIteration;
    requiredSystemFeatures = [ "benchmark" ];
  } ''
  mkdir -pv $out/nix-support

  ${lib.optionalString inVM ''
  echo 0 > /tmp/xchg/in-vm-exit

  # never overcommit
  echo 2 > /proc/sys/vm/overcommit_memory
  ''}

  pwd
  free -m
  cp -r ${snapshot}/chain chain
  chmod -R +w chain

  echo ${flags}

  ls -ltrh chain
  ${if legacyTracing
    then
      ''
        jq '
          .setupScribes =
          [ .setupScribes[0] * { "scFormat":"ScJson" }
          , { scFormat:"ScJson"
            , scKind:"FileSK"
            , scName:"log.json"
            , scRotation:
              { rpLogLimitBytes: 300000000
              , rpMaxAgeHours:   24
              , rpKeepFilesNum:  20
              }
            }
          ]
        | .defaultScribes =
            .defaultScribes + [ [ "FileSK", "log.json" ] ]
        | .options.mapBackends =
            { "cardano.node.resources": [ "KatipBK" ] }
        | delpaths([["options","mapSubtrace"]])
        ' ${node-measured}/configuration/cardano/mainnet-config.json > config.json
      ''
    else
      ''
        yq '
        .
        | .TraceOptionResourceFrequency = 1000
        ' ${node-measured}/configuration/cardano/mainnet-config-new-tracing.yaml > config.json
      ''}
  cp -v ${node-measured}/configuration/cardano/*-genesis.json .

  args=( +RTS -s$out/rts.dump
              ${flags}
         -RTS
         run
         --database-path           chain/
         --config                  config.json
         --topology                ${topologyPath}
         --shutdown-on-slot-synced 200000
       )

  # so the node wont get GC'd, and you could confirm the source it came from
  ln -s ${node-measured.packages.${system}.cardano-node}/bin/cardano-node .

  command time -f %M -o $out/highwater \
    ./cardano-node "''${args[@]}" ${if legacyTracing
                                    then ""
                                    else " | grep '^{' > log.json"
                                   } || true
  test -f log.json || {
    echo "FATAL: cardano-node did not create log.json"
    exit 1
  }

  pwd
  df -h
  free -m

  egrep '[Cc]ardano\.[Nn]ode\.[Rr]esources|ReplayFromSnapshot|ReplayedBlock|will terminate|Ringing the node shutdown|TookSnapshot' log.json > $out/summary.json

  mv -vi log*json config.json $out/

  ln -s ${snapshot} $out/chaindb
  args=( --arg             measuredNodeRev  ${input.node-measured.rev}
       )
  jq '{ measuredNodeRev:  $measuredNodeRev
      }
     ' "''${args[@]}" > $out/run-info.json

  cd $out

  totaltime=$({ head -n1 log.json
                tail -n1 log.json
              } |
              jq 'def katip_timestamp_to_iso8601:
                    ${if legacyTracing
                      then ".[:-4] + \"Z\""
                      else ".[:10] + \"T\" + .[11:19] + \"Z\""}
                     | fromdateiso8601;
                  .
                  | map(.at | katip_timestamp_to_iso8601)
                  | .[1] - .[0]
                 ' --slurp)
  highwater=$(cat highwater | cut -d' ' -f6)

  cat summary.json
  jq '
    def minavgmax:
        length as $len
      | { min: (min/1024/1024)
        , avg: ((add / $len)/1024/1024)
        , max: (max/1024/1024)
        };

      map(select(${if legacyTracing
                   then ".ns[0] == \"cardano.node.resources\""
                   else ".ns    == \"Resources\""}) | .data)
    | { RSS:          map(.RSS) | minavgmax
      , Heap:         map(.Heap) | minavgmax
      , CentiCpuMax:  map(.CentiCpu) | max
      , CentiMutMax:  map(.CentiMut) | max
      , SecGC:       (map(.CentiGC) | max / 100)
      , CentiBlkIO:   map(.CentiBlkIO) | max
      , flags:       "${flags}"
      , chain:       { startSlot: ${toString snapshot.snapshotSlot}
                     , stopFile:  ${toString snapshot.finalChunkNo}
                     }
      , totaltime:   '$totaltime'
      , pass:        true
      }' --slurp summary.json > refined.json
''
