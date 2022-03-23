{ runCommand, lib
, jq, strace, util-linux, e2fsprogs, gnugrep, procps, time, hexdump
## Code
, input, node-snapshot, node-measured
## Parameters
, snapshot
, rtsflags, rtsMemSize
## Iteration & nomenclature
, currentIteration ? null
, suffix ? ""
}:

let
  flags = "${rtsflags} ${lib.optionalString (rtsMemSize != null) "-M${rtsMemSize}"}";
  topology = { Producers = []; };
  topologyPath = builtins.toFile "topology.json" (builtins.toJSON topology);
  inVM = false;
  ident = "node-${input.node-measured.rev}${suffix}-iter${toString currentIteration}";
in
  runCommand "membench-run-${ident}" {
    buildInputs = [ node-measured hexdump jq strace util-linux procps time ];
    inherit currentIteration;
    requiredSystemFeatures = [ "benchmark" ];
  } ''
  mkdir -pv $out/nix-support
  set -x

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
  jq '.setupScribes = [
      .setupScribes[0] * { "scFormat":"ScJson" },
      {
        scFormat:"ScJson",
        scKind:"FileSK",
        scName:"log.json",
        scRotation:{
          rpLogLimitBytes: 300000000,
          rpMaxAgeHours:   24,
          rpKeepFilesNum:  20
        }
      }
    ]
    | .defaultScribes =
      .defaultScribes + [ [ "FileSK", "log.json" ] ]
    | .options.mapBackends =
       { "cardano.node.resources": [ "KatipBK" ] }
    '   ${node-measured}/configuration/cardano/mainnet-config.json > config.json
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
  ln -s ${node-measured.packages.x86_64-linux.cardano-node}/bin/cardano-node .

  command time -f %M -o $out/highwater \
    ./cardano-node "''${args[@]}" || true
  test -f log.json || {
    echo "FATAL: cardano-node did not create log.json"
    exit 1
  }

  pwd
  df -h
  free -m

  egrep 'cardano\.node\.resources|ReplayFromSnapshot|ReplayedBlock|will terminate|Ringing the node shutdown|TookSnapshot' log.json > $out/summary.json

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
                    .[:-4] + "Z" | fromdateiso8601;
                  .
                  | map(.at | katip_timestamp_to_iso8601)
                  | .[1] - .[0]
                 ' --slurp)
  highwater=$(cat highwater | cut -d' ' -f6)

  jq '
    def minavgmax:
        length as $len
      | { min: (min/1024/1024)
        , avg: ((add / $len)/1024/1024)
        , max: (max/1024/1024)
        };

      map(select(.ns[0] == "cardano.node.resources") | .data)
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
