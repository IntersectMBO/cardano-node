#!/usr/bin/env bash

hostmap=$(jq '
  .public_ip
  | values
  | map({ "\(.hostname)": .public_ip})
  | add' last-meta.json)

echo 'obtaining latency matrix for the hostmap:'
jq . -C <<<$hostmap

nixops ssh-for-each --parallel -- "
   self=\$(hostname)

  function probe() {
      local host=\$1 ip=\$2

      ping -qAc21 \$ip                       |
      grep 'rtt\|transmitted'                |
      sed 's_, \|/_\n_g'                     |
      sed 's_ packets transmitted\| received\| packet loss\|time \|rtt min\|avg\|max\|mdev = \|ipg\|ewma \| ms\|ms\|%__g'         |
      grep -v '^$'                           |
      jq '{ source: \"'\$self'\"
          , target: \"'\$host'\"

          , sent:          .[0]
          , received:      .[1]
          , percents_lost: .[2]
          , duration_ms:   .[3]
          , ipg:           .[8]
          , ewma:          .[9]

          , rtt: { min: .[4], avg: .[5], max: .[6], mdev: .[7] }
          }' --slurp --compact-output
   }

   hostmap=${hostmap@Q}

   for host in \$(jq 'keys | .[]' <<<\$hostmap --raw-output)
   do ip=\$(jq '.[\$host]' --arg host \$host <<<\$hostmap --raw-output)
      probe \$host \$ip\ &
   done"
