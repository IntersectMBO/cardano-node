{ runCommand
, jq
, eras ?
  [ "shelley"
    "allegra"
    "mary"
  ]
, ...
}:

runCommand "cluster-profiles.json" { buildInputs = [ jq ]; } ''
    jq --argjson eras '${__toJSON eras}' '
      include "profiles" { search: "${./.}" };

      $eras
      | map(profiles(.; null; null; []))
      | add
      ' --null-input > $out
  ''
