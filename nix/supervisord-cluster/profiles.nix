{ runCommand
, jq
, ...
}:
let
  profilesJSON = runCommand "profiles" { buildInputs = [ jq ]; } ''
    jq --null-input '
      include "profiles" { search: "${./.}" };
      profiles("shelley"; null; null)
      ' > $out
  '';
in
  __fromJSON (__readFile profilesJSON)
