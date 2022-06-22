{ pkgs }:

{ profile }:
pkgs.runCommand "workbench-profile-genesis-cache-${profile.name}"
  { requiredSystemFeatures = [ "benchmark" ];
    nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
      [ bash cardano-cli coreutils gnused jq moreutils workbench.workbench ];
  }
  ''
  mkdir $out

  cache_key_input=$(wb genesis profile-cache-key-input ${profile}/profile.json)
  cache_key=$(      wb genesis profile-cache-key       ${profile}/profile.json)

  genesis_keepalive() {
      while test ! -e $out/profile; do echo 'genesis_keepalive for Hydra'; sleep 10s; done
  }
  genesis_keepalive &
  __genesis_keepalive_pid=$!
  __genesis_keepalive_termination() {
      kill $__genesis_keepalive_pid 2>/dev/null || true
  }
  trap __genesis_keepalive_termination EXIT

  args=(
     genesis actually-genesis
     ${profile}/profile.json
     ${profile}/node-specs.json
     $out
     "$cache_key_input"
     "$cache_key"
  )
  time wb "''${args[@]}"

  touch done

  ln -s ${profile} $out/profile
  ''
