{ pkgs }:

{ profileNix, profile, topology }:
pkgs.runCommand "workbench-profile-genesis-cache-${profileNix.name}"
  { requiredSystemFeatures = [ "benchmark" ];
    nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
      [ bash cardano-cli coreutils gnused jq moreutils workbench.workbench ];
  }
  ''
  mkdir $out

  cache_key_input=$(wb genesis profile-cache-key-input ${profileNix.JSON})
  cache_key=$(wb genesis profile-cache-key ${profileNix.JSON})

  args=(
     genesis actually-genesis
     ${profileNix.JSON}
     ${topology}
     $out
     "$cache_key_input"
     "$cache_key"
  )
  time wb ''${args[@]}

  ln -s ${profile} $out/profile
  ''
