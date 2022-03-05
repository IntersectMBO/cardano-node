{ pkgs }:

{ profileNix, topology }:
pkgs.runCommand "workbench-profile-genesis-cache-${profileNix.name}"
  { requiredSystemFeatures = [ "benchmark" ];
    nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
      [ bash cardano-cli coreutils gnused jq moreutils workbench ];
  }
  ''
  mkdir $out
  args=(
     genesis actually-genesis
     ${profileNix.JSON}
     ${topology}
     ${out}
  )
  time wb ''${args[@]}

  ln -s ${profile} $out/profile
  ''
