{ pkgs }:

{ profileNix, profile, workbench }:
pkgs.runCommand "workbench-topology-${profileNix.name}"
  { requiredSystemFeatures = [ "benchmark" ];
    nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
      [ bash cardano-cli coreutils gnused jq moreutils workbench ];
  }
  ''
  mkdir $out
  args=(
     topology make
     ${profileNix.JSON}
     $out
  )
  time wb ''${args[@]}

  ln -s ${profile} $out/profile
  ''
