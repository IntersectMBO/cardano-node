{ pkgs, profileName, profileJson }:
  pkgs.runCommand "workbench-topology-${profileName}"
    { requiredSystemFeatures = [ "benchmark" ];
      nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
        [ bash cardano-cli coreutils gnused jq moreutils workbench.workbench ];
    }
    ''
    mkdir $out
    wb topology make ${profileJson} $out
    ''
