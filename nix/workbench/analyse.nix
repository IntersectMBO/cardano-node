{ pkgs
, workbench
, profileNix
, run
, trace ? false }:

pkgs.runCommand "workbench-run-analysis-${profileNix.name}"
  { requiredSystemFeatures = [ "benchmark" ];
    nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
      [ bash coreutils gnused jq moreutils nixWrapped workbench.workbench ];
  }
  ''
  echo "analysing run:  ${run}"
  mkdir -p $out/nix-support

  ln -s ${run} $out/run

  cmd=(
      wb
      ${pkgs.lib.optionalString trace "--trace"}
      analyse
      # --filters size-full
      --outdir  $out
      standard
      ${run}
      )
  echo "''${cmd[*]}" > $out/wb-analyse.sh

  ''${cmd[@]} 2>&1 |
      tee $out/wb-analyse.log

  cd $out
  for x in $(ls *.json *.org *.txt | grep -v 'flt\.json$')
  do echo "report $x $out $x" >> $out/nix-support/hydra-build-products
  done
  EOF
  ''
