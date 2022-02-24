{ profile
, workbench
}:

{ pkgs, run, trace ? false }:

let profile = { name = "stub"; }; #__fromJSON (__readFile "${run}/profile.json");
in
pkgs.runCommand "workbench-run-analysis-${profile.name}"
  { requiredSystemFeatures = [ "benchmark" ];
    nativeBuildInputs = with pkgs.haskellPackages; with pkgs; [
      bash
      bech32
      coreutils
      gnused
      jq
      moreutils
      nixWrapped
      psmisc
      python3Packages.supervisor
      workbench
    ];
  }
  ''
  echo "analysing run:  ${run}"
  mkdir -p $out/nix-support

  ln -s ${run} $out/run

  args=(
      ${pkgs.lib.optionalString trace "--trace"}
      analyse
      # --filters size-full
      --outdir  $out
      standard
      ${run}
       )
  echo "wb ''${args[*]}" > $out/wb-invocation.sh

  wb ''${args[@]}

  cat > $out/nix-support/hydra-build-products <<EOF
  report block-propagation $out block-propagation.txt
  report node-1-timeline   $out logs-node-1.timeline.txt
  EOF
  ''
