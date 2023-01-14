{ lib, bash, jq, runCommand
, input, node-process
, batch
, batch-results
, node-config-name ? "baseline"
}:

let
  report-id = "${batch.batch-id}-config-${node-config-name}-report-${input.node-process.shortRev}";
in
runCommand "membench-report-${report-id}" {
  requiredSystemFeatures = [ "benchmark" ];
  preferLocalBuild = true;
  buildInputs = [ jq ];
} ''
  echo "membench | report:  generating ${report-id}"

  mkdir -p $out/nix-support

  cd $out

  ln -s ${batch}         batch
  ln -s ${batch-results} batch-results.json

  ${bash}/bin/bash ${node-process}/bench/process/process.sh \
    render-html < ${batch-results} > $out/raw-data.html

  ${bash}/bin/bash ${node-process}/bench/process/process.sh \
    render      < ${batch-results} > $out/report.csv

  cat > nix-support/hydra-build-products <<EOF
  report raw-data $out raw-data.html
  report testlog  $out report.csv
  EOF

  process_args=(
    --config ${node-config-name}
    )
  ${bash}/bin/bash ${node-process}/bench/process/process.sh \
    render-hydra-charts "''${process_args[@]}" \
      < batch-results.json \
      > $out/nix-support/hydra-metrics
  ''
