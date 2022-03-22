{ lib, bash, jq, runCommand
, input, node-process
, membench-batch
, membench-batch-results
, node-config-name ? "baseline"
}:

let
  report-id = "${membench-batch.batch-id}-config-${node-config-name}-report-${input.node-process.shortRev}";
in
runCommand "membench-report-${report-id}" {
  requiredSystemFeatures = [ "benchmark" ];
  preferLocalBuild = true;
  buildInputs = [ jq ];
} ''
  echo "membench | report:  generating ${report-id}"

  mkdir -p $out/nix-support

  cd $out

  ln -s ${membench-batch}         batch
  ln -s ${membench-batch-results} batch-results.json

  ${bash}/bin/bash ${node-process}/bench/process/process.sh \
    render-html < ${membench-batch-results} > $out/raw-data.html

  ${bash}/bin/bash ${node-process}/bench/process/process.sh \
    render      < ${membench-batch-results} > $out/report.csv

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
