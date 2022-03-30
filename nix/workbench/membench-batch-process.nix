{ lib, bash, runCommand, jq
, input
, batch
, node-process
}:

runCommand "membench-results-${batch.batch-id}-process-${input.node-process.shortRev}.json" {
  requiredSystemFeatures = [ "benchmark" ];
  preferLocalBuild = true;
  nativeBuildInputs = [ jq ];
} ''
  echo "membench | process:  processing batch ${batch.batch-id}"

  ${bash}/bin/bash ${node-process}/bench/process/process.sh \
    process < ${batch}/index.json > $out

  cat $out
''
