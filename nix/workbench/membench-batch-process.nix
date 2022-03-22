{ lib, bash, runCommand, jq
, input
, membench-batch
, node-process
}:

runCommand "membench-results-${membench-batch.batch-id}-process-${input.node-process.shortRev}.json" {
  requiredSystemFeatures = [ "benchmark" ];
  preferLocalBuild = true;
  nativeBuildInputs = [ jq ];
} ''
  echo "membench | process:  processing batch ${membench-batch.batch-id}"

  ${bash}/bin/bash ${node-process}/bench/process/process.sh \
    process < ${membench-batch}/index.json > $out

  cat $out
''
