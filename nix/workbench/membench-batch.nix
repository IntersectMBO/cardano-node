{ lib, bash, jq, zstd, runCommand
, membench-run, node-measured-rev
, node-process
, variantTable, nIterations ? 5
}:

with lib;
let
  ## From the variant table (Map Name RtsFlags), derive variants of the baseline:
  allVariants =
    mapAttrs
      (name: args:
        membench-run.override
        (args // {
          suffix   = "-${name}";
        }))
      variantTable;

  ## For a given variant, derive its run iterations:
  variantIterationsShell =
    name: variant:
    concatMapStringsSep "\n"
      (currentIteration:
        "ln -sv ${variant.override { inherit currentIteration; }} $out/${name}-${toString currentIteration}")
      (range 1 nIterations);

  ## Derive the (variants X iterations) cross product:
  allVariantIterationsShell =
    builtins.attrValues
      (mapAttrs variantIterationsShell allVariants);

  nVariants = length (__attrNames variantTable);
  batch-id  = "${node-measured-rev}-${toString nVariants}vars-${toString nIterations}runs";

in runCommand "membench-${batch-id}" {
  requiredSystemFeatures = [ "benchmark" ];
  preferLocalBuild = true;
  nativeBuildInputs = [ jq zstd ];
  passthru = {
    inherit batch-id variantTable nIterations;
  };
} ''
  mkdir -p $out/nix-support

  ## 0. link the runs
  ${concatStringsSep "\n" allVariantIterationsShell}

  ## 1. index the linked runs
  process_args=(
    --no-progress
    collect ${batch-id} membenches_v1 $out
    )
  ${bash}/bin/bash ${node-process}/bench/process/process.sh ''${process_args[*]} \
    > $out/index.json

  ## 2. Package
  cd $out

  tarname=membench-${batch-id}
  tar -cf $tarname.tar.zst \
     index.json         \
     */*.json           \
     */input/*.json     \
     */input/highwater  \
     */input/rts.dump   \
     */input/stderr     \
     --zstd

  ## 3. Mark for Hydra publishing
  cat > nix-support/hydra-build-products <<EOF
  file binary-dist $out/$tarname.tar.zst
  EOF
''
