# The CI cluster run, and nothing else: create the genesis, start the cluster,
# analyse what came out. Named for what builds it: Hydra, as `workbench-ci-test`
# in flake.nix, which imports this file and nothing else.
#
# The bundle reading it needs is ./shell.nix's `runner`: that file holds the
# reader and the dev shell together because the two take the same parameters,
# and both of its members are lazy, so taking one here does not build the other.
# This file only adds the three derivations that are the smoke test, so its
# value IS the run, with the analysis attached.
{ pkgs
, haskellProject     # No sensible default: whose executables to run.
, profileName        ? "default"
, eraName            ? "conway"
, backendName        ? "supervisor"
, stateDir           ? "run/current"
, batchName          ? "undefined"
, workbenchStartArgs ? []
, profiling          ? {}
, cardano-node-rev   ? pkgs.gitrev or "0000000000000000000000000000000000000000"
, cardanoNodeVersion ? null
}:

let

  inherit (pkgs) lib;

  runner =
    (import ./shell.nix
      { inherit pkgs;
        project = haskellProject;
        inherit profileName eraName backendName stateDir batchName;
        useCabalRun = false;
        inherit workbenchStartArgs profiling cardanoNodeVersion;
      }
    ).runner
  ;
  inherit (runner) materialised workbench-envars wbPath basePort;

  ## A run in a build sandbox has no PATH but this one, and the bundle already
  ## says what it needs: `workbenchTools` is the whole of it, resolved. Nothing
  ## is listed twice here, so nothing here can fall behind the data -- `zstd`
  ## included, which the bundle declares.
  workbench-deps-hydra = [ runner.wb ] ++ runner.workbenchTools;

  profileJson = "${materialised}/profile/profile.json";

  # Genesis files creation.
  genesisFiles = pkgs.runCommand "workbench-run-genesis-${profileName}"
    { requiredSystemFeatures = [ "benchmark" ];
      nativeBuildInputs = workbench-deps-hydra;
    }
    ''
    # wb is a bare directory, not a bin/ derivation: PATH by hand.
    export PATH=${wbPath}
    mkdir $out

    wb genesis create-cache \
      "${profileJson}"      \
      "$out"
    ''
  ;

  run = pkgs.runCommand "workbench-run-${profileName}-${backendName}"
    { requiredSystemFeatures = [ "benchmark" ];
      nativeBuildInputs = workbench-deps-hydra;
    }
    ''
    mkdir -p    $out/{cache,nix-support}
    cd          $out
    export HOME=$out

    ${workbench-envars}

    # cardano-node rejects vrf.skey if any "other" perm bit is set, but
    # Nix forces every store file to 0444. Copy out and chmod go= so the
    # run dir's symlinks resolve to 0600.
    genesis_cache_entry=$out/cache/genesis-cache-entry
    cp -rL ${genesisFiles} "$genesis_cache_entry"
    chmod -R u+rwX,go= "$genesis_cache_entry"

    cmd=(
      wb
      start
      --profile-data        ${materialised}/profile
      --backend-data        ${materialised}/backend
      --genesis-cache-entry "$genesis_cache_entry"
      --batch-name          smoke-test
      --era-name            ${eraName}
      --base-port           ${toString basePort}
      --node-source         ${haskellProject.args.src}
      --node-rev            ${cardano-node-rev}
      --cache-dir           ./cache
     ${builtins.concatStringsSep " " workbenchStartArgs}
    )
    echo "''${cmd[*]}" > $out/wb-start.sh

    time "''${cmd[@]}" 2>&1 |
      tee $out/wb-start.log
    status=$?
    if test $status != 0
    then echo "wb start failed"
         cd run/current
         echo "==========  txgen  stdout:"; cat generator/stdout || true
         echo "==========  txgen  stderr:"; cat generator/stderr || true
         echo "==========  node-0 stdout:"; cat node-0/stdout || true
         echo "==========  node-0 stderr:"; cat node-0/stderr || true
         wb call fail "wb start failed"
    fi

    ## Convert structure from $out/run/RUN-ID/* to $out/*:
    rm -rf cache
    rm -f run/{current,-current}
    find $out -type s | xargs rm -f
    run=$(cd run; ls)
    (cd run; tar c $run --zstd) > archive.tar.zst
    mv       run/$run/*  .
    rm -rf   run/$run    run

    cat > $out/nix-support/hydra-build-products <<EOF
    report workbench-log   $out wb-start.log
    report meta            $out meta.json
    $(for n in $(jq -r 'keys[]' ${materialised}/profile/node-specs.json)
      do echo "report $n-log $out $n/stdout"; done)
    report archive-tar-zst $out archive.tar.zst
    EOF

    echo "workbench-test:  completed run $run"
    ''
  ;

  # Analyze the run.
  trace = false;
  analysis = pkgs.runCommand "workbench-run-analysis-${profileName}"
    { requiredSystemFeatures = [ "benchmark" ];
      nativeBuildInputs = workbench-deps-hydra;
    }
    ''
    export PATH=${wbPath}
    mkdir -p $out/nix-support

    echo "analysing run:  ${run}"

    ln -s ${run} $out/run

    cmd=(
        wb
        ${lib.optionalString trace "--trace"}
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
    ''
  ;

in

  run // { inherit analysis; }

