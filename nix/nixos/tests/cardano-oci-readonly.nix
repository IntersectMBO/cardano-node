{
  pkgs,
  dockerImage,
  tracerDockerImage,
  submitApiDockerImage,
  ...
}: let
  inherit (pkgs) lib;

  # Image refs as `docker load` will tag them (repoName:gitrev).
  imageRef = "${dockerImage.imageName}:${dockerImage.imageTag}";
  tracerImageRef = "${tracerDockerImage.imageName}:${tracerDockerImage.imageTag}";
  submitApiImageRef = "${submitApiDockerImage.imageName}:${submitApiDockerImage.imageTag}";

  network = "preview";

  # Harmless merge keys, present only to exercise merge mode which is what
  # triggers the /tmp runtime dir + relative-path rewriting. The double quotes
  # are backslash-escaped because these are interpolated into double-quoted
  # Python string literals in the testScript below.
  configMerge = ''{\"MaxConcurrencyBulkSync\":2}'';
  topologyMerge = ''{\"useLedgerAfterSlot\":1}'';
  tracerConfigMerge = ''{\"verbosity\":\"Minimum\"}'';

  rt = "/tmp/cardano-node";
  tracerRt = "/tmp/cardano-tracer";
in {
  name = "cardano-oci-readonly-test";

  nodes = {
    machine = {...}: {
      nixpkgs.pkgs = pkgs;

      # Room for loading three images (node, tracer, submit-api) + overlay +
      # a starting node/tracer.
      virtualisation.diskSize = 10240;
      virtualisation.memorySize = 4096;
      virtualisation.docker.enable = true;
    };
  };

  # Like the other tests here, this is sandboxed: the node cannot sync, but it
  # starts and resolves its configuration, which is all these assertions need.
  testScript = ''
    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.wait_until_succeeds("docker info", timeout=120)
    machine.succeed("docker load -i ${dockerImage}")

    # Read-only root + non-root UID in group 0 + merge mode. Asserts the
    # container starts as read-only and group-0 volume writes work, creates its
    # fixed 0700 runtime dir, writes the merged config with absolute genesis
    # paths, writes the env snapshot, and the /usr/local/bin/env alias sources
    # it.
    machine.succeed(
        "docker run -d --name n1 --read-only --tmpfs /tmp --user 1000:0 "
        "-v n1data:/data -v n1ipc:/ipc -v n1logs:/logs "
        "-e NETWORK=${network} "
        "-e CARDANO_CONFIG_JSON_MERGE='${configMerge}' "
        "-e CARDANO_TOPOLOGY_JSON_MERGE='${topologyMerge}' "
        "${imageRef}"
    )

    # The runtime artifacts are written by the entrypoint/run-node before the
    # node execs, so they appear shortly after start. Waiting on the env
    # snapshot also confirms the container did not crash under --read-only and
    # --user since `docker exec` fails against an exited container.
    machine.wait_until_succeeds("docker exec n1 test -f ${rt}/env", timeout=60)
    machine.succeed("[ \"$(docker inspect -f '{{.State.Running}}' n1)\" = true ]")

    machine.succeed("docker exec n1 test -d ${rt}")
    machine.succeed("docker exec n1 sh -c '[ \"$(stat -c %a ${rt})\" = 700 ]'")
    machine.succeed("docker exec n1 test -f ${rt}/config-merged.json")
    machine.succeed("docker exec n1 test -f ${rt}/topology-merged.json")

    # The merged config's relative "*File" references (ByronGenesisFile,
    # ShelleyGenesisFile, ..., CheckpointsFile) must be rewritten to absolute
    # paths so they resolve from /tmp. Mirror the entrypoint's predicate -- any
    # top-level key ending in "File" with a string value -- and assert all are
    # absolute. Use jq from the container.
    machine.succeed(
        "docker exec n1 jq -e "
        "'[to_entries[]|select(.key|endswith(\"File\"))|.value|select(type==\"string\")] as $v "
        "| ($v|length>0) and ($v|all(startswith(\"/\")))' "
        "${rt}/config-merged.json"
    )

    # The merged topology's relative peerSnapshotFile must likewise be rewritten
    # to an absolute path.
    machine.succeed(
        "docker exec n1 jq -e "
        "'(.peerSnapshotFile|type==\"string\") and (.peerSnapshotFile|startswith(\"/\"))' "
        "${rt}/topology-merged.json"
    )

    # The env snapshot is POSIX-sourceable; use sh and `.` to load it and
    # confirm it populated the env.
    machine.succeed(
        "docker exec --user 1000:0 n1 sh -c "
        "'. /usr/local/bin/env && [ -n \"$CARDANO_CONFIG\" ]'"
    )
    machine.succeed("docker rm -f n1")

    # Read-only root WITHOUT a writable /tmp must fail fast with guidance.
    status, out = machine.execute(
        "docker run --rm --read-only -e NETWORK=${network} ${imageRef} 2>&1"
    )
    assert status != 0, "expected non-zero exit when /tmp is not writable"
    assert "/tmp is not writable" in out, f"missing actionable /tmp error; got: {out}"

    # Cli mode must NOT require a writable /tmp (read-only, no tmpfs).
    machine.succeed("docker run --rm --read-only ${imageRef} cli version")

    # Tracer image: same read-only + non-root + merge-mode behavior as the
    # node, minus the genesis/topology rewrites as tracer config has no
    # relative file references.
    machine.succeed("docker load -i ${tracerDockerImage}")
    machine.succeed(
        "docker run -d --name t1 --read-only --tmpfs /tmp --user 1000:0 "
        "-v t1data:/data -v t1ipc:/ipc -v t1logs:/logs "
        "-e NETWORK=${network} "
        "-e CARDANO_CONFIG_JSON_MERGE='${tracerConfigMerge}' "
        "${tracerImageRef}"
    )
    # Give the entrypoint time to write artifacts and exec the tracer. If the
    # container exits early, surface its logs rather than failing later with an
    # opaque `docker exec` timeout against a dead container.
    machine.sleep(10)
    if machine.succeed("docker inspect -f '{{.State.Running}}' t1").strip() != "true":
        _, logs = machine.execute("docker logs t1 2>&1")
        raise Exception("tracer container exited early; docker logs:\n" + logs)
    machine.succeed("docker exec t1 sh -c '[ \"$(stat -c %a ${tracerRt})\" = 700 ]'")
    machine.succeed("docker exec t1 test -f ${tracerRt}/tracer-config-merged.json")
    machine.succeed("docker exec t1 test -f ${tracerRt}/env")
    machine.succeed(
        "docker exec --user 1000:0 t1 sh -c "
        "'. /usr/local/bin/env && [ -n \"$CARDANO_CONFIG\" ]'"
    )
    machine.succeed("docker rm -f t1")

    # Submit-api image: stateless (no env snapshot / merge / state writes),
    # so it only needs to execute under --read-only and as non-root.
    machine.succeed("docker load -i ${submitApiDockerImage}")
    machine.succeed("docker run --rm --read-only --user 1000:0 ${submitApiImageRef} --help >/dev/null")
  '';
}
