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
  #
  # The node config merge is section qualified because the published config is
  # in the cardano-config envelope. A merge is a deep merge with no key
  # relocation, so the flat spelling would add a key outside Configuration that
  # the node does not resolve, which is the shape the docker readme warns
  # about. Topology and tracer configs are not enveloped and stay flat.
  configMerge = ''{\"Configuration\":{\"Network\":{\"MaxConcurrencyBulkSync\":2}}}'';
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

      # Keep dhcpcd off docker's ephemeral interfaces. Starting and stopping
      # containers churns veth pairs, dhcpcd tries to manage each one, and
      # dhcpcd 10.3.1 segfaults probing one for an IPv4LL address. Harmless to
      # the assertions but it core dumps and buries real errors in the log.
      networking.dhcpcd.denyInterfaces = ["veth*" "docker0"];
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
    # paths so they resolve from /tmp. Mirror the entrypoint's predicate, which
    # walks rather than reading the top level: in the cardano-config envelope
    # these keys sit under Configuration.<Section>, so a top-level scan finds
    # none of them. The length guard is what makes that a failure rather than a
    # vacuous pass, so keep it. Use jq from the container.
    machine.succeed(
        "docker exec n1 jq -e "
        "'[..|objects|to_entries[]|select((.key|endswith(\"File\")) "
        "and (.value|type==\"string\"))|.value] as $v "
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

    # The runtime dir is a fixed path, so two same-role containers sharing one
    # /tmp would otherwise both write it and the last writer would win. Both
    # containers here run as the default root, i.e. the same UID, which is the
    # case the directory ownership check cannot catch on its own.
    machine.succeed(
        "docker run -d --name s1 -v shtmp:/tmp "
        "-v s1data:/data -v s1ipc:/ipc -v s1logs:/logs "
        "-e NETWORK=${network} "
        "-e CARDANO_CONFIG_JSON_MERGE='${configMerge}' "
        "${imageRef}"
    )
    machine.wait_until_succeeds("docker exec s1 test -f ${rt}/env", timeout=60)

    status, out = machine.execute(
        "docker run --rm -v shtmp:/tmp -e NETWORK=${network} ${imageRef} 2>&1"
    )
    assert status != 0, f"expected non-zero exit for a shared /tmp; got: {out}"
    assert "is in use by another container" in out, f"missing lock error; got: {out}"

    # Once the holder is gone the lock is released, so the same volume, now
    # carrying a stale runtime dir, must still be usable. This is what separates
    # the lock from a blanket refusal whenever the directory already exists.
    machine.succeed("docker rm -f s1")
    machine.succeed(
        "docker run -d --name s2 -v shtmp:/tmp "
        "-v s2data:/data -v s2ipc:/ipc -v s2logs:/logs "
        "-e NETWORK=${network} "
        "-e CARDANO_CONFIG_JSON_MERGE='${configMerge}' "
        "${imageRef}"
    )
    machine.wait_until_succeeds("docker exec s2 test -f ${rt}/env", timeout=60)
    machine.succeed("docker rm -f s2")

    # Read-only root WITHOUT a writable /tmp must fail fast with guidance.
    status, out = machine.execute(
        "docker run --rm --read-only -e NETWORK=${network} ${imageRef} 2>&1"
    )
    assert status != 0, "expected non-zero exit when /tmp is not writable"
    assert "/tmp is not writable" in out, f"missing actionable /tmp error; got: {out}"

    # Cli mode must NOT require a writable /tmp (read-only, no tmpfs).
    machine.succeed("docker run --rm --read-only ${imageRef} cli version")

    # Cli mode is dispatched before the NETWORK modes, so a NETWORK left in the
    # environment, with or without a merge var, must neither start a node nor
    # reach the merge block, which writes to the runtime dir cli mode skips.
    machine.succeed(
        "docker run --rm --read-only -e NETWORK=${network} ${imageRef} cli version"
    )
    machine.succeed(
        "docker run --rm --read-only -e NETWORK=${network} "
        "-e CARDANO_CONFIG_JSON_MERGE='${configMerge}' ${imageRef} cli version"
    )

    # Operator-supplied RTS flags are appended after the image's baked +RTS
    # block and merge with it. `-po` covers .prof, .hp and .ticky only, so an
    # eventlog without its own `-ol` is created in the container cwd (/) and the
    # RTS aborts before main under a read-only root. /logs must be a writable
    # mount here or the baked `-t/logs/cardano-node.stats` aborts first, which
    # would pass the exit-code check for the wrong reason.
    status, out = machine.execute(
        "docker run --rm --read-only --tmpfs /tmp -v e0logs:/logs "
        "-e NETWORK=${network} ${imageRef} +RTS -l -RTS 2>&1"
    )
    assert status != 0, f"expected non-zero exit for -l without -ol; got: {out}"
    assert "initEventLogFileWriter" in out, f"expected an eventlog open failure; got: {out}"

    # Given `-ol` under a writable mount it starts and writes the eventlog there.
    machine.succeed(
        "docker run -d --name e1 --read-only --tmpfs /tmp --user 1000:0 "
        "-v e1data:/data -v e1ipc:/ipc -v e1logs:/logs "
        "-e NETWORK=${network} "
        "${imageRef} +RTS -l -ol/logs/cardano-node.eventlog -RTS"
    )
    machine.wait_until_succeeds(
        "docker exec e1 test -s /logs/cardano-node.eventlog", timeout=60
    )
    machine.succeed("docker rm -f e1")

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

    # Graceful shutdown on SIGTERM and SIGINT, for both ways in.
    #
    # Each mode execs the whole way down to cardano-node, so the node is the
    # container's pid 1 and takes the signal itself: scripts mode goes
    # entrypoint -> run-network -> the per-network script -> exec node, custom
    # mode goes entrypoint -> run-node -> exec node. Drop any one of those
    # execs and a shell sits at pid 1 forwarding nothing, docker gives up after
    # the grace period and SIGKILLs, and the exit code is 137. That is what
    # this asserts against rather than a specific clean code, which differs by
    # whether the node handles the signal or dies from it.
    def shutdown_case(name, args, sig):
        # /logs must be a writable mount: the image bakes
        # `-t/logs/cardano-node.stats` into its RTS block, and the node aborts
        # at startup if it cannot open that file under a read-only root.
        machine.succeed(
            "docker run -d --name " + name + " --read-only --tmpfs /tmp "
            "--user 1000:0 -v " + name + "data:/data -v " + name + "ipc:/ipc "
            "-v " + name + "logs:/logs " + args
        )
        # Long enough to be past config resolution and into the node proper,
        # so the signal lands on a running node rather than on startup.
        machine.sleep(10)
        if machine.succeed(
            "docker inspect -f '{{.State.Running}}' " + name
        ).strip() != "true":
            _, logs = machine.execute("docker logs " + name + " 2>&1")
            raise Exception(
                name + " exited before the shutdown signal; docker logs:\n" + logs
            )

        if sig == "TERM":
            # Sends SIGTERM, waits, then SIGKILLs if it outlives the grace period.
            machine.succeed("docker stop -t 30 " + name)
        else:
            # docker kill does not wait, so poll for the exit ourselves.
            machine.succeed("docker kill --signal=INT " + name)
            machine.wait_until_succeeds(
                "[ \"$(docker inspect -f '{{.State.Running}}' " + name + ")\" = false ]",
                timeout=40,
            )

        code = machine.succeed(
            "docker inspect -f '{{.State.ExitCode}}' " + name
        ).strip()
        if code == "137":
            _, logs = machine.execute("docker logs " + name + " 2>&1")
            raise Exception(
                name + ": SIG" + sig + " did not stop the node, docker had to "
                "SIGKILL it after the grace period; docker logs:\n" + logs
            )
        machine.succeed("docker rm -f " + name)

    shutdown_case("sd1", "-e NETWORK=${network} ${imageRef}", "TERM")
    shutdown_case("sd2", "-e NETWORK=${network} ${imageRef}", "INT")
    shutdown_case("sd3", "${imageRef} run", "TERM")
    shutdown_case("sd4", "${imageRef} run", "INT")
  '';
}
