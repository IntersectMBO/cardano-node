############################################################################
# Docker image builder for cardano-node
#
# To build and load into the Docker engine:
#
#   nix build .#dockerImage/node
#   docker load -i result
#
# Include `-L` in the nix build command args to see build logs.
#
# See the nix/docker/README.md file for details on modes of operation.
############################################################################

{ pkgs
, commonLib
, dockerTools

# The main contents of the image.
, cardano-cli
, cardano-node
, db-analyser
, db-synthesizer
, db-truncater
, snapshot-converter
, scripts

# Optional: the headless ghc-debug `cardano-debug` exe (a fully-static musl
# build). When non-null, it and a `take-snapshot` helper are installed, and the
# image exports GHC_DEBUG_SOCKET so the bundled (ghc-debug-instrumented) node
# can be snapshotted. Copied (not symlinked) so the one binary both runs
# in-container and `docker cp`'s out to another host (e.g. to snapshot against
# an ssh-forwarded GHC_DEBUG_SOCKET) with no glibc / Nix-store deps. Used by the
# `dockerImageNodeDebug` image only; null for normal images.
, cardano-debug ? null

# Set gitrev to null, to ensure the version below is used
, gitrev ? null

# Other things to include in the image.
, bashInteractive
, cacert
, coreutils
, curl
, glibcLocales
, iana-etc
, iproute2
, iputils
, jq
, socat
, utillinux
, lib
, exe
, script
, repoName ? "ghcr.io/intersectmbo/${exe}"
}:

let
  inherit (lib) concatStringsSep escapeShellArgs getAttrs mapAttrsToList;

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      pathsToLink = ["/"];
      paths = [
        cardano-cli       # Provide cardano-cli capability
        bashInteractive   # Provide the BASH shell
        cacert            # X.509 certificates of public CA's
        coreutils         # Basic utilities expected in GNU OS's
        curl              # CLI tool for transferring files via URLs
        glibcLocales      # Locale information for the GNU C Library
        iana-etc          # IANA protocol and port number assignments
        iproute2          # Utilities for controlling TCP/IP networking
        iputils           # Useful utilities for Linux networking
        jq                # Lightweight and flexible command-line JSON processor
        socat             # Utility for bidirectional data transfer
        utillinux         # System utilities for Linux
      ];
    };

    # Set up /tmp (override with TMPDIR variable)
    extraCommands = ''
      mkdir -m 0777 tmp
    '';
  };

  # For "script" mode, generate scripts for iohk-nix networks which can be
  # utilized by setting the environment NETWORK variable to the desired
  # network in the docker command: `-e NETWORK <network>`
  clusterStatements = concatStringsSep "\n" (mapAttrsToList (env: scripts: let
    scriptBin = scripts.${script};
    in ''
      elif [[ "$NETWORK" == "${env}" ]]; then
        exec ${scriptBin}/bin/${scriptBin.name} $@
    '') scripts);

  runNetwork = pkgs.writeShellScriptBin "run-network" ''
    if [[ -z "$NETWORK" ]]; then
      echo "[Error] Cannot obtain NETWORK env variable"
      exit 1
    ${clusterStatements}
    else
      echo "[Error] Managed configuration for network "$NETWORK" does not exist"
      exit 1
    fi
  '';

  # ghc-debug: the unix socket the instrumented node serves on, and a small
  # operator helper to capture a heap snapshot and report where it landed.
  # Capture is the only thing that runs in the field; all analysis is done
  # later, offline, against the shipped-back snapshot file.
  ghcDebugSocket = "/ipc/ghc-debug.socket";

  takeSnapshot = pkgs.writeShellScriptBin "take-snapshot" ''
    set -euo pipefail
    SOCK="''${GHC_DEBUG_SOCKET:-${ghcDebugSocket}}"
    OUT="''${1:-/data/ghc-debug-$(date -u +%Y%m%dT%H%M%SZ).snapshot}"
    # THREADS tunes the parallel heap-traversal worker count (ghc-debug's
    # GHC.Debug.ParTrace uses `threads = numCapabilities` natively in 0.8). Unset => the
    # exe default of 64 (baked via -with-rtsopts=-N64), matching upstream's
    # original behaviour. On a large heap that 64-way traversal can OOM the
    # client; set e.g. THREADS=1 for serial/low-memory (slower, node paused
    # longer) or THREADS=4 for a middle ground. Passed through as the RTS -N flag.
    RTS=()
    if [ -n "''${THREADS:-}" ]; then RTS=(+RTS "-N''${THREADS}" -RTS); fi
    if [ ! -S "$SOCK" ]; then
      echo "[take-snapshot] ghc-debug socket not found at: $SOCK" >&2
      echo "[take-snapshot] Is the node running, and built with the ghc-debug flag?" >&2
      exit 1
    fi
    echo "[take-snapshot] socket=$SOCK out=$OUT threads=''${THREADS:-64 (default)}"
    echo "[take-snapshot] NOTE: this pauses the node for the duration of the capture."
    ${cardano-debug}/bin/cardano-debug snapshot "$OUT" "$SOCK" "''${RTS[@]}"
    echo "[take-snapshot] wrote $OUT ($(du -h "$OUT" | cut -f1))."
    echo "[take-snapshot] Copy it off the container (docker cp / volume) and ship it back for offline analysis."
  '';

  # The docker context with static content
  context = ./context/node;

  genCfgs = let
    environments' = getAttrs [ "mainnet" "preprod" "preview" ] commonLib.environments;
    cardano-deployment = commonLib.mkConfigHtml environments';
  in
    pkgs.runCommand "cardano-html" {} ''
      mkdir "$out"
      cp "${cardano-deployment}/index.html" "$out/"
      cp "${cardano-deployment}/rest-config.json" "$out/"

      ENVS=(${escapeShellArgs (builtins.attrNames environments')})
      for ENV in "''${ENVS[@]}"; do
        # Migrate each env from a flat dir to an ENV subdir
        mkdir -p "$out/config/$ENV"
        for i in $(find ${cardano-deployment} -type f -name "$ENV-*" -printf "%f\n"); do
          cp -v "${cardano-deployment}/$i" "$out/config/$ENV/''${i#"$ENV-"}"
        done

        # Adjust genesis file, config refs
        for i in config config-legacy db-sync-config; do
          if [ -f "$out/config/$ENV/$i.json" ]; then
            sed -i "s|\"$ENV-|\"|g" "$out/config/$ENV/$i.json"
          fi
        done

        # Normalize the topology file peer snapshot ref for per ENV dir placement
        ${jq}/bin/jq '.peerSnapshotFile = "peer-snapshot.json"' < "$out/config/$ENV/topology.json" > "$ENV-topology.json"
        mv -v "$ENV-topology.json" "$out/config/$ENV/topology.json"

        # Adjust index.html file refs
        sed -i "s|$ENV-|config/$ENV/|g" "$out/index.html"
      done
    '';

in
  dockerTools.buildImage {
    name = "${repoName}";
    tag = "${gitrev}";
    fromImage = baseImage;

    # Set creation date to build time. Breaks reproducibility.
    created = "now";

    extraCommands = ''
      # The "scripts" operation mode of this image, when the NETWORK env var is
      # set to a valid network, will use the following default directories
      # mounted at /:
      mkdir -p data
      mkdir -p ipc

      # Similarly, make a root level dir for logs:
      mkdir -p logs

      # The "custom" operation mode of this image, when the NETWORK env is
      # unset and "run" is provided as an entrypoint arg, will use the
      # following default directories.  To reduce confusion caused by default
      # directory paths varying by mode, symlink these directories to the
      # "scripts" mode default directories at the root location.  This will
      # permit use of volume mounts at the root directory location regardless
      # of which mode the image is operating in.
      mkdir -p opt/cardano
      ln -sv /data opt/cardano/data
      ln -sv /ipc opt/cardano/ipc
      ln -sv /logs opt/cardano/logs

      # Setup bins
      mkdir -p usr/local/bin
      cp -v ${runNetwork}/bin/* usr/local/bin
      cp -v ${context}/bin/* usr/local/bin
      ln -sv ${cardano-node}/bin/cardano-node usr/local/bin/cardano-node
      ln -sv ${cardano-cli}/bin/cardano-cli usr/local/bin/cardano-cli
      ln -sv ${db-analyser}/bin/db-analyser usr/local/bin/db-analyser
      ln -sv ${db-synthesizer}/bin/db-synthesizer usr/local/bin/db-synthesizer
      ln -sv ${db-truncater}/bin/db-truncater usr/local/bin/db-truncater
      ln -sv ${snapshot-converter}/bin/snapshot-converter usr/local/bin/snapshot-converter
      ln -sv ${jq}/bin/jq usr/local/bin/jq
      ${lib.optionalString (cardano-debug != null) ''
        # Static (musl) ghc-debug client + operator helper (debug images only).
        # Copied (real file, no store deps) so `docker cp` yields a standalone
        # binary that also runs off-container.
        cp -vL ${cardano-debug}/bin/cardano-debug usr/local/bin/cardano-debug
        chmod 0755 usr/local/bin/cardano-debug
        cp -v ${takeSnapshot}/bin/take-snapshot usr/local/bin/take-snapshot
      ''}

      # Create iohk-nix network configs, organized by network directory.
      SRC="${genCfgs}"
      DST="opt/cardano"

      # Make the directory structure with the iohk-nix configs mutable. This
      # enables creation of merge mode entrypoint configs in the respective
      # NETWORK directory. Keep config files as read-only copies from the nix
      # store instead of direct symlinks.  This avoids volume mount failures
      # caused by broken symlinks as seen from the host.
      cp -R "$SRC"/* "$DST"
      find "$DST" -mindepth 1 -type d -exec bash -c "chmod 0755 {}" \;
    '';

    config = {
      EntryPoint = [ "entrypoint" ];
    } // lib.optionalAttrs (cardano-debug != null) {
      # Make the instrumented node serve ghc-debug here; `take-snapshot` reads
      # the same variable. Placed under /ipc so it shares the standard volume.
      Env = [ "GHC_DEBUG_SOCKET=${ghcDebugSocket}" ];
    };
  }
