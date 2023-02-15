############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   nix run .#dockerImage/node/load
#
# To launch with pre-loaded configuration, using the NETWORK env.
# An example using a docker volume to persist state:
#
#   docker run -v /data -e NETWORK=mainnet inputoutput/cardano-node
#
# Provide a complete command otherwise:
#
#   docker run -v $PWD/configuration/defaults/byron-mainnet:/configuration \
#     inputoutput/cardano-node run \
#      --config /configuration/configuration.yaml \
#      --topology /configuration/topology.json \
#      --database-path /db
#
# Mount a volume into /ipc for establishing cross-container communication via node.socket
#
#   docker run -v node-ipc:/ipc inputoutput/cardano-node
#   docker run -v node-ipc:/ipc inputoutput/some-node-client
############################################################################

{ pkgs
, commonLib
, dockerTools

# The main contents of the image.
, cardano-cli
, cardano-node
, scripts

# Set gitrev to null, to ensure the version below is used
, gitrev ? null

# Other things to include in the image.
, bashInteractive
, buildPackages
, cacert
, coreutils
, curl
, glibcLocales
, gnutar
, gzip
, iana-etc
, iproute
, iputils
, socat
, utillinux
, writeScriptBin
, runtimeShell
, lib
, exe
, script
, repoName ? "inputoutput/${exe}"
}:

let

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [
      cardano-cli       # Provide cardano-cli capability
      bashInteractive   # Provide the BASH shell
      cacert            # X.509 certificates of public CA's
      coreutils         # Basic utilities expected in GNU OS's
      curl              # CLI tool for transferring files via URLs
      glibcLocales      # Locale information for the GNU C Library
      gnutar            # GNU tar
      gzip              # Gnuzip
      iana-etc          # IANA protocol and port number assignments
      iproute           # Utilities for controlling TCP/IP networking
      iputils           # Useful utilities for Linux networking
      socat             # Utility for bidirectional data transfer
      utillinux         # System utilities for Linux
    ];
    # set up /tmp (override with TMPDIR variable)
    extraCommands = ''
      mkdir -m 0777 tmp
    '';
  };
  # Image with all iohk-nix network configs or utilizes a configuration volume mount
  # To choose a network, use `-e NETWORK testnet`
  clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (env: scripts: let
    scriptBin = scripts.${script};
    snapshotStatements = ''
      if [ -n "''${USE_SNAPSHOT:-}" ]; then
        SNAPSHOT_BASE_URL="''${SNAPSHOT_BASE_URL:-https://update-cardano-mainnet.iohk.io/cardano-node-state}"
        SNAPSHOT_FILE_NAME="''${SNAPSHOT_FILE_NAME:-db-mainnet.tar.gz}"
        DATA_DIR="''${DATA_DIR:-/data/db}"
      fi
      if [ -n "''${SNAPSHOT_BASE_URL:-}" ]; then
        pull_snapshot
        extract_snapshot_tgz_to "$DATA_DIR" 1
      fi
    '';
    in ''
      elif [[ "$NETWORK" == "${env}" ]]; then
        ${lib.optionalString (env == "mainnet") "${snapshotStatements}"}
        exec ${scriptBin}/bin/${scriptBin.name} $@
    '') scripts);

  runNetwork = pkgs.writeShellScriptBin "run-network" ''
    ${pull-snapshot}

    if [[ -z "$NETWORK" ]]; then
      echo "[Error] Cannot obtain NETWORK env variable"
      exit 1
    ${clusterStatements}
    else
      echo "[Error] Managed configuration for network "$NETWORK" does not exist"
      exit 1
    fi
  '';

  # The Docker context with static content
  context = ./context;

  # Mainnet configuration used by the 'run' option
  mainnetConfigFile = builtins.toFile "mainnet-config.json"
    (builtins.toJSON commonLib.environments.mainnet.nodeConfig);
  mainnetTopologyFile = commonLib.mkEdgeTopology { edgeNodes = [ commonLib.environments.mainnet.relaysNew ]; valency = 2; };

  pull-snapshot = ''
    function pull_snapshot {
      [ -z "''${SNAPSHOT_BASE_URL:-}" ] && echo "SNAPSHOT_BASE_URL env var must be set -- aborting" && exit 1
      [ -z "''${SNAPSHOT_FILE_NAME:-}" ] && echo "SNAPSHOT_FILE_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DATA_DIR:-}" ] && echo "DATA_DIR env var must be set -- aborting" && exit 1

      SNAPSHOT_DIR="$DATA_DIR/initial-snapshot"
      mkdir -p "$SNAPSHOT_DIR"

      # we are already initialized
      [ -s "$SNAPSHOT_DIR/$SNAPSHOT_FILE_NAME.sha256sum" ] && INITIALIZED=true && return

      # shellcheck source=/dev/null
      source ${pkgs.cacert}/nix-support/setup-hook
      echo "Downloading $SNAPSHOT_BASE_URL/$SNAPSHOT_FILE_NAME into $SNAPSHOT_DIR  ..." >&2
      if curl -fL "$SNAPSHOT_BASE_URL/$SNAPSHOT_FILE_NAME" --output "$SNAPSHOT_DIR/$SNAPSHOT_FILE_NAME"; then
        echo "Downloading $SNAPSHOT_BASE_URL/$SNAPSHOT_FILE_NAME.sha256sum into $SNAPSHOT_DIR ..." >&2
        if curl -fL "$SNAPSHOT_BASE_URL/$SNAPSHOT_FILE_NAME.sha256sum" --output "$SNAPSHOT_DIR/$SNAPSHOT_FILE_NAME.sha256sum"; then
          echo -n "pushd: " >&2
          pushd "$SNAPSHOT_DIR" >&2
          echo "Validating sha256sum for ./$SNAPSHOT_FILE_NAME." >&2
          if sha256sum -c "$SNAPSHOT_FILE_NAME.sha256sum" >&2; then
            echo "Downloading  $SNAPSHOT_BASE_URL/$SNAPSHOT_FILE_NAME{,.sha256sum} into $SNAPSHOT_DIR complete." >&2
          else
            echo "Could retrieve snapshot, but could not validate its checksum -- aborting" && exit 1
          fi
          echo -n "popd: " >&2
          popd >&2
        else
          echo "Could retrieve snapshot, but not its sha256 file -- aborting" && exit 1
        fi
      else
        echo "No snapshot pulled -- aborting" && exit 1
      fi
    }
    function extract_snapshot_tgz_to {
      local targetDir="$1"
      local strip="''${2:-0}"
      mkdir -p "$targetDir"

      [ -n "''${INITIALIZED:-}" ] && return

      echo "Extracting snapshot to $targetDir ..." >&2
      if tar --strip-components="$strip" -C "$targetDir" -zxf "$SNAPSHOT_DIR/$SNAPSHOT_FILE_NAME"; then
        echo "Extracting snapshot to $targetDir complete." >&2
      else
        echo "Extracting snapshot to $targetDir failed -- aborting" && exit 1
      fi
    }
  '';

in
  dockerTools.buildImage {
    name = "${repoName}";
    tag = "${gitrev}";
    fromImage = baseImage;
    created = "now";   # Set creation date to build time. Breaks reproducibility
    contents = [
    ];

    # May require system-features = kvm in /etc/nix/nix.conf
    # https://discourse.nixos.org/t/cannot-build-docker-image/7445
    # runAsRoot = '' ln -s ${cardano-node} bin/cardano-node '';

    extraCommands = ''
      mkdir -p opt/cardano/config
      mkdir -p opt/cardano/data
      mkdir -p opt/cardano/ipc
      mkdir -p opt/cardano/logs
      mkdir -p usr/local/bin
      ln -s ${mainnetConfigFile} opt/cardano/config/mainnet-config.json
      ln -s ${mainnetTopologyFile} opt/cardano/config/mainnet-topology.json
      cp ${runNetwork}/bin/* usr/local/bin
      cp ${context}/bin/* usr/local/bin
      ln -s ${cardano-node}/bin/cardano-node usr/local/bin/cardano-node
      ln -s ${cardano-cli}/bin/cardano-cli usr/local/bin/cardano-cli
    '';
    config = {
      EntryPoint = [ "entrypoint" ];
    };
  }
