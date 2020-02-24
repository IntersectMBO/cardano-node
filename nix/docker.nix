############################################################################
# Docker image builder
#
# To test it out, use:
#
#   docker load -i $(nix-build -A dockerImage.base --no-out-link)
#   docker load -i $(nix-build -A dockerImage.latency-tests --no-out-link)
#   docker load -i $(nix-build -A dockerImage.mainnet --no-out-link)
#   docker load -i $(nix-build -A dockerImage.mainnet-ci --no-out-link)
#   docker load -i $(nix-build -A dockerImage.shelley_staging --no-out-link)
#   docker load -i $(nix-build -A dockerImage.shelley_staging_short --no-out-link)
#   docker load -i $(nix-build -A dockerImage.staging --no-out-link)
#   docker load -i $(nix-build -A dockerImage.testnet --no-out-link)
#
# For all output except base, run:
#
#   docker run inputoutput/cardano-node:<TAG>
#
# ie:
#
#   docker run inputoutput/cardano-node:6a5b233e5861aa0012287a5557b3a6a08afbf3ca-testnet
#
# For base, you will need to provide all variables. ie:
#
#   docker run -ti \
#   --volume $PATH_TO/state-docker:/data \
#   --volume $PATH_TO/configuration-mainnet.yaml:/config/config.yaml \
#   --volume $PATH_TO/mainnet-topology.json:/config/topology.json \
#   --volume $PATH_TO/mainnet-genesis.json:/config/genesis.json \
#   inputoutput/cardano-node:<GIT_HASH>-custom
#   --genesis-hash <GENESIS_HASH>
#
############################################################################

{ iohkNix
, commonLib
, dockerTools

# The main contents of the image.
, cardano-node
, scripts

# Get the current commit
, gitrev ? iohkNix.commitIdFromGitRepoOrZero ../.git

# Other things to include in the image.
, bashInteractive
, cacert
, coreutils
, curl
, glibcLocales
, iana-etc
, iproute
, iputils
, socat
, utillinux

, repoName ? "inputoutput/cardano-node"
}:

let

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [
      bashInteractive   # Provide the BASH shell
      cacert            # X.509 certificates of public CA's
      coreutils         # Basic utilities expected in GNU OS's
      curl              # CLI tool for transferring files via URLs
      glibcLocales      # Locale information for the GNU C Library
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
  customImage = dockerTools.buildImage {
    name = "${repoName}";
    tag = "${gitrev}-base";
    fromImage = baseImage;
    contents = [
      cardano-node
    ];
    created = "now";   # Set creation date to build time. Breaks reproducibility
    extraCommands = ''
      mkdir -p data config
    '';
    config = {
      EntryPoint = [
        "${cardano-node}/bin/cardano-node" "run" "--config"
        "/config/config.yaml" "--database-path" "/data.db" "--genesis-file"
        "/config/genesis.json" "--host-addr" "127.0.0.1" "--port" "3001"
        "--socket-path" "/data/ipc/socket0" "--topology" "/config/topology.json"
      ];
      ExposedPorts = {
        "3001/tcp" = {};  # Cardano node p2p
      };
    };
  };
  clusterImage = env: dockerTools.buildImage {
    name = "${repoName}";
    tag = "${gitrev}-${env.name}";
    fromImage = customImage;
    created = "now";   # Set creation date to build time. Breaks reproducibility
    config = {
      EntryPoint = [ scripts.${env.name}.node ];
      ExposedPorts = {
        "3001/tcp" = {};  # Cardano node p2p
      };
    };
  };

in commonLib.forEnvironments clusterImage // { base = customImage; }
