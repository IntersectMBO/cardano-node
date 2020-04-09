############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#
# To launch with provided mainnet configuration and persist state in a docker volume run:
#
#   docker run -v /data -e NETWORK=mainnet inputoutput/cardano-node:<TAG>
#
# To launch with provided testnet configuration without persisting state run:
#
#   docker run -e NETWORK=testnet inputoutput/cardano-node:<TAG>
#
# To launch with custom config, mount a dir containing configuration.yaml, genesis.json, and topology.json
#
#   docker run -v $PATH_TO/configuration:/configuration \
#     inputoutput/cardano-node:<TAG>
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
, writeScriptBin
, runtimeShell
, lib

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
  # Image with all iohk-nix network configs or utilizes a configuration volume mount
  # To choose a network, use `-e NETWORK testnet`
    clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (_: value: value) (commonLib.forEnvironments (env: ''
      elif [[ "$NETWORK" == "${env.name}" ]]; then
        exec ${scripts.${env.name}.node}
    '')));
  nodeDockerImage = let
    entry-point = writeScriptBin "entry-point" ''
      #!${runtimeShell}
      echo $NETWORK
      if [[ -d /configuration ]]; then
        exec ${cardano-node}/bin/cardano-node run \
          --config /configuration/configuration.yaml \
          --database-path /data/db \
          --host-addr 127.0.0.1 \
          --port 3001 \
          --socket-path /data/node.socket \
          --topology /configuration/topology.json $@
      ${clusterStatements}
      else
        echo "Please set a NETWORK environment variable to one of: mainnet/testnet"
        echo "Or mount a /configuration volume containing: configuration.yaml, genesis.json, and topology.json"
      fi
    '';
  in dockerTools.buildImage {
    name = "${repoName}";
    fromImage = baseImage;
    tag = "${gitrev}";
    created = "now";   # Set creation date to build time. Breaks reproducibility
    contents = [ entry-point ];
    config = {
      EntryPoint = [ "${entry-point}/bin/entry-point" ];
      ExposedPorts = {
        "3001/tcp" = {};  # Cardano node p2p
      };
    };
  };

in nodeDockerImage
