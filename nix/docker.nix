############################################################################
# Docker image builder
#
# To test it out, use:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#
# To launch mainnet and keep state in a persistent docker volume run:
#
#   docker run -v /data -e ENV=mainnet inputoutput/cardano-node:<TAG>

# To launch testnet and keep no state between launches run:
#
#   docker run -e ENV=testnet inputoutput/cardano-node:<TAG>
#
# To launch with a custom config, volume mount /config and /data
#
#   docker run -v $PATH_TO/config:/config -v $PATH_TO/data:/data \
#     inputoutput/cardano-node:<TAG> --genesis-hash <GENESIS_HASH>
#
# /config must contain config.json, genesis.json and topology.json
#
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
  # Image with all environment configs or utilizes a config volume mount
  # To choose an environment, use `-e ENV testnet`
    clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (_: value: value) (commonLib.forEnvironments (env: ''
      elif [[ "$ENV" == "${env.name}" ]]; then
        exec ${scripts.${env.name}.node}
    '')));
  nodeDockerImage = let
    entry-point = writeScriptBin "entry-point" ''
      #!${runtimeShell}
      echo $ENV
      if [[ -d /config ]]; then
        exec ${cardano-node}/bin/cardano-node run \
          --config /config/config.json \
          --database-path /data/db \
          --genesis-file /config/genesis.json \
          --host-addr 127.0.0.1 \
          --port 3001 \
          --socket-path /data/node.socket \
          --topology /config/topology.json $@
      ${clusterStatements}
      else
        echo "Please set ENV variable to one of: mainnet/testnet"
        echo "Or add a /config volume with the config files: config.json, topology.json and genesis.json"
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
