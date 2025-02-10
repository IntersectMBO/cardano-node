############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   nix run .#dockerImage/submit-api
#   docker load -i result
#
# Scripts Mode:
#
# To launch cardano-submit-api with pre-loaded configuration, "scripts" mode,
# use the NETWORK env variable to declare an existing cardano network name.
#
# An example using a docker named volume to share ipc socket state:
#
#   docker run \
#     -v node-ipc:/ipc \
#     -e NETWORK=mainnet \
#     ghcr.io/intersectmbo/cardano-submit-api
#
# In "scripts" mode, the node.socket file is expected at /ipc.
#
#
# Custom Mode:
#
# To launch cardano-submit-api with a custom configuration, "custom" mode,
# leave the NETWORK env variable unset and provide a complete set of
# cardano-submit-api args to the entrypoint.
#
# For example:
#
#   docker run \
#     -v $PWD/config.json:/config.json \
#     ghcr.io/intersectmbo/cardano-submit-api \
#     --config /config.json \
#     --mainnet \
#     --socket-path /ipc/node.socket
#
# See the docker-compose.yml for a demonstration of cardano-node and
# cardano-submit-api together.
#
#
# Bind Mounting Considerations:
#
# In the container a /node-ipc directory is symlinked to /ipc both align the
# default ipc socket state directory in both the cardano-node and
# cardano-submit-api images and remain backward compatible.
#
############################################################################
{ pkgs
, dockerTools

# The main contents of the image.
, scripts

# Set gitrev to null, to ensure the version below is used
, gitrev ? null

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
, exe
, script
, repoName ? "ghcr.io/intersectmbo/${exe}"
}:

let

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      pathsToLink = ["/"];
      paths = [
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
    };

    # Set up /tmp (override with TMPDIR variable)
    extraCommands = ''
      mkdir -m 0777 tmp
    '';
  };

  # For "script" mode, generate scripts for iohk-nix networks which can be
  # utilized by setting the environment NETWORK variable to the desired
  # network in the docker command: `-e NETWORK <network>`
  clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (env: scripts: let
    scriptBin = scripts.${script};
    in ''
      elif [[ "$NETWORK" == "${env}" ]]; then
        exec ${scriptBin}/bin/${scriptBin.name} $@
    '') scripts);

  entry-point = writeScriptBin "entry-point" ''
    #!${runtimeShell}
    if [[ -z "$NETWORK" ]]; then
      exec ${pkgs.${exe}}/bin/${exe} $@
    ${clusterStatements}
    else
      echo "Managed configuration for network "$NETWORK" does not exist"
    fi
  '';

  in dockerTools.buildImage {
    name = "${repoName}";
    tag = "${gitrev}";
    fromImage = baseImage;

    # Set creation date to build time. Breaks reproducibility.
    created = "now";

    extraCommands = ''
      # The "scripts" operation mode of this image, when the NETWORK env var is
      # set to a valid network, will use the following default directories
      # mounted at /:
      mkdir -p ipc
      ln -sv ipc node-ipc
    '';

    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      pathsToLink = ["/"];
      paths = [entry-point];
    };

    config = {
      EntryPoint = [ "${entry-point}/bin/entry-point" ];
      ExposedPorts = {
        "${toString scripts.mainnet.${script}.passthru.service.port}/tcp" = {};
      };
    };
  }
