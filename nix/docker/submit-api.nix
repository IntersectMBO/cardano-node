############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   nix run .#DockerImage/submit-api/load
#
# cardano-submit-api
#   To launch with provided mainnet configuration
#
#    docker run -e NETWORK=mainnet inputoutput/cardano-submit-api:<TAG>
#
#  To launch with provided testnet configuration
#
#    docker run -e NETWORK=testnet inputoutput/cardano-submit-api:<TAG>
#
#   Provide a complete command otherwise:
#
#    docker run -v $PWD/config.yaml:/config.yaml inputoutput/cardano-submit-api:<TAG> \
#      --config /config.yaml --mainnet --socket-path /node-ipc/node.socket
#
#  See the docker-compose.yml for demonstration of using Docker secrets instead of mounting a pgpass
#
############################################################################

{ pkgs
, commonLib
, dockerTools

# The main contents of the image.
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
    # set up /tmp (override with TMPDIR variable)
    extraCommands = ''
      mkdir -m 0777 tmp
    '';
  };
  # Image with all iohk-nix network configs or utilizes a configuration volume mount
  # To choose a network, use `-e NETWORK testnet`
  clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (env: scripts: let
    scriptBin = scripts.${script};
    in ''
      elif [[ "$NETWORK" == "${env}" ]]; then
        exec ${scriptBin}/bin/${scriptBin.name} $@
    '') scripts);
  nodeDockerImage = let
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
    fromImage = baseImage;
    tag = "${gitrev}";
    created = "now";   # Set creation date to build time. Breaks reproducibility
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
  };

in nodeDockerImage
