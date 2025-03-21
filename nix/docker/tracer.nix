############################################################################
# Docker image builder for cardano-tracer
#
# To build and load into the Docker engine:
#
#   nix build .#dockerImage/tracer
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
, cardano-tracer
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
, jq
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
        cardano-tracer    # Provide cardano-tracer capability
        bashInteractive   # Provide the BASH shell
        cacert            # X.509 certificates of public CA's
        coreutils         # Basic utilities expected in GNU OS's
        curl              # CLI tool for transferring files via URLs
        glibcLocales      # Locale information for the GNU C Library
        iana-etc          # IANA protocol and port number assignments
        iproute           # Utilities for controlling TCP/IP networking
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
    '';

    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      pathsToLink = ["/"];
      paths = [entry-point];
    };

    config = {
      EntryPoint = [ "${entry-point}/bin/entry-point" ];
    };
  }
