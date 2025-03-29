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

  # The docker context with static content
  context = ./context/tracer;

  genCfgs = let
    environments' = lib.getAttrs [ "mainnet" "preprod" "preview" ] commonLib.environments;
    cardano-deployment = commonLib.mkConfigHtml environments';
  in
    pkgs.runCommand "cardano-html" {} ''
      mkdir "$out"

      ENVS=(${lib.escapeShellArgs (builtins.attrNames environments')})
      for ENV in "''${ENVS[@]}"; do
        # Migrate each env from a flat dir to an ENV subdir
        mkdir -p "$out/config/$ENV"
        for i in $(find ${cardano-deployment} -type f -name "$ENV-tracer-config*" -printf "%f\n"); do
          cp -v "${cardano-deployment}/$i" "$out/config/$ENV/''${i#"$ENV-"}"

          # Adjust from iohk-nix default config for the oci environment
          sed -i -r \
            -e 's|"contents": ".*"|"contents": "/ipc/cardano-tracer.socket"|g' \
            -e 's|"logRoot": ".*"|"logRoot": "/logs"|g' \
            "$out/config/$ENV/''${i#"$ENV-"}"
        done
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
      # following default directories.
      mkdir -p opt/cardano

      # Setup bins
      mkdir -p usr/local/bin
      cp -v ${runNetwork}/bin/* usr/local/bin
      cp -v ${context}/bin/* usr/local/bin
      ln -sv ${cardano-tracer}/bin/cardano-tracer usr/local/bin/cardano-tracer
      ln -sv ${jq}/bin/jq usr/local/bin/jq

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
    };
  }
