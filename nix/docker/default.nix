############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   nix build .#dockerImage/node
#   docker load -i result
#
#
# Scripts Mode:
#
# To launch cardano-node with pre-loaded configuration, "scripts" mode,
# use the NETWORK env variable to declare an existing cardano network name.
#
# An example using a docker named volume to persist state:
#
#   docker run \
#     -v data:/data \
#     -e NETWORK=mainnet \
#     ghcr.io/intersectmbo/cardano-node
#
# In "scripts" mode, default state directories include /{data,ipc,logs}, with
# /data/db being the default database state location.
#
#
# Custom Mode:
#
# To launch cardano-node with a custom configuration, "custom" mode, provide
# entrypoint args starting with "run" and:
#   * Leave the NETWORK env variable unset
#   * Optionally include additional cardano-node args to the entrypoint afer "run"
#   * Optionally include environment variables interpreted by nix/docker/context/bin/run-node
#     from the cardano-node repo, or /usr/local/bin/run-node in the container
#
# For example, launch a custom cardano-node using cardano-node args and a
# local configuration mapped into the container:
#
#   docker run \
#     -v "$PWD/config/cardano:/config" \
#     ghcr.io/intersectmbo/cardano-node \
#     run \
#     --config /config/mainnet/config.json \
#     --topology /config/mainnet/topology.json \
#     --database-path /data/db
#
# Custom mode may also leverage standard mainnet or testnet network config
# files found at /opt/cardano/config and organized under a subdirectory of the
# network name.  For example, to utilize standard configs for preprod network,
# but modify the cardano-node listening port:
#
#   docker run \
#     -v "$PWD/preprod-data:/data" \
#     -e CARDANO_CONFIG="/opt/cardano/config/preprod/config.json" \
#     -e CARDANO_TOPOLOGY="/opt/cardano/config/preprod/topology.json" \
#     -e CARDANO_PORT="6001" \
#     ghcr.io/intersectmbo/cardano-node \
#     run
#
# In "custom" mode, default state directories include
# /opt/cardano/{data,ipc,logs}, with /opt/cardano/data/db being the default
# database state location.  These state directories are symlinked to root in the container:
# /opt/cardano/{data,ipc,logs} -> /{data,ipc,logs} for more consistency between modes.
# Standard network config files can be found under /opt/cardano/config.
#
#
# Merge Mode:
#
# With the NETWORK env variable set and one or both of
# CARDANO_<CONFIG|TOPOLOGY>_JSON_MERGE env variables set and containing valid
# json, cardano-node will run with deep merged base NETWORK config and json
# merge config.
#
# Optional env variables and cardano-node args which can be used in custom mode
# can also be used in this mode.
#
#
# CLI Mode:
#
# To run cardano-cli, leave the NETWORK env variable unset and provide
# entrypoint args starting with "cli" followed by cardano-cli command args.
# The cardano-node ipc socket state will need to be provided in cli mode.
#
# An example using a docker named volume to share cardano-node ipc socket state:
#
#   docker run \
#     -v node-ipc:/ipc \
#     ghcr.io/intersectmbo/cardano-node \
#     cli \
#     query tip \
#     --mainnet
#
#
# Bind Mounting Considerations:
#
# For "custom" mode, the /opt/cardano/{data,ipc,logs} default state directories have been
# symlinked to the "scripts" mode default state directories of /{data,ipc,logs}
# respectively.  This makes bind mounting easier when switching between
# "scripts" and "custom" container modes as bind mounting any of the root
# default state directory locations, /{data,ipc,logs}, will work for both modes.
#
#
# Cardano-node socket sharing:
#
# To share a cardano-node socket with a different container, a volume can be made
# for establishing cross-container communication:
#
#   docker run -v node-ipc:/ipc -e NETWORK=mainnet ghcr.io/intersectmbo/cardano-node
#   docker run -v node-ipc:/ipc -e NETWORK=mainnet ghcr.io/intersectmbo/some-node-client
#
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
        cardano-cli       # Provide cardano-cli capability
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
  context = ./context;

  genCfgs = let
    environments' = lib.getAttrs [ "mainnet" "preprod" "preview" "sanchonet" ] commonLib.environments;
    cardano-deployment = commonLib.mkConfigHtml environments';
  in
    pkgs.runCommand "cardano-html" {} ''
      mkdir "$out"
      cp "${cardano-deployment}/index.html" "$out/"
      cp "${cardano-deployment}/rest-config.json" "$out/"

      ENVS=(${lib.escapeShellArgs (builtins.attrNames environments')})
      for ENV in "''${ENVS[@]}"; do
        # Migrate each env from a flat dir to an ENV subdir
        mkdir -p "$out/config/$ENV"
        for i in $(find ${cardano-deployment} -type f -name "$ENV-*" -printf "%f\n"); do
          cp -v "${cardano-deployment}/$i" "$out/config/$ENV/''${i#"$ENV-"}"
        done

        # Adjust genesis file, config and config-bp refs
        for i in config config-bp db-sync-config; do
          if [ -f "$out/config/$ENV/$i.json" ]; then
            sed -i "s|\"$ENV-|\"|g" "$out/config/$ENV/$i.json"
          fi
        done

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
      ln -sv ${jq}/bin/jq usr/local/bin/jq

      # Create iohk-nix network configs, organized by network directory.
      SRC="${genCfgs}"
      DST="opt/cardano"

      # Make the directory structure with the iohk-nix configs mutable.
      # This allows the option to create merged entrypoint configs in the network directory.
      find "$SRC" -mindepth 1 -type d -exec bash -c "DIR=\"{}\"; mkdir -v -p \"$DST/\''${DIR#${genCfgs}/}\"" \;

      # Keep all base iohk-nix config files immutable via symlinks to nix store.
      find "$SRC" -mindepth 1 -type f -exec bash -c "FILE=\"{}\"; TGT=\"$DST/\''${FILE#${genCfgs}/}\"; ln -sv \"\$FILE\" \"\$TGT\"" \;

      # Preserve legacy oci config and topo path for backwards compatibility.
      pushd opt/cardano/config
      ln -sv mainnet/config.json mainnet-config.json
      ln -sv mainnet/topology.json mainnet-topology.json
      popd
    '';

    config = {
      EntryPoint = [ "entrypoint" ];
    };
  }
