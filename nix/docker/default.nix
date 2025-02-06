############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   nix build .#dockerImage/node
#   docker load -i result
#
# To launch with pre-loaded configuration, using the NETWORK env.
# An example using a docker volume to persist state:
#
#   docker run -v /data -e NETWORK=mainnet ghcr.io/intersectmbo/cardano-node
#
# Provide a complete command otherwise:
#
#   docker run -v $PWD/configuration/cardano:/configuration \
#     ghcr.io/intersectmbo/cardano-node run \
#      --config /configuration/mainnet-config.yaml \
#      --topology /configuration/mainnet-topology.json \
#      --database-path /db
#
# Mount a volume into /ipc for establishing cross-container communication via node.socket
#
#   docker run -v node-ipc:/ipc -e NETWORK=mainnet ghcr.io/intersectmbo/cardano-node
#   docker run -v node-ipc:/ipc -e NETWORK=mainnet ghcr.io/intersectmbo/some-node-client
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

  # The Docker context with static content
  context = ./context;

  genCfgs = let
    environments = lib.getAttrs [ "mainnet" "preprod" "preview" "sanchonet" ] commonLib.environments;
    cardano-deployment = commonLib.mkConfigHtml environments;
  in
    pkgs.runCommand "cardano-html" {} ''
      mkdir "$out"
      cp "${cardano-deployment}/index.html" "$out/"
      cp "${cardano-deployment}/rest-config.json" "$out/"

      ENVS=(${lib.escapeShellArgs (builtins.attrNames environments)})
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

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    extraCommands = ''
      # These directories serve as defaults when the node docker container uses the `run` arg.
      # Alternatively, when the NETWORK environment variable is set the defaults are different.
      # TODO: Reduce the confusion on this.
      mkdir -p opt/cardano/data
      mkdir -p opt/cardano/ipc
      mkdir -p opt/cardano/logs
      mkdir -p usr/local/bin

      cp -v ${runNetwork}/bin/* usr/local/bin
      cp -v ${context}/bin/* usr/local/bin

      ln -sv ${cardano-node}/bin/cardano-node usr/local/bin/cardano-node
      ln -sv ${cardano-cli}/bin/cardano-cli usr/local/bin/cardano-cli

      # Create iohk-nix network configs, organized by network directory.
      SRC="${genCfgs}"
      DST="opt/cardano"

      # Make the directory structure with the iohk-nix configs mutable.
      # This leaves the option to create merged entrypoint configs in the network directory.
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
