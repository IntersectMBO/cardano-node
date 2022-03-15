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

  # Mainnet configuration used by the 'run' option
  mainnetConfigFile = builtins.toFile "mainnet-config.json"
    (builtins.toJSON commonLib.environments.mainnet.nodeConfig);
  mainnetTopologyFile = commonLib.mkEdgeTopology { edgeNodes = [ commonLib.environments.mainnet.relaysNew ]; valency = 2; };

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
