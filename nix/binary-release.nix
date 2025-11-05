############################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, version
, exes
, platform
}:

let
  inherit (pkgs) lib;

  name = "cardano-node-${version}-${platform}";

  environments = lib.getAttrs
    [ "mainnet" "preprod" "preview" ]
    pkgs.cardanoLib.environments;


  writeConfig = name: env:
    let
      genesisAttrs = {
        # File references point to the nix store, so we need to rewrite them
        # as relative paths
        ByronGenesisFile =  "byron-genesis.json";
        ShelleyGenesisFile = "shelley-genesis.json";
        AlonzoGenesisFile = "alonzo-genesis.json";
      } // lib.optionalAttrs (env.nodeConfig ? ConwayGenesisFile) {
        ConwayGenesisFile = "conway-genesis.json";
      } // lib.optionalAttrs (env.nodeConfig ? CheckpointsFile) {
        CheckpointsFile = "checkpoints.json";
      };

      nodeConfig = pkgs.writeText
        "config.json"
        (builtins.toJSON
          (env.nodeConfig // genesisAttrs));

      nodeConfigLegacy= pkgs.writeText
        "config-legacy.json"
        (builtins.toJSON
          (env.nodeConfigLegacy // genesisAttrs));

      submitApiConfig = pkgs.writeText
        "submit-api-config.json"
        (builtins.toJSON env.submitApiConfig);

      tracerConfig = pkgs.writeText
        "tracer-config.json"
        (builtins.toJSON env.tracerConfig);

      peerSnapshot = pkgs.writeText
        "peer-snapshot.json"
        (builtins.toJSON env.peerSnapshot);

      topologyConfig = pkgs.cardanoLib.mkTopology env;

      inherit (env.nodeConfig)
        ByronGenesisFile ShelleyGenesisFile AlonzoGenesisFile;
    in
      # Format the node config file and copy the genesis files. Normalize the
      # topology file peer snapshot ref for per env dir placement.
      ''
        mkdir -p "share/${name}"
        jq . < "${nodeConfig}" > share/${name}/config.json
        jq . < "${nodeConfigLegacy}" > share/${name}/config-legacy.json
        jq . < "${submitApiConfig}" > share/${name}/submit-api-config.json
        jq . < "${tracerConfig}" > share/${name}/tracer-config.json
        jq . < "${peerSnapshot}" > share/${name}/peer-snapshot.json
        jq '.peerSnapshotFile = "peer-snapshot.json"' < "${topologyConfig}" > share/${name}/topology.json
        cp -n --remove-destination -v \
          "${ByronGenesisFile}" \
           share/${name}/byron-genesis.json
        cp -n --remove-destination -v \
          "${ShelleyGenesisFile}"  \
           share/${name}/shelley-genesis.json
        cp -n --remove-destination -v \
          "${AlonzoGenesisFile}" \
           share/${name}/alonzo-genesis.json
        ${lib.optionalString (env.nodeConfig ? ConwayGenesisFile) ''
        cp -n --remove-destination -v \
          "${env.nodeConfig.ConwayGenesisFile}" \
           share/${name}/conway-genesis.json
        ''}
        ${lib.optionalString (env.nodeConfig ? CheckpointsFile) ''
        cp -n --remove-destination -v \
          "${env.nodeConfig.CheckpointsFile}" \
           share/${name}/checkpoints.json
        ''}
      '';

in pkgs.runCommand name {
    nativeBuildInputs = with pkgs.pkgsBuildBuild; [
      haskellBuildUtils bintools jq nix zip
    ];
  } ''
  mkdir -p $out release/{bin,share}
  cd release

  # note: on windows, we have all the .dlls in the same /bin folder. Thus we will
  #       get the same dlls for each executable multiple times. So we cannot really
  #       use `-n` here, which would warn that we "skipped" some duplicates; and
  #       exit with 1. `-u` on the otherhand will just update as needed.
  cp -u --remove-destination -v ${pkgs.lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ./bin
  chmod -R +w .

  ${lib.pipe environments [
    (lib.mapAttrs writeConfig)
    (lib.mapAttrsToList (_: val: val))
    (lib.concatStringsSep "\n")
  ]}

  ${lib.optionalString (platform == "macos") (lib.concatMapStrings (exe: ''
    rewrite-libs bin ${exe}/bin/*
  '') exes)}

  ${if (platform == "win64")
    then "zip -r $out/${name}.zip ."
    else "tar -czf $out/${name}.tar.gz ."
  }
  dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
''
