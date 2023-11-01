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

in pkgs.runCommand name {
    nativeBuildInputs = with pkgs.pkgsBuildBuild; [
      haskellBuildUtils bintools nix zip
    ];
  } ''
  mkdir -p $out release
  cd release

  # note: on windows, we have all the .dlls in the same /bin folder. Thus we will
  #       get the same dlls for each executable multiple times. So we cannot really
  #       use `-n` here, which would warn that we "skipped" some duplicates; and
  #       exit with 1. `-u` on the otherhand will just update as needed.
  cp -u --remove-destination -v ${pkgs.lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ./
  mkdir -p configuration
  cp -Rv ${../configuration}/* ./configuration/
  chmod -R +w .

  ${lib.optionalString (platform == "macos") (lib.concatMapStrings (exe: ''
    rewrite-libs . ${exe}/bin/*
  '') exes)}

  ${if (platform == "win64")
    then "zip -r $out/${name}.zip ."
    else "tar -czf $out/${name}.tar.gz ."
  }
  dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
''
