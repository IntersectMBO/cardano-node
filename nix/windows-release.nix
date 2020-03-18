############################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, project
, cardano-node
}:

let
  name = "cardano-node-${project.version}-win64";

in pkgs.runCommand name { buildInputs = [ pkgs.buildPackages.zip ]; } ''
  mkdir -p $out/nix-support release
  cd release

  cp ${cardano-node}/bin/* .
  chmod -R +w .

  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
