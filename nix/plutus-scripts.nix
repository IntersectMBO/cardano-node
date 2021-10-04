############################################################################
# The plutus-example scripts as nix-outputs
############################################################################

{ runCommand
, plutus-builder
}:

runCommand "plutus-example-scripts" { }
  ''
  mkdir -p $out
  cd $out
  ${plutus-builder}/bin/plutus-example
  ls -alR .
  ''
