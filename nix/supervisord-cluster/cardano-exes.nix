{ lib
, stateDir
, useCabalRun ? false
, cabal-install
, cardano-cli
, cardano-node
}@args:
with lib;
let
  runner = exe:
    if useCabalRun
    then toString
      [ "${cabal-install}/bin/cabal"
        "-v0"
        "run"
        "exe:cardano-${exe}"
        "--"
      ]
    else "${args."cardano-${exe}"}/bin/cardano-${exe}";
in
''
function cli() {
  ${runner "cli"} "$@"
}

function node() {
  ${runner "node"} "$@"
}

${optionalString useCabalRun
  ''
  echo 'Prebuilding executables with Cabal..'
  ${cabal-install}/bin/cabal build exe:cardano-cli
  ${cabal-install}/bin/cabal build exe:cardano-node
  ''}
${optionalString (!useCabalRun)
  ''
  echo 'Nix-supplied executables are:'
  echo '${cardano-node}/bin/cardano-node'
  echo '${cardano-cli}/bin/cardano-cli'
  ''}
''
