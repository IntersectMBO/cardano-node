{ lib
, stateDir
, useCabalRun ? false
, cabal-install
, cardano-cli
, cardano-node
, cardano-topology
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

function topology() {
  ${runner "topology"} "$@"
}

${optionalString useCabalRun
  ''
  echo 'Prebuilding executables with Cabal..'
  ${cabal-install}/bin/cabal build exe:cardano-cli
  ${cabal-install}/bin/cabal build exe:cardano-node
  ${cabal-install}/bin/cabal build exe:cardano-topology
  ''}
${optionalString (!useCabalRun)
  ''
  echo 'Nix-supplied executables are:'
  echo '${cardano-node}/bin/cardano-node'
  echo '${cardano-cli}/bin/cardano-cli'
  echo '${cardano-topology}/bin/cardano-topology'
  ''}
''
