import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cli.ITN
import qualified Test.Cli.Pioneers.Exercise1
import qualified Test.Cli.Pioneers.Exercise2
import qualified Test.Cli.Pioneers.Exercise3
import qualified Test.Cli.Pioneers.Exercise4

main :: IO ()
main =
  defaultMain
    [ Test.Cli.ITN.tests
    , Test.Cli.Pioneers.Exercise1.tests
    , Test.Cli.Pioneers.Exercise2.tests
    , Test.Cli.Pioneers.Exercise3.tests
    , Test.Cli.Pioneers.Exercise4.tests
    ]
