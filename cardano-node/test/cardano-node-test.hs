
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Node.Chairman
import qualified Test.Cardano.Node.Json

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Node.Chairman.tests
    , Test.Cardano.Node.Json.tests
    ]
