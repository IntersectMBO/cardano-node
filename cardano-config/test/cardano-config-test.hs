
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Config.Json

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Config.Json.tests
    ]
