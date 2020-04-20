
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Config

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Config.tests
    ]
