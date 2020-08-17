
import           Cardano.Prelude
import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Node.Json

main :: IO ()
main = defaultMain
    [ Test.Cardano.Node.Json.tests
    ]
