
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Node.Chairman
import qualified Test.Cardano.Node.Json
import qualified Test.Common.NetworkSpec

main :: IO ()
main = defaultMain
    [ Test.Cardano.Node.Chairman.tests
    , Test.Cardano.Node.Json.tests
    , Test.Common.NetworkSpec.tests
    ]
