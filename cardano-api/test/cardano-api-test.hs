
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Api
import qualified Test.Cardano.Api.CBOR
import qualified Test.Cardano.Api.Convert
import qualified Test.Cardano.Api.View

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Api.CBOR.tests
    , Test.Cardano.Api.Convert.tests
    , Test.Cardano.Api.View.tests
    , Test.Cardano.Api.tests
    ]
