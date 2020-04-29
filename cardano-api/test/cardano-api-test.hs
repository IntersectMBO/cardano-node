
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Api
import qualified Test.Cardano.Api.Byron.CBOR
import qualified Test.Cardano.Api.CBOR
import qualified Test.Cardano.Api.Byron.View
import qualified Test.Cardano.Api.Shelley.CBOR
import qualified Test.Cardano.Api.Shelley.View
import qualified Test.Cardano.Api.View

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Api.Byron.CBOR.tests
    , Test.Cardano.Api.Byron.View.tests
    , Test.Cardano.Api.CBOR.tests
    , Test.Cardano.Api.Shelley.CBOR.tests
    , Test.Cardano.Api.Shelley.View.tests
    , Test.Cardano.Api.View.tests
    , Test.Cardano.Api.tests
    ]
