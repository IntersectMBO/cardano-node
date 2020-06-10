
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Api
import qualified Test.Cardano.Api.CBOR
import qualified Test.Cardano.Api.Convert
import qualified Test.Cardano.Api.View
import qualified Test.Cardano.Api.TextView
import qualified Test.Cardano.Api.Types
import qualified Test.Cardano.Api.Typed.CBOR
import qualified Test.Cardano.Api.Typed.Envelope

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Api.CBOR.tests
    , Test.Cardano.Api.Convert.tests
    , Test.Cardano.Api.TextView.tests
    , Test.Cardano.Api.Types.tests
    , Test.Cardano.Api.View.tests
    , Test.Cardano.Api.tests
    , Test.Cardano.Api.Typed.CBOR.tests
    , Test.Cardano.Api.Typed.Envelope.tests
    ]
