
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Api
import qualified Test.Cardano.Api.CBOR
import qualified Test.Cardano.Api.Convert
import qualified Test.Cardano.Api.Ledger
import qualified Test.Cardano.Api.View
import qualified Test.Cardano.Api.Typed.CBOR
import qualified Test.Cardano.Api.Typed.RawBytes
import qualified Test.Cardano.Api.Typed.Envelope

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Api.CBOR.tests
    , Test.Cardano.Api.Convert.tests
    , Test.Cardano.Api.Ledger.tests
    , Test.Cardano.Api.View.tests
    , Test.Cardano.Api.tests
    , Test.Cardano.Api.Typed.CBOR.tests
    , Test.Cardano.Api.Typed.Envelope.tests
    , Test.Cardano.Api.Typed.RawBytes.tests
    ]
