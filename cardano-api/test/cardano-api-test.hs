
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Cardano.Api.Ledger
import qualified Test.Cardano.Api.MetaData
import qualified Test.Cardano.Api.Typed.Bech32
import qualified Test.Cardano.Api.Typed.CBOR
import qualified Test.Cardano.Api.Typed.Envelope
import qualified Test.Cardano.Api.Typed.MultiSigScript
import qualified Test.Cardano.Api.Typed.RawBytes

main :: IO ()
main =
  defaultMain
    [ Test.Cardano.Api.Ledger.tests
    , Test.Cardano.Api.MetaData.tests
    , Test.Cardano.Api.Typed.Bech32.tests
    , Test.Cardano.Api.Typed.CBOR.tests
    , Test.Cardano.Api.Typed.Envelope.tests
    , Test.Cardano.Api.Typed.MultiSigScript.tests
    , Test.Cardano.Api.Typed.RawBytes.tests
    ]
