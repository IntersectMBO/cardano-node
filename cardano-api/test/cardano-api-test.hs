
import           Cardano.Prelude

import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.Api.Crypto
import qualified Test.Cardano.Api.Ledger
import qualified Test.Cardano.Api.MetaData
import qualified Test.Cardano.Api.Typed.Bech32
import qualified Test.Cardano.Api.Typed.CBOR
import qualified Test.Cardano.Api.Typed.Envelope
import qualified Test.Cardano.Api.Typed.Json
import qualified Test.Cardano.Api.Typed.MultiSigScript
import qualified Test.Cardano.Api.Typed.RawBytes

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup "Cardano.Api"
    [ Test.Cardano.Api.Crypto.tests
    , Test.Cardano.Api.Ledger.tests
    , Test.Cardano.Api.MetaData.tests
    , Test.Cardano.Api.Typed.Bech32.tests
    , Test.Cardano.Api.Typed.CBOR.tests
    , Test.Cardano.Api.Typed.Envelope.tests
    , Test.Cardano.Api.Typed.Json.tests
    , Test.Cardano.Api.Typed.MultiSigScript.tests
    , Test.Cardano.Api.Typed.RawBytes.tests
    ]
