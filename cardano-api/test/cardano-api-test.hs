
import           Cardano.Crypto.Libsodium (sodiumInit)
import           Cardano.Prelude

import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.Api.Crypto
import qualified Test.Cardano.Api.Ledger
import qualified Test.Cardano.Api.MetaData
import qualified Test.Cardano.Api.Typed.Bech32
import qualified Test.Cardano.Api.Typed.CBOR
import qualified Test.Cardano.Api.Typed.Envelope
import qualified Test.Cardano.Api.Typed.MultiSig.Allegra
import qualified Test.Cardano.Api.Typed.MultiSig.Mary
import qualified Test.Cardano.Api.Typed.MultiSig.Shelley
import qualified Test.Cardano.Api.Typed.RawBytes

main :: IO ()
main = do
  -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
  sodiumInit
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
    , Test.Cardano.Api.Typed.MultiSig.Allegra.tests
    , Test.Cardano.Api.Typed.MultiSig.Mary.tests
    , Test.Cardano.Api.Typed.MultiSig.Shelley.tests
    , Test.Cardano.Api.Typed.RawBytes.tests
    ]
