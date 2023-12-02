module Codec.CBOR.SchemaSpec where

import           Prelude

import qualified Codec.CBOR.Schema as CDDL
import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Test.Hspec (Spec, SpecWith, context, shouldBe, specify)

spec :: Spec
spec = context "Codec.CBOR.SchemaSpec" $ do
  specifyCddl "Primitive / Int"
    "rule = uint"
    "182A"

specifyCddl
  :: String
  -> Text
  -> Text
  -> SpecWith ()
specifyCddl title cddl cbor = specify title $ do
  CDDL.validate cddl (unsafeFromBase16 cbor) `shouldBe` Right ()
 where
  unsafeFromBase16 =
    either (error . show) id . Base16.decode . T.encodeUtf8
