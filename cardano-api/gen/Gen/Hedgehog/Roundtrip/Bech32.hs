module Gen.Hedgehog.Roundtrip.Bech32
  ( roundtrip_Bech32
  ) where

import           Cardano.Api
import           Cardano.Prelude
import           Hedgehog (Gen, Property)

import qualified Hedgehog as H

roundtrip_Bech32
  :: (SerialiseAsBech32 a, Eq a, Show a)
  => AsType a -> Gen a -> Property
roundtrip_Bech32 typeProxy gen =
  H.property $ do
    val <- H.forAll gen
    H.tripping val serialiseToBech32 (deserialiseFromBech32 typeProxy)
