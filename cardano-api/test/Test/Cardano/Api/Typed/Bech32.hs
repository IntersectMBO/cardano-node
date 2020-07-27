{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Api.Typed.Bech32
  ( tests,
  )
where

import Cardano.Api.Typed
import Cardano.Prelude
import Hedgehog (Gen, Property, discover)
import qualified Hedgehog as H
import Test.Cardano.Api.Typed.Gen

prop_roundtrip_Address_Shelley :: Property
prop_roundtrip_Address_Shelley = roundtrip_Bech32 AsShelleyAddress genAddressShelley

prop_roundtrip_StakeAddress :: Property
prop_roundtrip_StakeAddress = roundtrip_Bech32 AsStakeAddress genStakeAddress

-- -----------------------------------------------------------------------------

roundtrip_Bech32 ::
  (SerialiseAsBech32 a, Eq a, Show a) =>
  AsType a ->
  Gen a ->
  Property
roundtrip_Bech32 typeProxy gen =
  H.property $ do
    val <- H.forAll gen
    H.tripping val serialiseToBech32 (deserialiseFromBech32 typeProxy)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
