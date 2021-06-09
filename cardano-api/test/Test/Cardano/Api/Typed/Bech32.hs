{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.Typed.Bech32
  ( tests
  ) where

import           Cardano.Api
import           Gen.Cardano.Api.Typed
import           Gen.Hedgehog.Roundtrip.Bech32 (roundtrip_Bech32)
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover)
import           Test.Tasty (TestTree)

prop_roundtrip_Address_Shelley :: Property
prop_roundtrip_Address_Shelley = roundtrip_Bech32 AsShelleyAddress genAddressShelley

prop_roundtrip_StakeAddress :: Property
prop_roundtrip_StakeAddress = roundtrip_Bech32 AsStakeAddress genStakeAddress

tests :: TestTree
tests = fromGroup $$discover
