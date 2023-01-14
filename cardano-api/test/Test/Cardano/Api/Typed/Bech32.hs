module Test.Cardano.Api.Typed.Bech32
  ( tests
  ) where

import           Cardano.Api (AsType(AsStakeAddress, AsShelleyAddress))
import           Gen.Cardano.Api.Typed( genAddressShelley, genStakeAddress)
import           Gen.Hedgehog.Roundtrip.Bech32 (roundtrip_Bech32)
import           Hedgehog (Property)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

prop_roundtrip_Address_Shelley :: Property
prop_roundtrip_Address_Shelley = roundtrip_Bech32 AsShelleyAddress genAddressShelley

prop_roundtrip_StakeAddress :: Property
prop_roundtrip_StakeAddress = roundtrip_Bech32 AsStakeAddress genStakeAddress

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.Bech32"
  [ testPropertyNamed "roundtrip Address Shelley" "roundtrip Address Shelley" prop_roundtrip_Address_Shelley
  , testPropertyNamed "roundtrip StakeAddress"    "roundtrip StakeAddress"    prop_roundtrip_StakeAddress
  ]
