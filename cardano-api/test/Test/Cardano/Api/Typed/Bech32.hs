{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Fix deprecations

module Test.Cardano.Api.Typed.Bech32
  ( tests
  ) where

import           Hedgehog (Property)
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Cardano.Api
import           Gen.Cardano.Api.Typed
import           Gen.Hedgehog.Roundtrip.Bech32 (roundtrip_Bech32)

prop_roundtrip_Address_Shelley :: Property
prop_roundtrip_Address_Shelley = roundtrip_Bech32 AsShelleyAddress genAddressShelley

prop_roundtrip_StakeAddress :: Property
prop_roundtrip_StakeAddress = roundtrip_Bech32 AsStakeAddress genStakeAddress

tests :: TestTree
tests = $testGroupGenerator
