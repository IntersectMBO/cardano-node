{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.Byron.View
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()

prop_roundtrip_ByronAddress_view :: Property
prop_roundtrip_ByronAddress_view =
  H.property $ do
    addr <- H.forAll genByronAddress
    H.tripping addr renderByronAddressView parseByronAddressView

prop_roundtrip_ByronKeyPair_view :: Property
prop_roundtrip_ByronKeyPair_view =
  H.property $ do
    kp <- H.forAll genKeyPairByron
    H.tripping kp renderByronKeyPairView parseByronKeyPairView

prop_roundtrip_ByronPublicKey_view :: Property
prop_roundtrip_ByronPublicKey_view =
  H.property $ do
    pk <- H.forAll genPublicKeyByron
    H.tripping pk renderByronPublicKeyView parseByronPublicKeyView

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
