{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Test.Cardano.Api.Orphans ()

import           Hedgehog (Property, (/==), discover)
import qualified Hedgehog as Hedgehog

import           Test.Cardano.Api.Gen

prop_byronGenKeyPair_unique :: Property
prop_byronGenKeyPair_unique =
  Hedgehog.property $ do
    -- Basic sanity test that two distinct calls to the real 'genByronKeyPair'
    -- produces two distinct KeyPairs.
    kp1 <- liftIO byronGenKeyPair
    kp2 <- liftIO byronGenKeyPair
    kp1 /== kp2

-- | Basic sanity test that two distinct calls to the real 'shelleyGenKeyPair'
-- produces two distinct 'KeyPair's.
prop_shelleyGenKeyPair_unique :: Property
prop_shelleyGenKeyPair_unique =
  Hedgehog.property $ do
    kp1 <- liftIO shelleyGenKeyPair
    kp2 <- liftIO shelleyGenKeyPair
    kp1 /== kp2

prop_roundtrip_AddressByron_hex :: Property
prop_roundtrip_AddressByron_hex = do
  Hedgehog.property $ do
    addr <- Hedgehog.forAll genByronVerificationKeyAddress
    Hedgehog.tripping addr addressToHex addressFromHex

prop_roundtrip_AddressShelley_hex :: Property
prop_roundtrip_AddressShelley_hex = do
  Hedgehog.property $ do
    addr <- Hedgehog.forAll genShelleyVerificationKeyAddress
    Hedgehog.tripping addr addressToHex addressFromHex

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
