{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property, (/==), discover)
import qualified Hedgehog as H

import           Cardano.Api

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()

prop_byronGenKeyPair_unique :: Property
prop_byronGenKeyPair_unique =
  H.property $ do
    kp1 <- liftIO byronGenKeyPair
    kp2 <- liftIO byronGenKeyPair
    kp1 /== kp2

prop_shelleyGenKeyPair_unique :: Property
prop_shelleyGenKeyPair_unique =
  H.property $ do
    kp1 <- H.forAll genRegularKeyPairShelley
    kp2 <- H.forAll genRegularKeyPairShelley
    kp1 /== kp2

prop_shelleyGenesisGenKeyPair_unique :: Property
prop_shelleyGenesisGenKeyPair_unique =
  H.property $ do
    kp1 <- H.forAll genGenesisKeyPairShelley
    kp2 <- H.forAll genGenesisKeyPairShelley
    kp1 /== kp2

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
