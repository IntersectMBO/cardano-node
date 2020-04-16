{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Config
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog

import           Test.Cardano.Config.Gen
import           Test.Cardano.Prelude (roundTripsAesonShow)


prop_wibble :: Property
prop_wibble =
  Hedgehog.property $ do
    sg <- Hedgehog.forAll genShelleyGenesis
    roundTripsAesonShow sg

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
