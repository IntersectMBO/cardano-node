{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api
  ( tests
  ) where

import           Cardano.Api

import           Cardano.Prelude

import           Hedgehog (Property, (/==), discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Orphans ()


prop_byronGenKeyPair_unique :: Property
prop_byronGenKeyPair_unique =
  H.property $ do
    -- Basic sanity test that two distinct calls to the real 'genByronKeyPair'
    -- produces two distinct KeyPairs.
    kp1 <- liftIO byronGenKeyPair
    kp2 <- liftIO byronGenKeyPair
    kp1 /== kp2

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
