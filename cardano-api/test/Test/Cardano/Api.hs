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


prop_genByronKeyPair_unique :: Property
prop_genByronKeyPair_unique =
  H.property $ do
    -- Basic sanity test that two distinct calls to the real 'genByronKeyPair'
    -- produces two distinct KeyPairs.
    kp1 <- liftIO genByronKeyPair
    kp2 <- liftIO genByronKeyPair
    kp1 /== kp2

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
