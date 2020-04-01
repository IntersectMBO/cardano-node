{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.CBOR
  ( tests
  ) where

import           Cardano.Api

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Gen
import           Test.Cardano.Api.Orphans ()


prop_KeyPair_CBOR :: Property
prop_KeyPair_CBOR =
  H.property $ do
    kp <- H.forAll genKeyPair
    H.tripping kp keyPairToCBOR keyPairFromCBOR

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
