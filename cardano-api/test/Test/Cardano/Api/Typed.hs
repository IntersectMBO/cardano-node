{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed
  ( tests
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Test.Cardano.Api.Gen (genSeed)
import           Test.Cardano.Api.Orphans ()


prop_roundtrip_VrfVerificationKey_envelope :: Property
prop_roundtrip_VrfVerificationKey_envelope =
  H.property $ do
    vkey <- H.forAll (genVerificationKey AsVrfKey)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsVerificationKey AsVrfKey))

-- -----------------------------------------------------------------------------

genSigningKey :: Key keyrole => AsType keyrole -> H.Gen (SigningKey keyrole)
genSigningKey ttoken = do
  seed <- genSeed (fromIntegral seedSize)
  let sk = deterministicSigningKey ttoken seed
  return sk
  where
    seedSize :: Word
    seedSize = deterministicSigningKeySeedSize ttoken

genVerificationKey :: Key keyrole
                   => AsType keyrole -> H.Gen (VerificationKey keyrole)
genVerificationKey ttoken = getVerificationKey <$> genSigningKey ttoken

--TODO: move this to the Orphans module once we expand and split up the typed
-- API tests.
deriving instance Eq (VerificationKey VrfKey)


tests :: IO Bool
tests =
  H.checkParallel $$discover
