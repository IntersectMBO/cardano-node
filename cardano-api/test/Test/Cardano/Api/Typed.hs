{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed
  ( tests
  ) where

import           Cardano.Api.Typed

import           Cardano.Prelude

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Cardano.Crypto.Hash
import           Cardano.Crypto.KES

import           Test.Cardano.Api.Gen (genSeed)
import           Test.Cardano.Api.Orphans ()


prop_roundtrip_ByronVerificationKey_envelope :: Property
prop_roundtrip_ByronVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsByronKey

prop_roundtrip_ByronSigningKey_envelope :: Property
prop_roundtrip_ByronSigningKey_envelope =
  roundtrip_SigningKey_envelope AsByronKey


prop_roundtrip_PaymentVerificationKey_envelope :: Property
prop_roundtrip_PaymentVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsPaymentKey

prop_roundtrip_PaymentSigningKey_envelope :: Property
prop_roundtrip_PaymentSigningKey_envelope =
  roundtrip_SigningKey_envelope AsPaymentKey


prop_roundtrip_StakeVerificationKey_envelope :: Property
prop_roundtrip_StakeVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsStakeKey

prop_roundtrip_StakeSigningKey_envelope :: Property
prop_roundtrip_StakeSigningKey_envelope =
  roundtrip_SigningKey_envelope AsStakeKey


prop_roundtrip_StakePoolVerificationKey_envelope :: Property
prop_roundtrip_StakePoolVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsStakePoolKey

prop_roundtrip_StakePoolSigningKey_envelope :: Property
prop_roundtrip_StakePoolSigningKey_envelope =
  roundtrip_SigningKey_envelope AsStakePoolKey


prop_roundtrip_GenesisVerificationKey_envelope :: Property
prop_roundtrip_GenesisVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsGenesisKey

prop_roundtrip_GenesisSigningKey_envelope :: Property
prop_roundtrip_GenesisSigningKey_envelope =
  roundtrip_SigningKey_envelope AsGenesisKey


prop_roundtrip_GenesisDelegateVerificationKey_envelope :: Property
prop_roundtrip_GenesisDelegateVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsGenesisDelegateKey

prop_roundtrip_GenesisDelegateSigningKey_envelope :: Property
prop_roundtrip_GenesisDelegateSigningKey_envelope =
  roundtrip_SigningKey_envelope AsGenesisDelegateKey


prop_roundtrip_KesVerificationKey_envelope :: Property
prop_roundtrip_KesVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsKesKey

prop_roundtrip_KesSigningKey_envelope :: Property
prop_roundtrip_KesSigningKey_envelope =
  roundtrip_SigningKey_envelope AsKesKey


prop_roundtrip_VrfVerificationKey_envelope :: Property
prop_roundtrip_VrfVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsVrfKey

prop_roundtrip_VrfSigningKey_envelope :: Property
prop_roundtrip_VrfSigningKey_envelope =
  roundtrip_SigningKey_envelope AsVrfKey

-- -----------------------------------------------------------------------------

roundtrip_VerificationKey_envelope :: (Key keyrole,
                                       Show (VerificationKey keyrole),
                                       Eq (VerificationKey keyrole))
                                   => AsType keyrole -> Property
roundtrip_VerificationKey_envelope roletoken =
  H.property $ do
    vkey <- H.forAll (genVerificationKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsVerificationKey roletoken))

roundtrip_SigningKey_envelope :: (Key keyrole,
                                  Show (SigningKey keyrole),
                                  Eq (SigningKey keyrole))
                              => AsType keyrole -> Property
roundtrip_SigningKey_envelope roletoken =
  H.property $ do
    vkey <- H.forAll (genSigningKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsSigningKey roletoken))


-- -----------------------------------------------------------------------------

genSigningKey :: Key keyrole => AsType keyrole -> H.Gen (SigningKey keyrole)
genSigningKey roletoken = do
  seed <- genSeed (fromIntegral seedSize)
  let sk = deterministicSigningKey roletoken seed
  return sk
  where
    seedSize :: Word
    seedSize = deterministicSigningKeySeedSize roletoken

genVerificationKey :: Key keyrole
                   => AsType keyrole -> H.Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

--TODO: move this to the Orphans module once we expand and split up the typed
-- API tests.

deriving instance Eq (SigningKey ByronKey)
deriving instance Eq (SigningKey PaymentKey)
deriving instance Eq (SigningKey StakeKey)
deriving instance Eq (SigningKey StakePoolKey)
deriving instance Eq (SigningKey GenesisKey)
deriving instance Eq (SigningKey GenesisDelegateKey)
deriving instance Eq (SigningKey KesKey)
deriving instance Eq (SigningKey VrfKey)


instance (HashAlgorithm h, KESAlgorithm d) => Eq (SignKeyKES (SumKES h d)) where
  k1 == k2 = rawSerialiseSignKeyKES k1 == rawSerialiseSignKeyKES k2




tests :: IO Bool
tests =
  H.checkParallel $$discover
