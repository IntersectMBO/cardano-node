{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.Typed.Envelope
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Prelude
import           Gen.Cardano.Api.Typed
import           Hedgehog (Property)
import           Test.Cardano.Api.Typed.Orphans ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

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

roundtrip_VerificationKey_envelope :: Key keyrole
                                   => AsType keyrole -> Property
roundtrip_VerificationKey_envelope roletoken =
  H.property $ do
    vkey <- H.forAll (genVerificationKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsVerificationKey roletoken))

roundtrip_SigningKey_envelope :: (Key keyrole,
                                  Eq (SigningKey keyrole),
                                  Show (SigningKey keyrole))
                              => AsType keyrole -> Property
roundtrip_SigningKey_envelope roletoken =
  H.property $ do
    vkey <- H.forAll (genSigningKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsSigningKey roletoken))

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.Envelope"
  [ testPropertyNamed "roundtrip ByronVerificationKey envelope"           "roundtrip ByronVerificationKey envelope"           prop_roundtrip_ByronVerificationKey_envelope
  , testPropertyNamed "roundtrip ByronSigningKey envelope"                "roundtrip ByronSigningKey envelope"                prop_roundtrip_ByronSigningKey_envelope
  , testPropertyNamed "roundtrip PaymentVerificationKey envelope"         "roundtrip PaymentVerificationKey envelope"         prop_roundtrip_PaymentVerificationKey_envelope
  , testPropertyNamed "roundtrip PaymentSigningKey envelope"              "roundtrip PaymentSigningKey envelope"              prop_roundtrip_PaymentSigningKey_envelope
  , testPropertyNamed "roundtrip StakeVerificationKey envelope"           "roundtrip StakeVerificationKey envelope"           prop_roundtrip_StakeVerificationKey_envelope
  , testPropertyNamed "roundtrip StakeSigningKey envelope"                "roundtrip StakeSigningKey envelope"                prop_roundtrip_StakeSigningKey_envelope
  , testPropertyNamed "roundtrip StakePoolVerificationKey envelope"       "roundtrip StakePoolVerificationKey envelope"       prop_roundtrip_StakePoolVerificationKey_envelope
  , testPropertyNamed "roundtrip StakePoolSigningKey envelope"            "roundtrip StakePoolSigningKey envelope"            prop_roundtrip_StakePoolSigningKey_envelope
  , testPropertyNamed "roundtrip GenesisVerificationKey envelope"         "roundtrip GenesisVerificationKey envelope"         prop_roundtrip_GenesisVerificationKey_envelope
  , testPropertyNamed "roundtrip GenesisSigningKey envelope"              "roundtrip GenesisSigningKey envelope"              prop_roundtrip_GenesisSigningKey_envelope
  , testPropertyNamed "roundtrip GenesisDelegateVerificationKey envelope" "roundtrip GenesisDelegateVerificationKey envelope" prop_roundtrip_GenesisDelegateVerificationKey_envelope
  , testPropertyNamed "roundtrip GenesisDelegateSigningKey envelope"      "roundtrip GenesisDelegateSigningKey envelope"      prop_roundtrip_GenesisDelegateSigningKey_envelope
  , testPropertyNamed "roundtrip KesVerificationKey envelope"             "roundtrip KesVerificationKey envelope"             prop_roundtrip_KesVerificationKey_envelope
  , testPropertyNamed "roundtrip KesSigningKey envelope"                  "roundtrip KesSigningKey envelope"                  prop_roundtrip_KesSigningKey_envelope
  , testPropertyNamed "roundtrip VrfVerificationKey envelope"             "roundtrip VrfVerificationKey envelope"             prop_roundtrip_VrfVerificationKey_envelope
  , testPropertyNamed "roundtrip VrfSigningKey envelope"                  "roundtrip VrfSigningKey envelope"                  prop_roundtrip_VrfSigningKey_envelope
  ]
