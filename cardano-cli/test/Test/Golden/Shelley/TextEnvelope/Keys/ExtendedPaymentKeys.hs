{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys
  ( golden_shelleyExtendedPaymentKeys
  , golden_shelleyExtendedPaymentKeys_bech32
  , golden_shelleyExtendedPaymentKeys_te
  ) where

import           Cardano.Api (AsType (..), HasTextEnvelope (..))

import           Control.Monad (void)
import           Hedgehog (Property)
import           Test.OptParse
import           Text.Regex.TDFA ((=~))

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyExtendedPaymentKeys :: Property
golden_shelleyExtendedPaymentKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/data/golden/shelley/keys/extended_payment_keys/verification_key"
  referenceSignKey <- noteInputFile "test/data/golden/shelley/keys/extended_payment_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "extended-payment-verification-key-file"
  signKey <- noteTempFile tempDir "extended-payment-signing-key-file"

  -- Generate payment verification key
  void $ execCardanoCLI
    [ "address","key-gen"
    , "--extended-key"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentExtendedKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentExtendedKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyExtendedPaymentKeys_te :: Property
golden_shelleyExtendedPaymentKeys_te = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/data/golden/shelley/keys/extended_payment_keys/verification_key"
  referenceSignKey <- noteInputFile "test/data/golden/shelley/keys/extended_payment_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "extended-payment-verification-key-file"
  signKey <- noteTempFile tempDir "extended-payment-signing-key-file"

  -- Generate payment verification key
  void $ execCardanoCLI
    [ "address","key-gen"
    , "--key-output-format", "text-envelope"
    , "--extended-key"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentExtendedKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentExtendedKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the bech32 serialization format has not changed.
golden_shelleyExtendedPaymentKeys_bech32 :: Property
golden_shelleyExtendedPaymentKeys_bech32 = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  H.note_ tempDir

  -- Key filepaths
  verKeyFile <- noteTempFile tempDir "payment-verification-key-file"
  signKeyFile <- noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $ execCardanoCLI
    [ "address","key-gen"
    , "--key-output-format", "bech32"
    , "--extended-key"
    , "--verification-key-file", verKeyFile
    , "--signing-key-file", signKeyFile
    ]

  verKey <- H.readFile verKeyFile
  H.assert $ verKey =~ id @String "^addr_xvk[a-z0-9]{110}$"

  signKey <- H.readFile signKeyFile
  H.assert $ signKey =~ id @String "^addr_xsk[a-z0-9]{212}$"
