{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Keys.GenesisDelegateKeys
  ( golden_shelleyGenesisDelegateKeys
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Generate a key pair & operational certificate counter file
--   2. Check for the existence of the key pair & counter file
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyGenesisDelegateKeys :: Property
golden_shelleyGenesisDelegateKeys = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- OP.noteInputFile "test/Test/golden/shelley/keys/genesis_delegate_keys/verification_key"
  referenceSignKey <- OP.noteInputFile "test/Test/golden/shelley/keys/genesis_delegate_keys/signing_key"
  referenceOpCertCounter <- OP.noteInputFile "test/Test/golden/shelley/keys/genesis_delegate_keys/operational_certificate_counter"

  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "genesis-delegate-verification-key-file"
  signKey <- OP.noteTempFile tempDir "genesis-delegate-signing-key-file"
  opCertCounter <- OP.noteTempFile tempDir "delegate-operational-cert-counter-file"

  -- Generate payment verification key
  void $ OP.execCardanoCLI
    [ "shelley","genesis","key-gen-delegate"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    , "--operational-certificate-issue-counter-file", opCertCounter
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisDelegateKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisDelegateKey)
      operationalCertCounterType = textEnvelopeType AsOperationalCertificateIssueCounter

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  OP.checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
  OP.checkTextEnvelopeFormat operationalCertCounterType referenceOpCertCounter opCertCounter
