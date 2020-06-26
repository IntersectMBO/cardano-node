{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.TextEnvelope.Golden.GenesisDelegateKeys
  ( golden_shelleyGenesisDelegateKeys
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. We generate a key pair & operational certificate counter file
--   2. We check for the existence of the key pair & counter file
--   3. We check the TextEnvelope serialization format has not changed.
golden_shelleyGenesisDelegateKeys :: Property
golden_shelleyGenesisDelegateKeys =
  propertyOnce $ do

    -- Reference keys
    let referenceVerKey = "test/Test/golden/shelley/genesis_delegate_keys/verification_key"
        rreferenceSignKey = "test/Test/golden/shelley/genesis_delegate_keys/signing_key"
        referenceOpCertCounter = "test/Test/golden/shelley/genesis_delegate_keys/operational_certificate_counter"

    -- Key filepaths
    let verKey = "genesis-delegate-verification-key-file"
        signKey = "genesis-delegate-signing-key-file"
        opCertCounter = "delegate-operational-cert-counter-file"
        createdFiles = [opCertCounter, verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
      "golden_shelleyGenesisDelegateKeys.genesis_delegate_keypair_gen"
        $ evalCardanoCLIParser [ "shelley","genesis","key-gen-delegate"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               , "--operational-certificate-issue-counter-file", opCertCounter
                               ]

    assertFilesExist createdFiles


    let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisDelegateKey)
        verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisDelegateKey)
        operationalCertCounterType = textEnvelopeType AsOperationalCertificateIssueCounter

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType referenceVerKey verKey
    checkTextEnvelopeFormat createdFiles signingKeyType rreferenceSignKey signKey
    checkTextEnvelopeFormat createdFiles operationalCertCounterType referenceOpCertCounter opCertCounter

    liftIO $ fileCleanup createdFiles
    H.success
