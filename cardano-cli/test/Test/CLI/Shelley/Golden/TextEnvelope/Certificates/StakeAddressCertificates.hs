{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Certificates.StakeAddressCertificates
  ( golden_shelleyStakeAddressCertificates
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakeAddressCertificates :: Property
golden_shelleyStakeAddressCertificates = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference files
  referenceRegistrationCertificate <- OP.noteInputFile "test/Test/golden/shelley/certificates/stake_address_registration_certificate"
  referenceDeregistrationCertificate <- OP.noteInputFile "test/Test/golden/shelley/certificates/stake_address_deregistration_certificate"

  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "stake-verification-key-file"
  signKey <- OP.noteTempFile tempDir "stake-signing-key-file"
  deregistrationCertificate <- OP.noteTempFile tempDir "stake-address-deregistration-certificate"
  registrationCertificate <- OP.noteTempFile tempDir "stake-address-registration-certificate"

  -- Generate stake verification key
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  OP.assertFilesExist [verKey, signKey]

  -- Create stake address registration certificate
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","registration-certificate"
    , "--stake-verification-key-file", verKey
    , "--out-file", registrationCertificate
    ]

  let registrationCertificateType = textEnvelopeType AsCertificate

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat registrationCertificateType referenceRegistrationCertificate registrationCertificate

  -- Create stake address deregistration certificate
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","deregistration-certificate"
    , "--stake-verification-key-file", verKey
    , "--out-file", deregistrationCertificate
    ]

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate

-- TODO: After delegation-certificate command is fixed to take a hash instead of a verfication key
{-
  -- Create stake address delegation certificate
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","delegation-certificate"
    , "--stake-verification-key-file", verKey
    , "--cold-verification-key-file", verKey --TODO: Should be stake pool's hash
    , "--out-file", deregistrationCertificate
    ]

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate
-}
