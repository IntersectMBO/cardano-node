{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Certificates.StakeAddressCertificates
  ( golden_shelleyStakeAddressCertificates
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
--   3. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakeAddressCertificates :: Property
golden_shelleyStakeAddressCertificates =
  propertyOnce $ do

    -- Reference files
    let referenceRegistrationCertificate = "test/Test/golden/shelley/certificates/stake_address_registration_certificate"
        referenceDeregistrationCertificate = "test/Test/golden/shelley/certificates/stake_address_deregistration_certificate"

    -- Key filepaths
    let verKey = "stake-verification-key-file"
        signKey = "stake-signing-key-file"
        deregistrationCertificate = "stake-address-deregistration-certificate"
        registrationCertificate = "stake-address-registration-certificate"
        createdFiles = [verKey, signKey, deregistrationCertificate, registrationCertificate]

    -- Generate stake verification key
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","key-gen"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]
    assertFilesExist [verKey, signKey]

    -- Create stake address registration certificate
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","registration-certificate"
                               , "--stake-verification-key-file", verKey
                               , "--out-file", registrationCertificate
                               ]

    let registrationCertificateType = textEnvelopeType AsCertificate

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles registrationCertificateType referenceRegistrationCertificate registrationCertificate


    -- Create stake address deregistration certificate
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","deregistration-certificate"
                               , "--stake-verification-key-file", verKey
                               , "--out-file", deregistrationCertificate
                               ]

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate

 -- TODO: After delegation-certificate command is fixed to take a hash instead of a verfication key
 {-
    -- Create stake address delegation certificate
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","delegation-certificate"
                               , "--stake-verification-key-file", verKey
                               , "--cold-verification-key-file", verKey --TODO: Should be stake pool's hash
                               , "--out-file", deregistrationCertificate
                               ]

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate

-}

    liftIO $ fileCleanup createdFiles
    H.success
