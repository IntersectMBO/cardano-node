{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Certificates.MIRCertificate
  ( golden_shelleyMIRCertificate
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

-- | 1. Generate stake key pair
--   2. Create MIR certificate
--   s. Check the TextEnvelope serialization format has not changed.
golden_shelleyMIRCertificate :: Property
golden_shelleyMIRCertificate =
  propertyOnce $ do
    -- Reference keys
    let referenceMIRCertificate = "test/Test/golden/shelley/certificates/mir_certificate"

    -- Key filepaths
    let verKey = "stake-verification-key-file"
        signKey = "stake-signing-key-file"
        mirCertificate = "mir-certificate-file"
        createdFiles = [verKey, signKey, mirCertificate]

    -- Generate stake key pair
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","key-gen"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    assertFilesExist [verKey, signKey]

    -- Create MIR certificate
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","governance","create-mir-certificate"
                               , "--reserves" --TODO: Should also do "--reserves"
                               , "--stake-verification-key-file", verKey
                               , "--reward", "1000"
                               , "--out-file", mirCertificate
                               ]

    assertFilesExist [mirCertificate]

    let registrationCertificateType = textEnvelopeType AsCertificate

    checkTextEnvelopeFormat createdFiles registrationCertificateType referenceMIRCertificate mirCertificate

    liftIO $ fileCleanup createdFiles
    H.success
