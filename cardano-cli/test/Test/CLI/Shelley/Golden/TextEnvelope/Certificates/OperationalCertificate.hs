{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Certificates.OperationalCertificate
  ( golden_shelleyOperationalCertificate
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

-- | 1. Create KES key pair.
--   2. Create cold keys.
--   3. Create operational certificate.
--   4. Check the TextEnvelope serialization format has not changed.
golden_shelleyOperationalCertificate :: Property
golden_shelleyOperationalCertificate =
  propertyOnce $ do

    -- Reference keys
    let referenceOperationalCertificate = "test/Test/golden/shelley/certificates/operational_certificate"

    -- Key filepaths
    let kesVerKey = "KES-verification-key-file"
        kesSignKey = "KES-signing-key-file"
        coldVerKey = "cold-verification-key-file"
        coldSignKey = "cold-signing-key-file"
        operationalCertCounter = "operational-certificate-counter-file"
        operationalCert = "operational-certificate-file"
        createdFiles = [kesVerKey, kesSignKey, coldVerKey, coldSignKey, operationalCertCounter, operationalCert]

    -- Create KES key pair
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","node","key-gen-KES"
                               , "--verification-key-file", kesVerKey
                               , "--signing-key-file", kesSignKey
                               ]

    assertFilesExist [kesSignKey, kesVerKey]

    -- Create cold key pair
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","node","key-gen"
                               , "--cold-verification-key-file", coldVerKey
                               , "--cold-signing-key-file", coldSignKey
                               , "--operational-certificate-issue-counter", operationalCertCounter
                               ]

    assertFilesExist [coldVerKey, coldSignKey, operationalCertCounter]

    -- Create operational certificate
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","node","issue-op-cert"
                               , "--kes-verification-key-file", kesVerKey
                               , "--cold-signing-key-file", coldSignKey
                               , "--operational-certificate-issue-counter", operationalCertCounter
                               , "--kes-period", "1000"
                               , "--out-file", operationalCert
                               ]

    assertFilesExist createdFiles
    let operationalCertificateType = textEnvelopeType AsOperationalCertificate

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles operationalCertificateType referenceOperationalCertificate operationalCert

    liftIO $ fileCleanup createdFiles
    H.success
