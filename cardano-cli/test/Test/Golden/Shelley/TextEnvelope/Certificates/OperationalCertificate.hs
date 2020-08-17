{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.OperationalCertificate
  ( golden_shelleyOperationalCertificate
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

-- | 1. Create KES key pair.
--   2. Create cold keys.
--   3. Create operational certificate.
--   4. Check the TextEnvelope serialization format has not changed.
golden_shelleyOperationalCertificate :: Property
golden_shelleyOperationalCertificate = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceOperationalCertificate <- noteInputFile "test/data/golden/shelley/certificates/operational_certificate"

  -- Key filepaths
  kesVerKey <- noteTempFile tempDir "KES-verification-key-file"
  kesSignKey <- noteTempFile tempDir "KES-signing-key-file"
  coldVerKey <- noteTempFile tempDir "cold-verification-key-file"
  coldSignKey <- noteTempFile tempDir "cold-signing-key-file"
  operationalCertCounter <- noteTempFile tempDir "operational-certificate-counter-file"
  operationalCert <- noteTempFile tempDir "operational-certificate-file"

  -- Create KES key pair
  void $ execCardanoCLI
    [ "shelley","node","key-gen-KES"
    , "--verification-key-file", kesVerKey
    , "--signing-key-file", kesSignKey
    ]

  assertFilesExist [kesSignKey, kesVerKey]

  -- Create cold key pair
  void $ execCardanoCLI
    [ "shelley","node","key-gen"
    , "--cold-verification-key-file", coldVerKey
    , "--cold-signing-key-file", coldSignKey
    , "--operational-certificate-issue-counter", operationalCertCounter
    ]

  assertFilesExist [coldVerKey, coldSignKey, operationalCertCounter]

  -- Create operational certificate
  void $ execCardanoCLI
    [ "shelley","node","issue-op-cert"
    , "--kes-verification-key-file", kesVerKey
    , "--cold-signing-key-file", coldSignKey
    , "--operational-certificate-issue-counter", operationalCertCounter
    , "--kes-period", "1000"
    , "--out-file", operationalCert
    ]

  let operationalCertificateType = textEnvelopeType AsOperationalCertificate

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat operationalCertificateType referenceOperationalCertificate operationalCert
