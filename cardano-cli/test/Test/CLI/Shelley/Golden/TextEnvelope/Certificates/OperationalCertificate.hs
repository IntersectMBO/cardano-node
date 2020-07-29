{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Certificates.OperationalCertificate
  ( golden_shelleyOperationalCertificate
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Create KES key pair.
--   2. Create cold keys.
--   3. Create operational certificate.
--   4. Check the TextEnvelope serialization format has not changed.
golden_shelleyOperationalCertificate :: Property
golden_shelleyOperationalCertificate = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceOperationalCertificate <- OP.noteInputFile "test/Test/golden/shelley/certificates/operational_certificate"

  -- Key filepaths
  kesVerKey <- OP.noteTempFile tempDir "KES-verification-key-file"
  kesSignKey <- OP.noteTempFile tempDir "KES-signing-key-file"
  coldVerKey <- OP.noteTempFile tempDir "cold-verification-key-file"
  coldSignKey <- OP.noteTempFile tempDir "cold-signing-key-file"
  operationalCertCounter <- OP.noteTempFile tempDir "operational-certificate-counter-file"
  operationalCert <- OP.noteTempFile tempDir "operational-certificate-file"

  -- Create KES key pair
  void $ OP.execCardanoCLI
    [ "shelley","node","key-gen-KES"
    , "--verification-key-file", kesVerKey
    , "--signing-key-file", kesSignKey
    ]

  OP.assertFilesExist [kesSignKey, kesVerKey]

  -- Create cold key pair
  void $ OP.execCardanoCLI
    [ "shelley","node","key-gen"
    , "--cold-verification-key-file", coldVerKey
    , "--cold-signing-key-file", coldSignKey
    , "--operational-certificate-issue-counter", operationalCertCounter
    ]

  OP.assertFilesExist [coldVerKey, coldSignKey, operationalCertCounter]

  -- Create operational certificate
  void $ OP.execCardanoCLI
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
  OP.checkTextEnvelopeFormat operationalCertificateType referenceOperationalCertificate operationalCert
