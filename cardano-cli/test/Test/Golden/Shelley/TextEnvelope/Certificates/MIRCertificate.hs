{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.MIRCertificate
  ( golden_shelleyMIRCertificate
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

-- | 1. Generate stake key pair
--   2. Create MIR certificate
--   s. Check the TextEnvelope serialization format has not changed.
golden_shelleyMIRCertificate :: Property
golden_shelleyMIRCertificate = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceMIRCertificate <- noteInputFile "test/data/golden/shelley/certificates/mir_certificate"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"
  mirCertificate <- noteTempFile tempDir "mir-certificate-file"

  -- Generate stake key pair
  void $ execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  assertFilesExist [verKey, signKey]

  -- Create MIR certificate
  void $ execCardanoCLI
    [ "shelley","governance","create-mir-certificate"
    , "--reserves" --TODO: Should also do "--reserves"
    , "--stake-verification-key-file", verKey
    , "--reward", "1000"
    , "--out-file", mirCertificate
    ]

  assertFilesExist [mirCertificate]

  let registrationCertificateType = textEnvelopeType AsCertificate

  checkTextEnvelopeFormat registrationCertificateType referenceMIRCertificate mirCertificate
