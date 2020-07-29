{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Certificates.MIRCertificate
  ( golden_shelleyMIRCertificate
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Generate stake key pair
--   2. Create MIR certificate
--   s. Check the TextEnvelope serialization format has not changed.
golden_shelleyMIRCertificate :: Property
golden_shelleyMIRCertificate = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceMIRCertificate <- OP.noteInputFile "test/Test/golden/shelley/certificates/mir_certificate"

  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "stake-verification-key-file"
  signKey <- OP.noteTempFile tempDir "stake-signing-key-file"
  mirCertificate <- OP.noteTempFile tempDir "mir-certificate-file"

  -- Generate stake key pair
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  OP.assertFilesExist [verKey, signKey]

  -- Create MIR certificate
  void $ OP.execCardanoCLI
    [ "shelley","governance","create-mir-certificate"
    , "--reserves" --TODO: Should also do "--reserves"
    , "--stake-verification-key-file", verKey
    , "--reward", "1000"
    , "--out-file", mirCertificate
    ]

  OP.assertFilesExist [mirCertificate]

  let registrationCertificateType = textEnvelopeType AsCertificate

  OP.checkTextEnvelopeFormat registrationCertificateType referenceMIRCertificate mirCertificate
