{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.TextEnvelope.Certificates.GenesisKeyDelegationCertificate
  ( golden_shelleyGenesisKeyDelegationCertificate
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_shelleyGenesisKeyDelegationCertificate :: Property
golden_shelleyGenesisKeyDelegationCertificate =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Reference certificate
    referenceCertificateFilePath <-
      noteInputFile $
        "test/data/golden/shelley/certificates/"
          <> "genesis_key_delegation_certificate"

    -- Verification key and certificate filepaths
    genesisVerKeyFilePath <-
      noteTempFile tempDir "genesis-verification-key-file"
    genesisDelegVerKeyFilePath <-
      noteTempFile tempDir "genesis-delegate-verification-key-file"
    vrfVerKeyFilePath <- noteTempFile tempDir "vrf-verification-key-file"
    genesisKeyDelegCertFilePath <-
      noteTempFile tempDir "genesis-key-delegation-certificate-file"

    -- Generate genesis key pair
    void $ execCardanoCLI
      [ "shelley","genesis","key-gen-genesis"
      , "--verification-key-file", genesisVerKeyFilePath
      , "--signing-key-file", "/dev/null"
      ]

    -- Generate genesis delegate key pair
    void $ execCardanoCLI
      [ "shelley","genesis","key-gen-delegate"
      , "--verification-key-file", genesisDelegVerKeyFilePath
      , "--signing-key-file", "/dev/null"
      , "--operational-certificate-issue-counter-file", "/dev/null"
      ]

    -- Generate VRF key pair
    void $ execCardanoCLI
      [ "shelley","node","key-gen-VRF"
      , "--verification-key-file", vrfVerKeyFilePath
      , "--signing-key-file", "/dev/null"
      ]

    H.assertFilesExist
      [ genesisVerKeyFilePath
      , genesisDelegVerKeyFilePath
      , vrfVerKeyFilePath
      ]

    -- Create genesis key delegation certificate
    void $ execCardanoCLI
      [ "shelley","governance","create-genesis-key-delegation-certificate"
      , "--genesis-verification-key-file", genesisVerKeyFilePath
      , "--genesis-delegate-verification-key-file", genesisDelegVerKeyFilePath
      , "--vrf-verification-key-file", vrfVerKeyFilePath
      , "--out-file", genesisKeyDelegCertFilePath
      ]

    H.assertFilesExist [genesisKeyDelegCertFilePath]

    let certificateType = textEnvelopeType AsCertificate

    checkTextEnvelopeFormat
      certificateType
      referenceCertificateFilePath
      genesisKeyDelegCertFilePath
