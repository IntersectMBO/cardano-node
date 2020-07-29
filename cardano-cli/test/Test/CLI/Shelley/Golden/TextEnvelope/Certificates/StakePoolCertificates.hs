{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Certificates.StakePoolCertificates
  ( golden_shelleyStakePoolCertificates
  ) where

import           Cardano.Api.Typed (AsType (..), HasTextEnvelope (..))
import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Test.OptParse as OP

-- | 1. Create cold key pair.
--   2. Create stake key pair.
--   3. Create VRF key pair.
--   4. Create stake pool registration certificate.
--   5. Create stake pool deregistration/retirement certificate.
--   6. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakePoolCertificates :: Property
golden_shelleyStakePoolCertificates = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference files
  referenceRegistrationCertificate <- OP.noteInputFile "test/Test/golden/shelley/certificates/stake_pool_registration_certificate"
  referenceDeregistrationCertificate <- OP.noteInputFile "test/Test/golden/shelley/certificates/stake_pool_deregistration_certificate"

  -- Key filepaths
  coldVerKey <- OP.noteTempFile tempDir "cold-verification-key-file"
  coldSignKey <- OP.noteTempFile tempDir "cold-signing-key-file"
  operationalCertCounter <- OP.noteTempFile tempDir "operational-certificate-counter-file"
  vrfVerKey <- OP.noteTempFile tempDir "vrf-verification-key-file"
  vrfSignKey <- OP.noteTempFile tempDir "vrf-signing-key-file"
  poolRewardAccountAndOwnerVerKey <- OP.noteTempFile tempDir "reward-account-verification-key-file"
  poolRewardAccountSignKey <- OP.noteTempFile tempDir "reward-account-signing-key-file"
  registrationCertificate <- OP.noteTempFile tempDir "stake-pool-registration-certificate"
  deregistrationCertificate <- OP.noteTempFile tempDir "stake-pool-deregistration-certificate"

  -- Create cold key pair
  void $ OP.execCardanoCLI
    [ "shelley","node","key-gen"
    , "--cold-verification-key-file", coldVerKey
    , "--cold-signing-key-file", coldSignKey
    , "--operational-certificate-issue-counter", operationalCertCounter
    ]

  OP.assertFilesExist [coldSignKey, coldVerKey, operationalCertCounter]

  -- Generate stake key pair
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", poolRewardAccountAndOwnerVerKey
    , "--signing-key-file", poolRewardAccountSignKey
    ]

  OP.assertFilesExist [poolRewardAccountAndOwnerVerKey, poolRewardAccountSignKey]

  -- Generate vrf verification key
  void $ OP.execCardanoCLI
    [ "shelley","node","key-gen-VRF"
    , "--verification-key-file", vrfVerKey
    , "--signing-key-file", vrfSignKey
    ]


  OP.assertFilesExist [vrfSignKey, vrfVerKey]

  -- Create stake pool registration certificate
  void $ OP.execCardanoCLI
    [ "shelley","stake-pool","registration-certificate"
    , "--cold-verification-key-file", coldVerKey
    , "--vrf-verification-key-file", vrfVerKey
    , "--mainnet"
    , "--pool-cost", "1000"
    , "--pool-pledge", "5000"
    , "--pool-margin", "0.1"
    , "--pool-reward-account-verification-key-file", poolRewardAccountAndOwnerVerKey
    , "--pool-owner-stake-verification-key-file", poolRewardAccountAndOwnerVerKey
    , "--out-file", registrationCertificate
    ]

  OP.assertFilesExist [registrationCertificate]

  let registrationCertificateType = textEnvelopeType AsCertificate

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat registrationCertificateType referenceRegistrationCertificate registrationCertificate

  -- Create stake pool deregistration certificate
  void $ OP.execCardanoCLI
    [ "shelley", "stake-pool", "deregistration-certificate"
    , "--cold-verification-key-file", coldVerKey
    , "--epoch", "42"
    , "--out-file", deregistrationCertificate
    ]

  OP.assertFilesExist [deregistrationCertificate]

  -- Check the newly created files have not deviated from the
  -- golden files
  OP.checkTextEnvelopeFormat registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate
