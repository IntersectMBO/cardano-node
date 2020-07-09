{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.TextEnvelope.Certificates.StakePoolCertificates
  ( golden_shelleyStakePoolCertificates
  ) where

import           Cardano.Prelude

import           Cardano.Api.Typed (AsType(..), HasTextEnvelope (..))

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

-- | 1. Create cold key pair.
--   2. Create stake key pair.
--   3. Create VRF key pair.
--   4. Create stake pool registration certificate.
--   5. Create stake pool deregistration/retirement certificate.
--   6. Check the TextEnvelope serialization format has not changed.
golden_shelleyStakePoolCertificates :: Property
golden_shelleyStakePoolCertificates =
  propertyOnce $ do

    -- Reference files
    let referenceRegistrationCertificate = "test/Test/golden/shelley/certificates/stake_pool_registration_certificate"
        referenceDeregistrationCertificate = "test/Test/golden/shelley/certificates/stake_pool_deregistration_certificate"

    -- Key filepaths
    let coldVerKey = "cold-verification-key-file"
        coldSignKey = "cold-signing-key-file"
        operationalCertCounter = "operational-certificate-counter-file"
        vrfVerKey = "vrf-verification-key-file"
        vrfSignKey = "vrf-signing-key-file"
        poolRewardAccountAndOwnerVerKey = "reward-account-verification-key-file"
        poolRewardAccountSignKey = "reward-account-signing-key-file"
        registrationCertificate = "stake-pool-registration-certificate"
        deregistrationCertificate = "stake-pool-deregistration-certificate"
        createdFiles = [ coldVerKey
                       , coldSignKey
                       , operationalCertCounter
                       , vrfVerKey
                       , vrfSignKey
                       , poolRewardAccountAndOwnerVerKey
                       , poolRewardAccountSignKey
                       , registrationCertificate
                       , deregistrationCertificate
                       ]

    -- Create cold key pair
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","node","key-gen"
                               , "--cold-verification-key-file", coldVerKey
                               , "--cold-signing-key-file", coldSignKey
                               , "--operational-certificate-issue-counter", operationalCertCounter
                               ]

    assertFilesExist [coldSignKey, coldVerKey, operationalCertCounter]

    -- Generate stake key pair
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","key-gen"
                               , "--verification-key-file", poolRewardAccountAndOwnerVerKey
                               , "--signing-key-file", poolRewardAccountSignKey
                               ]

    assertFilesExist [poolRewardAccountAndOwnerVerKey, poolRewardAccountSignKey]

    -- Generate vrf verification key
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","node","key-gen-VRF"
                               , "--verification-key-file", vrfVerKey
                               , "--signing-key-file", vrfSignKey
                               ]


    assertFilesExist [vrfSignKey, vrfVerKey]

    -- Create stake pool registration certificate
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley","stake-pool","registration-certificate"
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


    assertFilesExist [registrationCertificate]

    let registrationCertificateType = textEnvelopeType AsCertificate

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles registrationCertificateType referenceRegistrationCertificate registrationCertificate


    -- Create stake pool deregistration certificate
    execCardanoCLIParser
      createdFiles
        $ evalCardanoCLIParser [ "shelley", "stake-pool", "deregistration-certificate"
                               , "--cold-verification-key-file", coldVerKey
                               , "--epoch", "42"
                               , "--out-file", deregistrationCertificate
                               ]

    assertFilesExist [deregistrationCertificate]

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles registrationCertificateType referenceDeregistrationCertificate deregistrationCertificate

    liftIO $ fileCleanup createdFiles
    H.success
