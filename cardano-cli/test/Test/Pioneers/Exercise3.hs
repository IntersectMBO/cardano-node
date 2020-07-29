{-# LANGUAGE OverloadedStrings #-}

module Test.Pioneers.Exercise3
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

-- | 1. Create KES key pair.
--   2. Create cold keys.
--   3. Create operational certificate.
--   4. Create VRF key pair.
prop_createOperationalCertificate :: Property
prop_createOperationalCertificate =
  propertyOnce $ do
    -- Key filepaths
    let kesVerKey = "KES-verification-key-file"
        kesSignKey = "KES-signing-key-file"
        coldVerKey = "cold-verification-key-file"
        coldSignKey = "cold-signing-key-file"
        operationalCertCounter = "operational-certificate-counter-file"
        operationalCert = "operational-certificate-file"
        allFiles = [kesVerKey, kesSignKey, coldVerKey, coldSignKey, operationalCertCounter, operationalCert]

    -- Create KES key pair
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","node","key-gen-KES"
                               , "--verification-key-file", kesVerKey
                               , "--signing-key-file", kesSignKey
                               ]

    assertFilesExist [kesSignKey, kesVerKey]

    -- Create cold key pair
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","node","key-gen"
                               , "--cold-verification-key-file", coldVerKey
                               , "--cold-signing-key-file", coldSignKey
                               , "--operational-certificate-issue-counter", operationalCertCounter
                               ]

    assertFilesExist [coldVerKey, coldSignKey, operationalCertCounter]

    -- Create operational certificate
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","node","issue-op-cert"
                               , "--kes-verification-key-file", kesVerKey
                               , "--cold-signing-key-file", coldSignKey
                               , "--operational-certificate-issue-counter", operationalCertCounter
                               , "--kes-period", "1000"
                               , "--out-file", operationalCert
                               ]

    assertFilesExist allFiles

    liftIO $ fileCleanup allFiles
    H.success

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 3"
        [ ("prop_createOperationalCertificate", prop_createOperationalCertificate)
        ]
