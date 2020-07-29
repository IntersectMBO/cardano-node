{-# LANGUAGE OverloadedStrings #-}

module Test.Pioneers.Exercise3
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Hedgehog as H
import qualified Test.OptParse as OP

-- | 1. Create KES key pair.
--   2. Create cold keys.
--   3. Create operational certificate.
--   4. Create VRF key pair.
prop_createOperationalCertificate :: Property
prop_createOperationalCertificate = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
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

  OP.assertFilesExist [kesVerKey, kesSignKey, coldVerKey, coldSignKey, operationalCertCounter, operationalCert]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 3"
        [ ("prop_createOperationalCertificate", prop_createOperationalCertificate)
        ]
