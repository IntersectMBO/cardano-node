{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise4
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
prop_createStakeAddressRegistrationCertificate :: Property
prop_createStakeAddressRegistrationCertificate = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"
  stakeRegCert <- noteTempFile tempDir "stake-registration-certificate-file"

  -- Generate stake verification key
  void $ execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]
  assertFilesExist [verKey, signKey]

  -- Create stake address registration certificate
  void $ execCardanoCLI
    [ "shelley","stake-address","registration-certificate"
    , "--stake-verification-key-file", verKey
    , "--out-file", stakeRegCert
    ]

  assertFilesExist [verKey, signKey, stakeRegCert]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 4"
        [ ("prop_createStakeAddressRegistrationCertificate", prop_createStakeAddressRegistrationCertificate)
        ]
