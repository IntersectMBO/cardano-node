{-# LANGUAGE OverloadedStrings #-}

module Test.Pioneers.Exercise4
  ( tests
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)

import qualified Hedgehog as H
import qualified Test.OptParse as OP

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
prop_createStakeAddressRegistrationCertificate :: Property
prop_createStakeAddressRegistrationCertificate = OP.propertyOnce . OP.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  verKey <- OP.noteTempFile tempDir "stake-verification-key-file"
  signKey <- OP.noteTempFile tempDir "stake-signing-key-file"
  stakeRegCert <- OP.noteTempFile tempDir "stake-registration-certificate-file"

  -- Generate stake verification key
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]
  OP.assertFilesExist [verKey, signKey]

  -- Create stake address registration certificate
  void $ OP.execCardanoCLI
    [ "shelley","stake-address","registration-certificate"
    , "--stake-verification-key-file", verKey
    , "--out-file", stakeRegCert
    ]

  OP.assertFilesExist [verKey, signKey, stakeRegCert]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 4"
        [ ("prop_createStakeAddressRegistrationCertificate", prop_createStakeAddressRegistrationCertificate)
        ]
