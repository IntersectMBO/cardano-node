{-# LANGUAGE OverloadedStrings #-}

module Test.Pioneers.Exercise4
  ( tests
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse

-- | 1. Generate a stake verification key
--   2. Create a stake address registration certificate
prop_createStakeAddressRegistrationCertificate :: Property
prop_createStakeAddressRegistrationCertificate =
  propertyOnce $ do

    -- Key filepaths
    let verKey = "stake-verification-key-file"
        signKey = "stake-signing-key-file"
        stakeRegCert = "stake-registration-certificate-file"
        allFiles = [verKey, signKey, stakeRegCert]

    -- Generate stake verification key
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","key-gen"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]
    assertFilesExist [verKey, signKey]

    -- Create stake address registration certificate
    execCardanoCLIParser
      allFiles
        $ evalCardanoCLIParser [ "shelley","stake-address","registration-certificate"
                               , "--stake-verification-key-file", verKey
                               , "--out-file", stakeRegCert
                               ]

    assertFilesExist allFiles
    liftIO $ fileCleanup allFiles
    H.success

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 4"
        [ ("prop_createStakeAddressRegistrationCertificate", prop_createStakeAddressRegistrationCertificate)
        ]
