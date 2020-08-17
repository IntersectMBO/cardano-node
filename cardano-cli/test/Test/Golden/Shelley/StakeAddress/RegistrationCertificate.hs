{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley.StakeAddress.RegistrationCertificate
  ( golden_shelleyStakeAddressRegistrationCertificate
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressRegistrationCertificate :: Property
golden_shelleyStakeAddressRegistrationCertificate = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  keyGenStakingVerificationKeyFile <- noteInputFile "test/data/golden/shelley/keys/stake_keys/verification_key"
  registrationCertFile <- noteTempFile tempDir "registration.cert"

  void $ execCardanoCLI
    [ "shelley","stake-address","registration-certificate"
    , "--staking-verification-key-file", keyGenStakingVerificationKeyFile
    , "--out-file", registrationCertFile
    ]

  assertFileOccurences 1 "Stake Address Registration Certificate" registrationCertFile

  assertEndsWithSingleNewline registrationCertFile
