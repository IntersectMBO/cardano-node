{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.StakeAddress.RegistrationCertificate
  ( golden_shelleyStakeAddressRegistrationCertificate
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressRegistrationCertificate :: Property
golden_shelleyStakeAddressRegistrationCertificate = OP.propertyOnce $ do
  OP.workspace "tmp/stake-address-registration-certificate" $ \tempDir -> do
    keyGenStakingVerificationKeyFile <- OP.noteInputFile "test/Test/golden/shelley/keys/stake_keys/verification_key"
    registrationCertFile <- OP.noteTempFile tempDir "registration.cert"

    void $ OP.execCardanoCLI
        [ "shelley","stake-address","registration-certificate"
        , "--staking-verification-key-file", keyGenStakingVerificationKeyFile
        , "--out-file", registrationCertFile
        ]

    OP.assertFileOccurences 1 "Stake Address Registration Certificate" registrationCertFile
