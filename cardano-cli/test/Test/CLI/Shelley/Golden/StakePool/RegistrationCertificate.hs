{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.StakePool.RegistrationCertificate
  ( golden_shelleyStakePoolRegistrationCertificate
  ) where

import           Cardano.Prelude

import           Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakePoolRegistrationCertificate :: Property
golden_shelleyStakePoolRegistrationCertificate = OP.propertyOnce $ do
  OP.moduleWorkspace "tmp" $ \tempDir -> do
    operatorVerificationKeyFile <- OP.noteInputFile "test/Test/golden/shelley/node-pool/operator.vkey"
    vrfVerificationKeyFile <- OP.noteInputFile "test/Test/golden/shelley/node-pool/vrf.vkey"
    ownerVerificationKeyFile <- OP.noteInputFile "test/Test/golden/shelley/node-pool/owner.vkey"
    registrationCertFile <- OP.noteTempFile tempDir "registration.cert"

    void $ OP.execCardanoCLI
        [ "shelley","stake-pool","registration-certificate"
        , "--testnet-magic", "42"
        , "--pool-pledge", "0"
        , "--pool-cost", "0"
        , "--pool-margin", "0"
        , "--cold-verification-key-file", operatorVerificationKeyFile
        , "--vrf-verification-key-file", vrfVerificationKeyFile
        , "--reward-account-verification-key-file", ownerVerificationKeyFile
        , "--pool-owner-stake-verification-key-file", ownerVerificationKeyFile
        , "--out-file", registrationCertFile
        ]

    OP.assertFileOccurences 1 "Stake Pool Registration Certificate" registrationCertFile

    OP.assertEndsWithSingleNewline registrationCertFile
