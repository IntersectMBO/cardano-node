{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.StakeAddress.DeregistrationCertificate
  ( golden_shelleyStakeAddressDeregistrationCertificate
  ) where

import Cardano.Prelude

import Hedgehog (Property)

import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressDeregistrationCertificate :: Property
golden_shelleyStakeAddressDeregistrationCertificate = OP.propertyOnce $ do
  OP.workspace "tmp/stake-address-deregistration-certificate" $ \tempDir -> do
    verificationKeyFile <- OP.noteInputFile "test/Test/golden/shelley/keys/stake_keys/verification_key"
    deregistrationCertFile <- OP.noteTempFile tempDir "deregistrationCertFile"

    void $ OP.execCardanoCLI
        [ "shelley","stake-address","deregistration-certificate"
        , "--staking-verification-key-file", verificationKeyFile
        , "--out-file", deregistrationCertFile
        ]

    OP.assertFileOccurences 1 "Stake Address Deregistration Certificate" deregistrationCertFile
