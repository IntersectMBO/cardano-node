{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Golden.StakeAddress.DeregistrationCertificate
  ( golden_shelleyStakeAddressDeregistrationCertificate
  ) where

import           Cardano.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_shelleyStakeAddressDeregistrationCertificate :: Property
golden_shelleyStakeAddressDeregistrationCertificate = propertyOnce . moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteInputFile "test/Test/golden/shelley/keys/stake_keys/verification_key"
  deregistrationCertFile <- noteTempFile tempDir "deregistrationCertFile"

  void $ execCardanoCLI
    [ "shelley","stake-address","deregistration-certificate"
    , "--staking-verification-key-file", verificationKeyFile
    , "--out-file", deregistrationCertFile
    ]

  assertFileOccurences 1 "Stake Address Deregistration Certificate" deregistrationCertFile

  assertEndsWithSingleNewline deregistrationCertFile
